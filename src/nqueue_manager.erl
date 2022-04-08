%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(nqueue_manager).

-include("nqueue_manager.hrl").

%%% START/STOP EXPORTS
-export([start_link/4, stop/1]).

%%% INIT/TERMINATE EXPORTS
-export([init/5, terminate/1]).

%%% SPECIAL PROCESS EXPORTS
-export([
    system_continue/3,
    system_terminate/4,
    write_debug/3,
    system_get_state/1,
    system_replace_state/2
]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link(Name, ConsumerCount, ConsumerFun, Rps) ->
    proc_lib:start_link(?MODULE, init, [erlang:self(), Name, ConsumerCount, ConsumerFun, Rps]).

stop(Name) when is_atom(Name) ->
    case erlang:whereis(?MANAGER_NAME(Name)) of
        undefined ->
            {error, {not_found, Name}};
        ManagerPid ->
            stop(ManagerPid)
    end;
stop(Pid) ->
    case ?PID_NAME(Pid) of
        undefined ->
            {error, {not_found, Pid}};
        _ManagerName ->
            proc_lib:stop(Pid)
    end.

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init(Parent, Name, ConsumerCount, ConsumerFun, Rps) ->
    nthrottle:start_throttling(Name, Rps),
    nqueue_tab:create(Name),
    nhooks:do(nqueue, 'init_queue', [Name]),

    erlang:register(?MANAGER_NAME(Name), erlang:self()),
    Consumers = lists:map(
        fun(_I) ->
            new_consumer(Name, ConsumerFun)
        end,
        lists:seq(1, ConsumerCount)
    ),
    St = #st{name = Name, function = ConsumerFun, consumers = Consumers},

    Deb = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, erlang:self()}),
    loop(St, Parent, Deb).

terminate(#st{name = Name, consumers = Consumers}) ->
    nhooks:do(nqueue, 'terminate_queue', [Name]),
    lists:foreach(
        fun({_ConsumerRef, ConsumerPid}) ->
            erlang:exit(ConsumerPid, kill)
        end,
        Consumers
    ),
    nqueue_tab:destroy(Name),
    nthrottle:stop_throttling(Name),
    ok.

%%%-----------------------------------------------------------------------------
%%% SPECIAL PROCESS EXPORTS
%%%-----------------------------------------------------------------------------
system_continue(Parent, Deb, St) ->
    loop(St, Parent, Deb).

system_terminate(_Reason, _Parent, _Deb, St) ->
    terminate(St).

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

system_get_state(St) ->
    {ok, St}.

system_replace_state(StateFun, St) ->
    NSt = StateFun(St),
    {ok, NSt, NSt}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
loop(St, Parent, Deb) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(
                Request,
                From,
                Parent,
                ?MODULE,
                Deb,
                St
            );
        {'DOWN', MonitorRef, _Type, _Object, _Info} ->
            CleanConsumers = lists:keydelete(MonitorRef, 1, St#st.consumers),
            Consumer = new_consumer(St#st.name, St#st.function),
            loop(St#st{consumers = [Consumer | CleanConsumers]}, Parent, Deb)
    end.

new_consumer(Name, ConsumerFun) ->
    {ok, {Pid, MonitorRef}} = nqueue_consumer:start_monitor(Name, ConsumerFun),
    {MonitorRef, Pid}.

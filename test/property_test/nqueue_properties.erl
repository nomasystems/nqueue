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
-module(nqueue_properties).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").
-include_lib("../nqueue_test.hrl").

%%% EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
prop_start_stop() ->
    ConsumerFun = fun(_X) ->
        2 = 1 + 1,
        ok
    end,
    QueuesType = ?LET(
        {QueuesCount, ConsumersCount},
        {triq_dom:int(1, 100), triq_dom:int(1, 1000)},
        [
            #{
                name => erlang:list_to_atom("queue_" ++ erlang:integer_to_list(I)),
                count => ConsumersCount,
                fn => ConsumerFun
            }
            || I <- lists:seq(0, QueuesCount)
        ]
    ),
    ?FORALL(
        Queues,
        QueuesType,
        begin
            lists:foreach(
                fun(#{name := Name, count := Count, fn := Fun}) ->
                    {ok, _Pid} = nqueue:start_link(Name, Count, Fun)
                end,
                Queues
            ),
            lists:foreach(
                fun(#{name := Name, count := Count}) ->
                    Count = erlang:length(?GET_WORKERS(Name))
                end,
                Queues
            ),
            lists:foreach(
                fun(#{name := Name}) ->
                    Pid = erlang:whereis(?MANAGER_NAME(Name)),
                    erlang:monitor(process, Pid),
                    nqueue:stop(Name),
                    ok = wait_until_dies(Pid),
                    undefined = nqueue:info(Name)
                end,
                Queues
            ),
            true
        end
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
wait_until_dies(Pid) ->
    receive 
        {'DOWN', _MRef, process, Pid, normal} ->
            ok
    after 1000 ->
        error
    end.

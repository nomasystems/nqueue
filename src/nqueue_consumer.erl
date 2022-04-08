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
-module(nqueue_consumer).

%%% INCLUDE FILES
-include_lib("kernel/include/logger.hrl").

%%% START/STOP EXPORTS
-export([start_monitor/2]).

%%% INTERNAL EXPORTS
-export([loop/2]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_monitor(Queue, Fun) ->
    Pid = erlang:spawn_opt(?MODULE, loop, [Queue, Fun], [monitor, {fullsweep_after, 10}]),
    {ok, Pid}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
loop(Queue, Fun) ->
    case nthrottle:throttle(Queue, {erlang:self(), throttle_consume}) of
        ok ->
            consume(Queue, Fun);
        rps_exceeded ->
            receive
                throttle_consume ->
                    consume(Queue, Fun)
            end
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
consume(Queue, Fun) ->
    try
        Fun(nqueue:out(Queue))
    catch
        Class:Reason:Stacktrace ->
            case ets:whereis(Queue) of
                undefined ->
                    erlang:exit({not_found, Queue});
                _Tid ->
                    ?LOG_ERROR("Error consuming the queue ~p. Error: ~p:~p. Stacktrace: ~p", [
                        Queue,
                        Class,
                        Reason,
                        Stacktrace
                    ]),
                    erlang:raise(Class, Reason, Stacktrace)
            end
    end,
    ?MODULE:loop(Queue, Fun).

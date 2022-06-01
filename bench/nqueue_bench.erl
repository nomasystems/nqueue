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
-module(nqueue_bench).

%%% EXTERNAL EXPORTS
-export([
    bench/0
]).

%%% INTERNAL EXPORTS
-export([
    loop/2
]).

%%% RECORDS
-record(scenario, {
    name :: atom(),
    consumers :: integer(),
    producers :: integer(),
    items :: integer()
}).

%%% MACROS
-define(COUNTER, counter).
-define(TIME_UNIT, millisecond).
-define(TIMES, 3).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
bench() ->
    Scenarios = [
        #scenario{
            name = '2500C-5000P-500I',
            consumers = 2500,
            producers = 5000,
            items = 500
        },
        #scenario{
            name = '5000C-2500P-500I',
            consumers = 5000,
            producers = 2500,
            items = 500
        },
        #scenario{
            name = '5000C-5000P-5000I',
            consumers = 5000,
            producers = 5000,
            items = 5000
        },
        #scenario{
            name = '250000C-500000P-5I',
            consumers = 250000,
            producers = 500000,
            items = 5
        },
        #scenario{
            name = '500000C-250000P-5I',
            consumers = 500000,
            producers = 250000,
            items = 5
        },
        #scenario{
            name = '500000C-500000P-5I',
            consumers = 500000,
            producers = 500000,
            items = 5
        }
    ],
    TimeUnit = time_unit(?TIME_UNIT),
    Times = ?TIMES,
    io:format("~20s  ~30s  ~30s\n", [
        "Scenario", "nqueue (" ++ TimeUnit ++ ")", "npqueue (" ++ TimeUnit ++ ")"
    ]),
    lists:foreach(
        fun(Scenario) ->
            {NQueueMean, NQueueStdDev} = statistics(
                fun(_I) -> bench(nqueue, Scenario) end,
                Times
            ),
            {NPQueueMean, NPQueueStdDev} = statistics(
                fun(_I) -> bench(npqueue, Scenario) end,
                Times
            ),
            io:format(
                "~20s  ~30s  ~30s\n",
                [
                    Scenario#scenario.name,
                    io_lib:format("~.1f (±~6.1f)", [NQueueMean, NQueueStdDev]),
                    io_lib:format("~.1f (±~6.1f)", [NPQueueMean, NPQueueStdDev])
                ]
            )
        end,
        Scenarios
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
loop(N, P) ->
    erlang:register(?COUNTER, erlang:self()),
    loop(0, N, undefined, P).

loop(L, L, Ts, P) ->
    P ! Ts,
    erlang:unregister(?COUNTER);
loop(N, L, _Ts, P) ->
    receive
        Ts1 ->
            loop(N + 1, L, Ts1, P)
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
statistics(Fun, Times) ->
    Values = lists:map(Fun, lists:seq(1, Times)),
    Mean = lists:sum(Values) / Times,
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Values]) / Times,
    StdDev = math:sqrt(Variance),
    {Mean, StdDev}.

bench(
    Mod,
    #scenario{
        name = Name,
        consumers = Consumers,
        producers = Producers,
        items = Items
    }
) ->
    ConsumerFun = fun(_Arg) -> ?COUNTER ! erlang:monotonic_time() end,
    erlang:spawn(?MODULE, loop, [Producers * Items, erlang:self()]),
    start_queue(Mod, Name, Consumers, ConsumerFun),
    Begin = erlang:monotonic_time(),
    lists:foreach(
        fun(_I) ->
            erlang:spawn(fun() ->
                lists:foreach(
                    fun(_J) ->
                        Mod:in(Name, undefined)
                    end,
                    lists:seq(1, Items)
                )
            end)
        end,
        lists:seq(1, Producers)
    ),
    End =
        receive
            Ts ->
                Ts
        end,
    stop_queue(Mod, Name),
    timer:sleep(1000),
    erlang:convert_time_unit(End - Begin, native, ?TIME_UNIT).

start_queue(npqueue, Name, Consumers, ConsumerFun) ->
    {ok, Pid} = npqueue:start_link(Name, 1, Consumers, ConsumerFun),
    true = erlang:unlink(Pid),
    ok;
start_queue(Mod, Name, Consumers, ConsumerFun) ->
    {ok, Pid} = Mod:start_link(Name, Consumers, ConsumerFun),
    true = erlang:unlink(Pid),
    ok.

stop_queue(Mod, Name) ->
    Mod:stop(Name).

time_unit(second) ->
    "s";
time_unit(millisecond) ->
    "ms";
time_unit(microsecond) ->
    "us";
time_unit(nanosecond) ->
    "ns".

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
%%%
-module(nqueue_SUITE).

-include("nqueue_test.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        start_stop,
        api,
        not_found,
        consumer_throwing,
        consumer_throwing_no_log,
        consumers_waiting,
        throttling,
        supervision,
        hooks,
        performance
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    ct_property_test:init_per_suite(Config).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
start_stop() ->
    [{userdata, [{doc, "Tests the start/stop functionalities for several concurrent queues."}]}].

start_stop(Conf) ->
    ct_property_test:quickcheck(
        nqueue_properties:prop_start_stop(),
        Conf
    ).

api() ->
    [{userdata, [{doc, "Tests the exposed API methods."}]}].

api(_Conf) ->
    Name = test_queue,
    Consumers = 1,
    ConsumerFun = fun(_X) ->
        2 = 1 + 1,
        ok
    end,

    {ok, QueuePid} = nqueue:start_link(Name, Consumers, ConsumerFun),

    Name = proplists:get_value(name, nqueue:info(Name)),
    true = nqueue:is_empty(Name),
    0 = nqueue:len(Name),

    ok = nqueue:in(Name, element),
    1 = nqueue:total_in(Name),
    1 = nqueue:total_out(Name),

    ok = nqueue:in(Name, element_2),
    2 = nqueue:total_in(Name),
    1 = nqueue:total_out(Name),
    1 = nqueue:len(Name),
    [{2, element_2}] = nqueue:to_list(Name),
    element_2 = nqueue:out(Name),

    {ok, infinity} = nqueue:rps(Name),
    ok = nqueue:rps(Name, 100),
    {ok, 100} = nqueue:rps(Name),

    [] = nqueue:to_list(Name),

    nqueue:stop(QueuePid),
    ok.

not_found() ->
    [
        {userdata, [
            {doc, "Tests the behaviour of the API when a wrong queue reference is provided."}
        ]}
    ].

not_found(_Conf) ->
    Name = test_queue,
    Pid = erlang:self(),
    {error, {not_found, Name}} = nqueue:stop(Name),
    {error, {not_found, Pid}} = nqueue:stop(Pid),
    {error, {not_found, Name}} = nqueue:in(Name, element),
    ok.

consumer_throwing() ->
    [{userdata, [{doc, "Tests that a consumer throwing doesn't crash the queue."}]}].

consumer_throwing(_Conf) ->
    Name = test_queue_throwing,
    ConsumerFun = fun(_) -> throw(throwing) end,
    {ok, QueuePid} = nqueue:start_link(Name, 1, ConsumerFun),
    Ref = erlang:monitor(process, QueuePid),
    ok = nqueue:in(Name, something),
    receive
        {'DOWN', Ref, _, _, _} -> throw(did_crash)
    after 100 -> ok
    end,
    nqueue:stop(QueuePid).

consumer_throwing_no_log() ->
    [{userdata, [{doc, "Tests that a consumer throwing doesn't crash the queue."}]}].

consumer_throwing_no_log(_Conf) ->
    ok = logger:set_module_level(nqueue_consumer, none),
    Name = test_queue_throwing,
    ConsumerFun = fun(_) -> throw(throwing) end,
    {ok, QueuePid} = nqueue:start_link(Name, 1, ConsumerFun),
    Ref = erlang:monitor(process, QueuePid),
    ok = nqueue:in(Name, something),
    receive
        {'DOWN', Ref, _, _, _} -> throw(did_crash)
    after 100 -> ok
    end,
    ok = logger:unset_module_level(nhooks).

consumers_waiting() ->
    [{userdata, [{doc, "Tests that consumers keep waiting until new elements join the queue."}]}].

consumers_waiting(_Conf) ->
    Name = test_queue,
    Consumers = 10,
    Producers = 5,
    ConsumerFun = fun(_X) ->
        2 = 1 + 1,
        ok
    end,
    ProducerFun = fun() -> nqueue:in(Name, something) end,

    {ok, _QueuePid} = nqueue:start_link(Name, Consumers, ConsumerFun),
    lists:foreach(fun(_I) -> ProducerFun() end, lists:seq(1, Producers)),

    Producers = nqueue:total_out(Name),
    Producers = nqueue:total_in(Name),
    0 = nqueue:len(Name),

    timer:sleep(1000),
    lists:foreach(fun(_I) -> ProducerFun() end, lists:seq(1, Producers)),

    Consumers = nqueue:total_out(Name),
    Consumers = nqueue:total_in(Name),
    0 = nqueue:len(Name),

    nqueue:stop(Name),
    ok.

hooks() ->
    [{userdata, [{doc, "Tests that nqueue exposes hooks."}]}].

hooks(_Conf) ->
    true = erlang:is_list(nqueue:hooks()),
    ok.

throttling() ->
    [{userdata, [{doc, "Tests throttling."}]}].

throttling(_Conf) ->
    Name = test_queue,
    Consumers = 10,
    Pid = self(),
    ConsumerFun = fun(X) -> Pid ! {out, X} end,
    ProducerFun = fun(X) -> nqueue:in(Name, X) end,
    ReceiveFun = fun
        Receive(0) ->
            ok;
        Receive(Count) ->
            receive
                {out, _X} ->
                    Receive(Count - 1)
            end
    end,
    CountIn = Consumers * 10,
    Rps = 1,

    {ok, _QueuePid} = nqueue:start_link(Name, Consumers, ConsumerFun, Rps),
    true = nqueue:is_empty(Name),

    lists:foreach(ProducerFun, lists:seq(1, CountIn)),
    timer:sleep(1000),
    true = nqueue:total_out(Name) < 3,

    nqueue:rps(Name, 5),
    timer:sleep(1000),

    nqueue:rps(Name, infinity),
    ReceiveFun(CountIn),

    true = nqueue:is_empty(Name),
    nqueue:stop(Name),
    ok.

supervision() ->
    [{userdata, [{doc, "Tests the manager restart strategy."}]}].

supervision(_Conf) ->
    Name = test_queue,
    Consumers = 2,
    ConsumerFun = fun(_X) -> ok end,

    {ok, _QueuePid} = nqueue:start_link(Name, Consumers, ConsumerFun),

    [
        {_Consumer1Ref, Consumer1Pid} = Consumer1,
        _Consumer2
    ] = ?GET_WORKERS(Name),

    true = erlang:is_pid(Consumer1Pid),
    erlang:monitor(process, Consumer1Pid),
    erlang:exit(Consumer1Pid, kill),
    receive
        {'DOWN', _MonitorRef, _process, _Pid, _kill} ->
            ok
    after 3000 ->
        erlang:throw({error, {manager, restart_strategy}})
    end,

    NewWorkers = ?GET_WORKERS(Name),
    false = lists:member(Consumer1, NewWorkers),

    nqueue:stop(Name),
    ok.

performance() ->
    [{userdata, [{doc, "Tests the performance."}]}].

performance(_Conf) ->
    Name = test_queue,
    Consumers = ct:get_config(consumers, 1000),
    Producers = ct:get_config(producers, 1000),
    Items = ct:get_config(items, 1000),
    Total = Producers * Items,

    CountersRef = counters:new(1, [write_concurrency]),

    Consume = fun(_Item) ->
        counters:add(CountersRef, 1, 1)
    end,
    Produce = fun
        Produce(_N, 0) ->
            ok;
        Produce(N, Count) ->
            nqueue:in(Name, {N, Count}),
            Produce(N, Count - 1)
    end,
    IsAllConsumed = fun() ->
        case counters:get(CountersRef, 1) of
            Total ->
                true;
            Other ->
                ct:print("Other: ~p~nLen: ~p", [Other, nqueue:len(Name)]),
                false
        end
    end,
    WaitTrue = fun WaitTrue(Fun) ->
        case Fun() of
            true ->
                ok;
            false ->
                timer:sleep(1000),
                WaitTrue(Fun)
        end
    end,

    SecondsBefore = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),

    {ok, QueuePid} = nqueue:start_link(Name, Consumers, Consume),
    ct:print("~p consumers ready...", [Consumers]),

    lists:foreach(
        fun(N) -> spawn(fun() -> Produce(N, Items) end) end, lists:seq(1, Producers)
    ),
    ct:print("~p producers created...", [Producers]),

    ok = WaitTrue(IsAllConsumed),
    0 = nqueue:len(Name),
    [] = nqueue:to_list(Name),
    Total = nqueue:total_in(Name),
    Total = nqueue:total_out(Name),

    SecondsAfter = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Seconds = SecondsAfter - SecondsBefore,
    ct:print("All ~p items processed in ~p seconds! (~p items/sec)", [
        Total,
        Seconds,
        Total div Seconds
    ]),

    nqueue:stop(QueuePid),
    ok.

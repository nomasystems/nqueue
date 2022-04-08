# nqueue
![nqueue](https://github.com/nomasystems/nqueue/actions/workflows/build.yml/badge.svg)

`nqueue` is an OTP library to spawn/manage queues based on ETS public tables.

## Setup

Add `nqueue` to your project dependencies.

```erl
%%% e.g., rebar.config
{deps, [
    {nqueue, {git, "git@github.com:nomasystems/nqueue.git", {tag, "1.0.0"}}}
]}.
```

## Features

`nqueue` exposes utilities via its API that allows you to:

| Function | Description |
| --------  | ------------ |
| `nqueue:start_link/3` | Start a queue without rate limit |
| `nqueue:start_link/4` | Start a queue with a custom rate limit |
| `nqueue:in/2` | Insert a `Item` in a queue. |
| `nqueue:out/1` | Consume a `Item` from a queue. |
| `nqueue:info/1` |  Returns the information associated with `ets` |
| `nqueue:is_empty/1` | Returns if a queue is empty |
| `nqueue:len/1` | Returns the count of `Items` are in a queue| 
| `nqueue:to_list/1` | Returns a list of `Items` are in a queue |
| `nqueue:total_in/1` | Returns the count of `Items` added in a queue |
| `nqueue:total_out/1` | Returns the count of `Items` consumed in a queue |

This set of functionalities provides concurrent and performant production/consumption in several concurrent queues.

## Implementation

Each queue, defined by a name, is sustained by an ETS table. The ETS table stores, apart from the data to be consumed (i.e., the produced data) and the consumers ready to consume, a pair of counters that matches a produced element with its consumer. Thus, each time a consumer tries to consume an element from the queue, it receives a numerical index (i.e., the consumer counter) that specifies which of the produced elements is the one it will consume. This means that a consumer that receives a 2, will consume the second element in the queue. As the element is stored with its index as its key, the element is directly accessed by the consumer via a lookup to the table.

When a consumer tries to consume an element that is not already in the queue (i.e., the consumers go faster than the producers), it stores itself as waiting in the queue. Each time a producer generates a new element to be processed in the queue pipeline, it checks if the correspondent consumer is waiting before enqueuing the element. If it were the case, the producer would send the item directly to the consumer without being written/read from the ETS table, allowing for a faster workflow.


## A simple example

```erl
%%% Start a queue with 2 consumers
1> nqueue:start_link(
1>     QueueName      = my_queue,
1>     ConsumersCount = 2,
1>     ConsumersFun   = fun(Element) ->
1>         timer:sleep(1000),
1>         my_consumer_fun(Element)
1>     end
1> ).
{ok,<0.185.0>}

%%% Insert elements in the queue
2> nqueue:in(my_queue, element).
ok
3> nqueue:in(my_queue, element_2).
ok
4> nqueue:in(my_queue, element_3).
ok

%%% Inspect the queue
5> nqueue:len(my_queue).
1
6> [{3, element_3}] = nqueue:to_list(my_queue).
[{3,element_3}]

%%% Inspect the queue after all the consumptions
7> timer:sleep(1000).
ok
8> nqueue:len(my_queue).
0
9> nqueue:total_in(my_queue).
3
10> nqueue:total_out(my_queue).
3

%%% Stop the queue
11> nqueue:stop(my_queue).
true
```

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/nqueue/issues).
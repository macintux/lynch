lynch
=====

Implement algorithms from Nancy Lynch's Distributed Algorithms book,
in Erlang.

### runtime

The `runtime` library defines a framework for testing algorithms. It
is in its very early stages, and currently only supports synchronous
ring topologies.


    1> application:start(runtime).
    ok
    2> runtime:run(lcr, 3).
    ok
    3> runtime:crank(verbose).
    Round: 0
    Proc #1 (291147458) will send 291147458 (status unknown)
    Proc #2 (388349957) will send 388349957 (status unknown)
    Proc #3 (486716090) will send 486716090 (status unknown)

    ok
    4> runtime:crank(verbose).
    Round: 1
    Proc #1 (291147458) will send 486716090 (status unknown)
    Proc #2 (388349957) will send null (status unknown)
    Proc #3 (486716090) will send null (status unknown)

    ok
    5> runtime:crank(verbose).
    Round: 2
    Proc #1 (291147458) will send 486716090 (status unknown)
    Proc #2 (388349957) will send 486716090 (status unknown)
    Proc #3 (486716090) will send null (status unknown)

    I'm the leader: 3/486716090
    ok
    6> runtime:crank(verbose).
    Algorithm has reached a stopping point
    Stopped: stop
    ok

The `runtime` application defines `crank/{0,1}` functions which "turn
the crank" on the processes, moving the round (as defined in Lynch's
synchronous algorithms) forward each time. `crank(verbose)` will print
a status dump before executing the round.

`runtime:autocrank()` can be used instead of repeatedly turning the
crank.

The `process` module defines a `process` behavior for the algorithm to
leverage.

### Application flow
1. When `application:start(runtime)` is invoked a supervisor and
  runtime process will launch.
2. `runtime:run(Algorithm, <n>)` will spawn `n` processes with the specified algorithm module.
3. `runtime:crank/{0,1}` or `runtime:autocrank/0` will start the synchronous network spinning.
4. `runtime` will call `process:start_round/2` for each algorithm process at each turn of the crank.
5. Each algorithm process will call `Algorithm:step/2`, which will return `stop`, `continue`, or `messages`; in the last case, a list of messages with routing information will be stored in the process state for later retrieval and delivery
6. If any process returns `stop` via `start_round`, the runtime will end the algorithm.
7. If the algorithm is not yet complete, `runtime` will call `process:retrieve_messages` for each algorithm process for a list of messages to deliver.
8. Between `retrieve_messages` calls each batch of returned messages is sent via `process:message` and `Algorithm:handle_message` to the appropriate destination; if any of those messages results in a `stop` response, the runtime will end the algorithm after that batch has been fully delivered.

### Complexity analysis

`runtime:info/0` will return the runtime state, which includes

* Number of rounds executed so far
* Number of messages sent
* Number of processes
* Whether the running algorithm (if any) has reached a point of
  completion

`info/0` is invoked as the last act of `autocrank/0`.

lynch
=====

Implement algorithms from Nancy Lynch's Distributed Algorithms book,
in Erlang.

### runtime

The `runtime` library defines a framework for testing algorithms. It
is in its very early stages, and currently only supports synchronous
ring topologies (and is largely untested even for that).


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

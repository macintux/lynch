lynch
=====

Implement algorithms from Nancy Lynch's Distributed Algorithms book

### runtime

The `runtime` library defines a framework for testing algorithms. It
is in its very early stages, and currently only supports synchronous
ring topologies (and is largely untested even for that).

Example of running the `testproc` process:

    1> application:start(runtime).
    ok
    2> runtime:run(3, testproc).
    ok
    3> runtime:crank().
    3 received {hi_from,1} from 1 in round 1
    1 received {hi_from,2} from 2 in round 1
    2 received {hi_from,3} from 3 in round 1
    ok
    4> runtime:crank().
    3 received {hi_from,1} from 1 in round 2
    1 received {hi_from,2} from 2 in round 2
    2 received {hi_from,3} from 3 in round 2
    ok

The `runtime` application defines a `crank/0` function which "turns
the crank" on the processes, moving the round (as defined in Lynch's
synchronous algorithms) forward each time.

In the above example, each process sends one message to the process
with UID one less than its own.

The `process` module defines a `process` behavior with a (thus far)
simple interface: `start/1`, `init/1` (probably should combine those),
`step/2` and `handle_message/4`.

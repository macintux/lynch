lynch
=====

Implement algorithms from Nancy Lynch's Distributed Algorithms book,
in Erlang.

### runtime

The `runtime` library defines a framework for testing algorithms. It
is in its very early stages, and currently only supports synchronous
ring topologies (and is largely untested even for that).

    107> application:start(runtime).
    ok
    108> runtime:run(3, lcr).
    ok
    109> runtime:crank(verbose).
    Next round: 1
    Proc #1 (108836655) will send 108836655 (status unknown)
    Proc #2 (403276418) will send 403276418 (status unknown)
    Proc #3 (230776318) will send 230776318 (status unknown)
    ok
    110> runtime:crank(verbose).
    Next round: 2
    Proc #1 (108836655) will send 230776318 (status unknown)
    Proc #2 (403276418) will send null (status unknown)
    Proc #3 (230776318) will send 403276418 (status unknown)
    ok
    111> runtime:crank(verbose).
    Next round: 3
    Proc #1 (108836655) will send 403276418 (status unknown)
    Proc #2 (403276418) will send null (status unknown)
    Proc #3 (230776318) will send 403276418 (status unknown)
    I'm the leader: 2/403276418
    ok
    112> runtime:crank().
    Cluster member crashed
    ok
    113>
    =ERROR REPORT==== 10-Nov-2013::21:25:46 ===
    ** Generic server runtime terminating
    ** Last message in was {'DOWN',#Ref<0.0.0.41>,process,<0.43.0>,normal}
    ** When Server state == {state,6,
                                   [<0.44.0>,<0.43.0>,<0.42.0>]}
    ** Reason for termination ==
    ** "Cluster unstable"

The `runtime` application defines `crank/{0,1}` functions which "turn
the crank" on the processes, moving the round (as defined in Lynch's
synchronous algorithms) forward each time. `crank(verbose)` will print
a status dump before executing the round.

The reason for the crash at the end of the above example? The process
which is elected leader indicates a stop, so next time its `step`
function is called the tail recursive loop terminates.

The `process` module defines a `process` behavior with a (thus far)
simple interface: `start/2`, `step/2`, `handle_message/4`, and
`dump/1`.

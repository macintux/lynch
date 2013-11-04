%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2013 by John Daily <jd@epep.us>

-module(ringproc).
-compile(export_all).

-record(state, {
          left,
          right
         }).

init() ->
    io:format("Spawned ~p~n", [self()]),
    setup(#state{}).

setup(State) ->
    receive
        {left, Pid} ->
            check_setup(State#state{left=Pid});
        {right, Pid} ->
            check_setup(State#state{right=Pid})
    end.

check_setup(#state{left=undefined}=State) ->
    setup(State);
check_setup(#state{right=undefined}=State) ->
    setup(State);
check_setup(State) ->
    wait_for_crank(State).

wait_for_crank(State) ->
    io:format("Process is waiting for the crank to turn~n", []),
    receive
        step ->
            wait_for_crank(step(State));
        Msg ->
            throw({unknown_message, Msg})
    end.

step(State) ->
    State.



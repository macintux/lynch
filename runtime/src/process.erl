%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2013 by John Daily <jd@epep.us>

%% Should consider making this a gen_fsm
-module(process).
-include("process.hrl").

-export([start/3]).

-callback start(Uid :: uid(), I :: i()) -> no_return().

-callback step(Round :: round_id(), State :: term()) ->
    {'messages', list(message()), NewState :: term()} |
    {'noreply', NewState :: term()}.

-callback handle_message(Message :: term(), From :: uid(),
                         Round :: round_id(), State :: term()) ->
    {ok, NewState :: term()}.

-record(state, {
          module,
          round=0,
          i=-1,
          algorithm_state
         }).
-type state() :: #state{}.

-spec start(Module :: atom(), I :: pos_integer(),
            AlgState :: term()) -> no_return().
start(Module, I, AlgState) ->
    wait_for_crank(#state{
                      algorithm_state=AlgState,
                      i=I,
                      module=Module
                     }).

-spec wait_for_crank(state()) -> no_return().
wait_for_crank(#state{algorithm_state=AlgState,i=I,
                      round=Round, module=Module}=State) ->
    receive
        {step, {round, Round}} ->
            wait_for_crank(State#state{
                             algorithm_state=
                                 handle_step_response(
                                   Module:step({round, Round+1}, AlgState),
                                   {i, I},
                                   {round, Round+1}
                                  ),
                             round=Round+1
                            });
        {msg, {i, From}, {round, Round}, Msg} ->
            {ok, NewAlgState} =
                Module:handle_message(Msg, {i, From}, {round, Round}, AlgState),
            wait_for_crank(State#state{
                             algorithm_state=NewAlgState});
        dump ->
            io:format("===~n~p~n", [AlgState]),
            wait_for_crank(State)
    end.

-spec handle_step_response({'messages',
                            Messages :: list(message()),
                            AlgState :: term()} |
                           {'noreply', AlgState :: term()},
                           From :: i(),
                           Round :: round_id()) ->
                                  AlgState :: term().
handle_step_response({messages, Messages, State}, From, Round) ->
    lists:foreach(fun({Dest, Message}) -> runtime:msg(Dest, From, Round, Message) end,
                  Messages),
    State;
handle_step_response({noreply, State}, _From, _Round) ->
    State.
                          

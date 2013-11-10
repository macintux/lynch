%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2013 by John Daily <jd@epep.us>

%% Should consider making this a gen_fsm
-module(process).
-include("process.hrl").

-export([start/2]).

-callback start(Uid :: uid()) -> no_return().

-callback init(Uid :: uid()) ->
    State :: term().

-callback step(Round :: round_id(), State :: term()) ->
    {'messages', list(message()), NewState :: term()} |
    {'noreply', NewState :: term()}.

-callback handle_message(Message :: term(), From :: uid(),
                         Round :: round_id(), State :: term()) ->
    {ok, NewState :: term()}.

-record(state, {
          module,
          round=0,
          uid=-1,
          algorithm_state
         }).
-type state() :: #state{}.

-spec start(Module :: atom(), Id :: uid()) -> no_return().
start(Module, {uid, Id}) ->
    wait_for_crank(#state{
                      algorithm_state=Module:init({uid, Id}),
                      uid=Id,
                      module=Module
                     }).

-spec wait_for_crank(state()) -> no_return().
wait_for_crank(#state{algorithm_state=AlgState,uid=Uid,
                      round=Round, module=Module}=State) ->
    receive
        {step, {round, Round}} ->
            wait_for_crank(State#state{
                             algorithm_state=
                                 handle_step_response(
                                   Module:step({round, Round+1}, AlgState),
                                   {uid, Uid},
                                   {round, Round+1}
                                  ),
                             round=Round+1
                            });
        {msg, {uid, From}, {round, Round}, Msg} ->
            {ok, NewAlgState} =
                Module:handle_message(Msg, {uid, From}, {round, Round}, AlgState),
            wait_for_crank(State#state{
                             algorithm_state=NewAlgState})
    end.

-spec handle_step_response({'messages',
                            Messages :: list(message()),
                            AlgState :: term()} |
                           {'noreply', AlgState :: term()},
                           Uid :: uid(),
                           Round :: round_id()) ->
                                  AlgState :: term().
handle_step_response({messages, Messages, State}, Uid, Round) ->
    lists:foreach(fun({Dest, Message}) -> runtime:msg(Dest, Uid, Round, Message) end,
                  Messages),
    State;
handle_step_response({noreply, State}, _Uid, _Round) ->
    State.
                          

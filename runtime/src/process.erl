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
    {'noreply', NewState :: term()} |
    {'stop', NewState :: term()}.

-callback handle_message(Message :: term(), From :: uid(),
                         Round :: round_id(), State :: term()) ->
    {ok, NewState :: term()}.

-callback dump(State :: term()) -> iolist().

-record(state, {
          module,
          round=0,
          i=-1,
          algorithm_state,
          stop=false
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

-spec wait_for_crank(state()) -> done.
wait_for_crank(#state{stop=true}) ->
    done;
wait_for_crank(#state{algorithm_state=AlgState,
                      round=Round, module=Module}=State) ->
    receive
        {step, {round, Round}} ->
            wait_for_crank(handle_step_response(
                             Module:step({round, Round+1}, AlgState),
                             State));
        {msg, From, {round, Round}, Msg} ->
            {ok, NewAlgState} =
                Module:handle_message(Msg, From, {round, Round}, AlgState),
            wait_for_crank(State#state{
                             algorithm_state=NewAlgState});
        dump ->
            io:format("~ts", [Module:dump(AlgState)]),
            wait_for_crank(State)
    end.

-spec handle_step_response({'messages',
                            Messages :: list(message()),
                            AlgState :: term()} |
                           {'noreply', AlgState :: term()} |
                           {'stop', AlgState :: term()},
                           State :: state()) -> NewState :: state().
handle_step_response({messages, Messages, AlgState},
                     #state{round=Round, i=I}=State) ->
    %% Describe the "From" for this message in the same terms as the
    %% "To". If the sender describes a relative position (left or
    %% right with a number of servers) then pass the source for the
    %% message the same way.
    lists:foreach(fun({{i, _D}=Dest, Message}) ->
                          runtime:msg(Dest, {i, I},
                                      {round, Round+1}, Message);
                     ({{i, _D, left, Rel}=Dest, Message}) ->
                          runtime:msg(Dest, {i, I, right, Rel},
                                      {round, Round+1}, Message);
                     ({{i, _D, right, Rel}=Dest, Message}) ->
                          runtime:msg(Dest, {i, I, left, Rel},
                                      {round, Round+1}, Message)
                  end,
                  Messages),
    State#state{round=Round+1, algorithm_state=AlgState};
handle_step_response({noreply, AlgState}, #state{round=Round}=State) ->
    State#state{round=Round+1, algorithm_state=AlgState};
handle_step_response({stop, AlgState}, #state{round=Round}=State) ->
    State#state{round=Round+1, algorithm_state=AlgState, stop=true}.

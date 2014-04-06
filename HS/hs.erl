%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%   Implement the Hirschberg and Sinclair algorithm for leader
%%%   election as described in Lynch section 3.4.
%%% @end
%%% Created : 10 Nov 2013 by John Daily <jd@epep.us>

-module(hs).
-include("process.hrl").

-behavior(process).
-export([init/3, step/2, handle_message/4, dump/1]).

-record(state, {
          i,
          u,
          'send+',
          'send-',
          status=unknown,
          phase=0,
          halfmatch=false %% Last branch in algorithm
                          %% matches on two messages
         }).
-type state() :: #state{}.

-spec init(Uid :: uid(), I :: i(), Extra::list()) -> state().
init({uid, Uid}, {i, I}, _Extra) ->
    #state{
       i=I,
       u=Uid,
       'send+'={Uid, out, 1},
       'send-'={Uid, out, 1}
      }.

-spec step(Round :: round_id(), State :: state()) ->
                  {'messages', list(message()), NewState :: state()} |
                  {'continue', NewState :: state()} |
                  {'stop', NewState :: state()}.
step(_Round, #state{'send+'=null,'send-'=null}=State) ->
    {continue, State#state{halfmatch=false}};
step(_Round, #state{i=I, 'send+'=SendPlus,'send-'=null}=State) ->
    {messages, [
                {{i, I+1, right, 1}, SendPlus}
               ], State#state{'send+'=null,halfmatch=false}};
step(_Round, #state{i=I, 'send+'=null,'send-'=SendMinus}=State) ->
    {messages, [
                {{i, I-1, left, 1}, SendMinus}
               ], State#state{'send-'=null,halfmatch=false}};
step(_Round, #state{i=I, 'send+'=SendPlus,'send-'=SendMinus}=State) ->
    {messages, [
                {{i, I+1, right, 1}, SendPlus},
                {{i, I-1, left, 1}, SendMinus}
               ], State#state{'send+'=null,'send-'=null,halfmatch=false}}.

-spec handle_message(Message :: term(), From :: i(),
                     Round :: round_id(), State :: state()) ->
                            {'continue', NewState :: state()} |
                            {'stop', NewState :: state()}.
handle_message({V, out, _H}, _From, _Round,
               #state{u=U}=State) when V < U ->
    {continue, State}; %% Drop the message. This is not explicit in the book's detailed algorithm
handle_message({V, out, H}, {i, _From, left, 1}, _Round,
               #state{u=U,i=_I}=State) when V > U, H > 1 ->
    {continue, State#state{'send+'={V, out, H-1}}};
handle_message({V, out, H}, {i, _From, left, 1}, _Round,
               #state{u=U,i=_I}=State) when V > U, H =:= 1 ->
    {continue, State#state{'send-'={V, in, 1}}};
handle_message({V, out, _H}, {i, _From, left, 1}, _Round,
               #state{u=U,i=I}=State) when V =:= U ->
    io:format("I'm the leader: ~B/~B~n", [I, U]),
    {stop, State#state{status=leader}};
handle_message({V, out, H}, {i, _From, right, 1}, _Round,
               #state{u=U,i=_I}=State) when V > U, H > 1 ->
    {continue, State#state{'send-'={V, out, H-1}}};
handle_message({V, out, H}, {i, _From, right, 1}, _Round,
               #state{u=U,i=_I}=State) when V > U, H =:= 1 ->
    {continue, State#state{'send+'={V, in, 1}}};
handle_message({V, out, _H}, {i, _From, right, 1}, _Round,
               #state{u=U,i=I}=State) when V =:= U ->
    io:format("I'm the leader: ~B/~B~n", [I, U]),
    {stop, State#state{status=leader}};
handle_message({V, in, 1}, {i, _From, left, 1}, _Round,
               #state{u=U,i=_I}=State) when V =/= U ->
    {continue, State#state{'send+'={V, in, 1}}};
handle_message({V, in, 1}, {i, _From, right, 1}, _Round,
               #state{u=U,i=_I}=State) when V =/= U ->
    {continue, State#state{'send-'={V, in, 1}}};
handle_message({U, in, 1}, _I, _Round,
               #state{u=U,halfmatch=false}=State) ->
    {continue, State#state{halfmatch=true}};
handle_message({U, in, 1}, _I, _Round,
               #state{u=U,halfmatch=true,phase=Phase}=State) ->
    NewPhase = Phase + 1,
    {continue, State#state{'send+'={U, out, trunc(math:pow(2, NewPhase))},
                           'send-'={U, out, trunc(math:pow(2, NewPhase))},
                           phase=NewPhase,
                           halfmatch=false
                          }}.


-spec dump(State :: state()) -> iolist().
dump(#state{i=I, u=Uid, 'send+'=SendPlus, 'send-'=SendMinus,
            status=Status, phase=Phase}) ->
    io_lib:format("Proc #~B (~B) : send+ ~p / send- ~p (status ~p, phase ~B)~n",
                  [I, Uid, SendPlus, SendMinus, Status, Phase]).

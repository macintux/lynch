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
-export([start/2, step/2, handle_message/4, dump/1]).

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

-spec start(Uid :: uid(), I :: i()) -> no_return().
start({uid, Uid}, {i, I}) ->
    process:start(?MODULE,
                  I,
                  #state{
                     i=I,
                     u=Uid,
                     'send+'={Uid, out, 1},
                     'send-'={Uid, out, 1}
                    }).

-spec step(Round :: round_id(), State :: state()) ->
                  {'messages', list(message()), NewState :: state()} |
                  {'noreply', NewState :: state()} |
                  {'stop', NewState :: term()}.
step(_Round, #state{'send+'=null,'send-'=null}=State) ->
    {noreply, State#state{halfmatch=false}};
step(_Round, #state{i=I, 'send+'=SendPlus,'send-'=null}=State) ->
    {messages, [
                {{i, I+1}, SendPlus}
               ], State#state{'send+'=null,halfmatch=false}};
step(_Round, #state{i=I, 'send+'=null,'send-'=SendMinus}=State) ->
    {messages, [
                {{i, I-1}, SendMinus}
               ], State#state{'send-'=null,halfmatch=false}};
step(_Round, #state{i=I, 'send+'=SendPlus,'send-'=SendMinus}=State) ->
    {messages, [
                {{i, I+1}, SendPlus},
                {{i, I-1}, SendMinus}
               ], State#state{'send+'=null,'send-'=null,halfmatch=false}}.

-spec handle_message(Message :: term(), From :: i(),
                     Round :: round_id(), State :: state()) ->
    {ok, NewState :: term()}.
handle_message({V, out, _H}, _From, _Round,
               #state{u=U}=State) when V < U ->
    {ok, State}; %% Drop the message. This is not explicit in the book's algorithm
handle_message({V, out, H}, {i, From}, _Round,
               #state{u=U,i=I}=State) when From =:= I-1, V > U, H > 1 ->
    {ok, State#state{'send+'={V, out, H-1}}};
handle_message({V, out, H}, {i, From}, _Round,
               #state{u=U,i=I}=State) when From =:= I-1, V > U, H =:= 1 ->
    {ok, State#state{'send-'={V, in, 1}}};
handle_message({V, out, _H}, {i, From}, _Round,
               #state{u=U,i=I}=State) when From =:= I-1, V =:= U ->
    io:format("I'm the leader: ~B/~B~n", [I, U]),
    {stop, State#state{status=leader}};
handle_message({V, out, H}, {i, From}, _Round,
               #state{u=U,i=I}=State) when From =:= I+1, V > U, H > 1 ->
    {ok, State#state{'send-'={V, out, H-1}}};
handle_message({V, out, H}, {i, From}, _Round,
               #state{u=U,i=I}=State) when From =:= I+1, V > U, H =:= 1 ->
    {ok, State#state{'send+'={V, in, 1}}};
handle_message({V, out, _H}, {i, From}, _Round,
               #state{u=U,i=I}=State) when From =:= I+1, V =:= U ->
    io:format("I'm the leader: ~B/~B~n", [I, U]),
    {stop, State#state{status=leader}};
handle_message({V, in, 1}, {i, From}, _Round,
               #state{u=U,i=I}=State) when From =:= I-1, V =/= U ->
    {ok, State#state{'send+'={V, in, 1}}};
handle_message({V, in, 1}, {i, From}, _Round,
               #state{u=U,i=I}=State) when From =:= I+1, V =/= U ->
    {ok, State#state{'send-'={V, in, 1}}};
handle_message({U, in, 1}, {i, _From}, _Round,
               #state{u=U,halfmatch=false}=State) ->
    {ok, State#state{halfmatch=true}};
handle_message({U, in, 1}, {i, _From}, _Round,
               #state{u=U,halfmatch=true,phase=Phase}=State) ->
    NewPhase = Phase + 1,
    {ok, State#state{'send+'={U, out, trunc(math:pow(2, NewPhase))},
                     'send-'={U, out, trunc(math:pow(2, NewPhase))},
                     phase=NewPhase,
                     halfmatch=false
                    }}.


-spec dump(State :: state()) -> iolist().
dump(#state{i=I, u=Uid, 'send+'=SendPlus, 'send-'=SendMinus,
            status=Status, phase=Phase}) ->
    io_lib:format("Proc #~B (~B) : send+ ~p / send- ~p (status ~p, phase ~B)~n",
                  [I, Uid, SendPlus, SendMinus, Status, Phase]).

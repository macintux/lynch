%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%   Implement the Le Lann, Chang, and Roberts algorithm for leader
%%%   election as described in Lynch section 3.3.
%%% @end
%%% Created : 10 Nov 2013 by John Daily <jd@epep.us>

-module(lcr).
-include("process.hrl").

-behavior(process).
-export([start/2, step/2, handle_message/4]).

-record(state, {
          i,
          u,
          send,
          status=unknown
         }).
-type state() :: #state{}.

-spec start(Uid :: uid(), I :: i()) -> no_return().
start({uid, Uid}, {i, I}) ->
    process:start(?MODULE,
                  I,
                  #state{
                     i=I,
                     u=Uid,
                     send=Uid,
                     status=unknown
                    }).

-spec step(Round :: round_id(), State :: state()) ->
                  {'messages', list(message()), NewState :: state()} |
                  {'noreply', NewState :: state()}.
step(_Round, #state{send=null}=State) ->
    {noreply, State};
step(_Round, #state{i=I,send=Send}=State) ->
    {messages, [{{i, I+1}, Send}], State}.

-spec handle_message(Message :: term(), From :: i(),
                     Round :: round_id(), State :: state()) ->
    {ok, NewState :: term()}.
handle_message(RcvId, _From, _Round, #state{u=Uid}=State) when RcvId < Uid ->
    {ok, State#state{send=null}};
handle_message(RcvId, _From, _Round, #state{u=Uid, i=I}=State) when RcvId == Uid ->
    io:format("I'm the leader: ~B/~B~n", [I, Uid]),
    {ok, State#state{send=null,status=leader}};
handle_message(RcvId, _From, _Round, #state{u=Uid}=State) when RcvId > Uid ->
    {ok, State#state{send=RcvId}}.

    

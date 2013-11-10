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
-export([start/1, init/1, step/2, handle_message/4]).

-record(state, {
          uid,
          send,
          status=unknown
         }).
-type state() :: #state{}.

-spec start(Id :: uid()) -> no_return().
start(Id) ->
    process:start(?MODULE, Id).

-spec init(Uid :: uid()) ->
                  State :: state().
init({uid, Uid}) ->
    #state{
       uid=Uid,
       send=Uid,
       status=unknown
      }.

-spec step(Round :: round_id(), State :: state()) ->
                  {'messages', list(message()), NewState :: state()} |
                  {'noreply', NewState :: state()}.
step(_Round, #state{send=null}=State) ->
    {noreply, State};
step(_Round, #state{uid=Uid,send=Send}=State) ->
    {messages, [{{uid, Uid+1}, Send}], State}.

-spec handle_message(Message :: term(), From :: uid(),
                     Round :: round_id(), State :: state()) ->
    {ok, NewState :: term()}.
handle_message(RcvId, _From, _Round, #state{uid=Uid}=State) when RcvId < Uid ->
    {ok, State#state{send=null}};
handle_message(RcvId, _From, _Round, #state{uid=Uid}=State) when RcvId == Uid ->
    io:format("I'm the leader: ~B~n", [Uid]),
    {ok, State#state{send=null,status=leader}};
handle_message(RcvId, _From, _Round, #state{uid=Uid}=State) when RcvId > Uid ->
    {ok, State#state{send=RcvId}}.


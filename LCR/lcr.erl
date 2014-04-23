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
-export([init/3, step/2, handle_message/4, dump/1]).

-record(state, {
          i,               %% Position in the ring
          u,               %% Unique ID
          send,            %% UID to communicate next round (or null)
          status=unknown   %% unknown or leader
         }).
-type state() :: #state{}.

-spec init(Uid :: uid(), I :: i(), Extra::list()) -> state().
init({uid, Uid}, {i, I}, _Extra) ->
    #state{
       i=I,
       u=Uid,
       send=Uid,
       status=unknown
      }.

-spec step(Round :: round_id(), State :: state()) ->
                  {'messages', list(message()), NewState :: state()} |
                  {'continue', NewState :: state()} |
                  {'stop', NewState :: state()}.
step(_Round, #state{status=leader}=State) ->
    {stop, State};
step(_Round, #state{send=null}=State) ->
    {continue, State};
step(_Round, #state{i=I,send=Send}=State) ->
    {messages, [{{i, I+1}, Send}], State#state{send=null}}.

-spec handle_message(Message :: term(), From :: i(),
                     Round :: round_id(), State :: state()) ->
                            {'continue', NewState :: state()} |
                            {'stop', NewState :: state()}.
handle_message(RcvId, _From, _Round, #state{u=Uid}=State) when RcvId < Uid ->
    {continue, State#state{send=null}};
handle_message(RcvId, _From, _Round, #state{u=Uid, i=I}=State) when RcvId == Uid ->
    io:format("I'm the leader: ~B/~B~n", [I, Uid]),
    {stop, State#state{send=null,status=leader}};
handle_message(RcvId, _From, _Round, #state{u=Uid}=State) when RcvId > Uid ->
    {continue, State#state{send=RcvId}}.


-spec dump(State :: state()) -> iolist().
dump(#state{i=I, u=Uid, send=Send, status=Status}) ->
    io_lib:format("Proc #~B (~B) will send ~p (status ~p)~n",
                  [I, Uid, Send, Status]).

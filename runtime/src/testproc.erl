%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2013 by John Daily <jd@epep.us>

-module(testproc).
-include("process.hrl").

-behavior(process).
-export([start/1, init/1, step/2, handle_message/4]).

-spec start(Id :: uid()) -> no_return().
start(Id) ->
    process:start(?MODULE, Id).

-spec init(Uid :: uid()) ->
                  State :: term().
init(Uid) ->
    Uid.

-spec step(Round :: round_id(), State :: term()) ->
                  {'messages', list(message()), NewState :: term()} |
                  {'noreply', NewState :: term()}.
step(_Round, {uid, Uid}) ->
    {messages, [{{uid, Uid - 1}, {hi_from, Uid}}], {uid, Uid}}.

-spec handle_message(Message :: term(), From :: uid(),
                     Round :: round_id(), State :: term()) ->
    {ok, NewState :: term()}.
handle_message(Message, {uid, From}, {round, Round}, {uid, Uid}=State) ->
    io:format("~B received ~p from ~B in round ~B~n",
              [Uid, Message, From, Round]),
    {ok, State}.


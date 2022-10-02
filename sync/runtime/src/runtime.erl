%%%-------------------------------------------------------------------
%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2013 by John Daily <jd@epep.us>
%%%-------------------------------------------------------------------
-module(runtime).
-include("process.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, run/2, msg/4,
         crank/0, crank/1, autocrank/0,
         dump/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, info/0]).

-define(SERVER, ?MODULE).

-record(state, {
          round=0,
          stop=false,
          msg_tally=0,
          procs=[]
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, #state{}}.

dump() ->
    gen_server:call(?SERVER, dump).

messages_or_stop(ok) ->
    gen_server:call(?SERVER, messages),
    continue;
messages_or_stop(Status) ->
    io:format("Stopped: ~p~n", [Status]),
    stop.

autocrank() ->
    io:format("~ts~n", [dump()]),
    run_autocrank(messages_or_stop(gen_server:call(?SERVER, start_round))),
    info().

run_autocrank(continue) ->
    io:format("~ts~n", [dump()]),
    run_autocrank(messages_or_stop(gen_server:call(?SERVER, start_round)));
run_autocrank(stop) ->
    stop.

crank() ->
    messages_or_stop(gen_server:call(?SERVER, start_round)).

crank(verbose) ->
    io:format("~ts~n", [dump()]),
    crank().

-spec msg(Dest :: loc(), From :: loc(),
          Round :: round_id(), Message :: term()) -> 'ok'.
msg(Dest, From, Round, Message) ->
    gen_server:call(?SERVER, {msg, Dest, From, Round, Message}).

run(Module, Count) ->
    gen_server:call(?SERVER, {run, Count, Module}).

run_procs(0, _Module, Procs) ->
    Procs;
run_procs(Count, Module, Procs) ->
    {ok, Pid} = process:start_link(Module, {uid, randuid()}, {i, Count}, []),
    run_procs(Count-1, Module, [Pid|Procs]).

info() ->
    gen_server:call(?SERVER, info).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({run, Count, Module}, _From, #state{procs=[]}) ->
    {reply, ok, #state{round=0,procs=run_procs(Count, Module, [])}};
handle_call({run, Count, Module}, _From, #state{stop=true}) ->
    %% Start over
    {reply, ok, #state{round=0,procs=run_procs(Count, Module, [])}};
handle_call({run, _Count, _Module}, _From, State) ->
    {reply, already_running, State};
handle_call(start_round, _From, #state{procs=[]}=State) ->
    {reply, noprocs, State};
handle_call(start_round, _From, #state{stop=true}=State) ->
    {reply, stop, State};
handle_call(start_round, _From, #state{round=Round,procs=Procs}=State) ->
    %% If any of our processes indicate the algorithm is complete by
    %% returning stop, we need to also return stop.
    %%
    %% Hate using cases at all, much less nested, probably refactor.
    {Reply, NewState} =
        case lists:foldl(fun(X, Sum) ->
                                 Sum +
                                 case process:start_round(X, Round) of
                                     continue -> 0;
                                     stop -> 1
                                 end
                         end,
                         0, Procs) of
            0 -> {ok, State};
            _ -> {stop, State#state{stop=true}}
        end,
    {reply, Reply, NewState};
handle_call(messages, _From, #state{round=Round,procs=Procs}=State) ->
    NewState =
        lists:foldl(fun(X, NS) -> deliver_messages(
                                    process:retrieve_messages(X, Round), NS)
                    end,
                    State, Procs),
    {reply, ok, NewState#state{round=Round+1}};
handle_call(dump, _From, #state{stop=true}=State) ->
    {reply, "Algorithm has reached a stopping point", State};
handle_call(dump, _From, #state{round=Round,procs=Procs}=State) ->
    Dump = [io_lib:format("Round: ~B~n", [Round]) |
            lists:map(fun(X) -> process:dump(X) end, Procs)],
    {reply, Dump, State};
handle_call(info, _From, State) ->
    {reply, State, State}.

deliver_messages([], State) ->
    State;
deliver_messages([H|T], #state{msg_tally=N}=State) ->
    deliver_messages(T, deliver_message(H, State#state{msg_tally=N+1})).

%% Update state to stop=true if a stop response comes back from any process
deliver_message({all, From, Round, Message}, #state{procs=Procs}=State) ->
    NewState =
        case lists:foldl(fun(X, Sum) ->
                                 Sum +
                                 case process:message(X, From, Round, Message) of
                                     continue -> 0;
                                     stop -> 1
                                 end
                         end,
                         0, Procs) of
            0 -> State;
            _ -> State#state{stop=true}
        end,
    NewState;
deliver_message({{i, I}, From, Round, Message}, #state{procs=Procs}=State) ->
    case process:message(lists:nth(map_i(I, length(Procs)), Procs), From, Round, Message) of
        continue -> State;
        stop -> State#state{stop=true}
    end;
deliver_message({{i, I, _Dir, _Rel}, From, Round, Message}, #state{procs=Procs}=State) ->
    case process:message(lists:nth(map_i(I, length(Procs)), Procs), From, Round, Message) of
        continue -> State;
        stop -> State#state{stop=true}
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, _Pid, _Details}, State) ->
    io:format("Cluster member crashed~n", []),
    {stop, "Cluster unstable", State};
handle_info(Info, State) ->
    io:format("runtime saw unexpected message ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Will map an integer into the cluster size. Examples, given
%%      a cluster size of 5:
%%  -6 => 4
%%  -5 => 5
%%  -1 => 4
%%   0 => 5
%%   1 => 1
%%   5 => 5
%%   6 => 1
%%   7 => 2
%%  10 => 5
-spec map_i(N :: integer(), Size :: pos_integer()) -> pos_integer().
map_i(N, Size) when N < 0 ->
    Size + (N rem Size);
map_i(0, Size) ->
    Size;
map_i(N, Size) ->
    check_for_zero(N rem Size, Size).

-spec check_for_zero(non_neg_integer(), pos_integer()) -> pos_integer().
check_for_zero(0, Size) ->
    Size;
check_for_zero(N, _Size) ->
    N.

%% We aren't tracking UIDs anywhere to prevent collision, but since
%% we're spawning just a few processes, the odds of such a collision
%% are remarkably low.
-spec randuid() -> pos_integer().
randuid() ->
    rand:uniform(500000000).

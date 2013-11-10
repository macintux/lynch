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
-export([start_link/0, run/2, msg/4, crank/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          round=0,
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

crank() ->
    gen_server:call(?SERVER, step).

-spec msg(Dest :: msg_dest(), From :: uid(),
          Round :: round_id(), Message :: term()) -> 'ok'.
msg(Dest, From, Round, Message) ->
    gen_server:cast(?SERVER, {msg, Dest, From, Round, Message}).

run(Count, Module) ->
    gen_server:call(?SERVER, {run, Count, Module}).

run_procs(0, _Module, Procs) ->
    Procs;
run_procs(Count, Module, Procs) ->
    {Pid, _Ref} = spawn_monitor(Module, start, [{uid, Count}]),
    run_procs(Count-1, Module, [Pid|Procs]).

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
handle_call({run, Count, Module}, _From, #state{procs=Procs}) ->
    {reply, ok, #state{round=0,procs=run_procs(Count, Module, Procs)}};
handle_call(step, _From, #state{round=Round,procs=Procs}=State) ->
    lists:foreach(fun(X) -> X ! {step, {round, Round}} end, Procs),
    {reply, ok, State#state{round=Round+1}}.

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
handle_cast({msg, all, From, Round, Message}, #state{procs=Procs}=State) ->
    lists:foreach(fun(X) -> X ! {msg, From, Round, Message} end, Procs),
    {noreply, State};
handle_cast({msg, {uid, Uid}, From, Round, Message}, #state{procs=Procs}=State) ->
    lists:nth(map_uid(Uid, length(Procs)), Procs) ! {msg, From, Round, Message},
    {noreply, State};
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

%% @doc Will map an integer into the cluster UID space. Examples, given
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
-spec map_uid(N :: integer(), Size :: pos_integer()) -> pos_integer().
map_uid(N, Size) when N < 0 ->
    Size + (N rem Size);
map_uid(0, Size) ->
    Size;
map_uid(N, Size) ->
    check_for_zero(N rem Size, Size).

-spec check_for_zero(non_neg_integer(), pos_integer()) -> pos_integer().
check_for_zero(0, Size) ->
    Size;
check_for_zero(N, _Size) ->
    N.

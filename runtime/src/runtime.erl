%%%-------------------------------------------------------------------
%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2013 by John Daily <jd@epep.us>
%%%-------------------------------------------------------------------
-module(runtime).

-behaviour(gen_server).

%% API
-export([start_link/0, run/2, cluster/1, break/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
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


run(Count, Module) ->
    gen_server:call(?SERVER, {run, Count, Module}).

cluster(ring) ->
    gen_server:call(?SERVER, ring).

break() ->
    gen_server:call(?SERVER, break_pid).

run_procs(0, _Module, Procs) ->
    Procs;
run_procs(Count, Module, Procs) ->
    {Pid, _Ref} = spawn_monitor(Module, init, []),
    run_procs(Count-1, Module, [Pid|Procs]).

link_ring(Procs) ->
    link_ring(right, Procs, hd(Procs)),
    Reversed = lists:reverse(Procs),
    link_ring(left, Reversed, hd(Reversed)).

link_ring(Dir, [Last], Head) ->
    io:format("Linking ~p to the ~p to ~p~n", [Last, Dir, Head]),
    Last ! {Dir, Head};
link_ring(Dir, [H|T], Head) ->
    io:format("Linking ~p to the ~p to ~p~n", [H, Dir, hd(T)]),
    H ! {Dir, hd(T)},
    link_ring(Dir, T, Head).

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
    {reply, ok, #state{procs=run_procs(Count, Module, Procs)}};
handle_call(break_pid, _From, #state{procs=Procs}) ->
    {reply, ok, hd(Procs) ! foo};
handle_call(ring, _From, #state{procs=Procs}=State) ->
    link_ring(Procs),
    {reply, ok, State}.

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


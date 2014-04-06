%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2013 by John Daily <jd@epep.us>

-module(process).
-behavior(gen_server).
-include("process.hrl").

-export([step/2, prep/2, dump/1, message/4]).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Callbacks for algorithms to define when implementing the process
%% behavior
-callback init(Uid :: uid(), I :: i(), Extra :: list()) -> term().

-callback step(Round :: round_id(), State :: term()) ->
    {'messages', list(message()), NewState :: term()} |
    {'noreply', NewState :: term()} |
    {'stop', NewState :: term()}.

-callback handle_message(Message :: term(), From :: i(),
                         Round :: round_id(), State :: term()) ->
    {'continue'|'stop', NewState :: term()}.

-callback dump(State :: term()) -> iolist().


-record(state, {
          module,
          round=-1,
          i=-1,
          algorithm_state,
          stop=false,
          messages=[] %% Messages from prep to send during step
         }).
-type state() :: #state{}.

-spec prep(Pid :: pid(), Round :: non_neg_integer()) -> 'continue'|'stop'.
prep(Pid, Round) ->
    gen_server:call(Pid, {prep, {round, Round}}).

-spec step(Pid :: pid(), Round :: non_neg_integer()) -> list(tuple()).
step(Pid, Round) ->
    gen_server:call(Pid, {step, {round, Round}}).

-spec dump(Pid :: pid()) -> iolist().
dump(Pid) ->
    gen_server:call(Pid, dump).

-spec message(Pid :: pid(), From :: loc(),
              Round :: round_id(), Message :: term()) -> 'continue'|'stop'.
message(Pid, From, Round, Message) ->
    gen_server:call(Pid, {msg, From, Round, Message}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a process to implement the algorithm
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(AlgorithmModule :: atom(),
                 Uid :: uid(),
                 I :: i(),
                 Extra :: list()) -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link(AlgorithmModule, Uid, I, Extra) ->
    gen_server:start_link(?MODULE, [AlgorithmModule, Uid, I, Extra], []).

%% -spec init([AlgorithmModule :: atom(), Uid :: uid(),
%%             I :: pos_integer(), Extra :: list()]) -> {ok, state()}.
-spec init(list()) -> {ok, state()}.
init([AlgorithmModule, Uid, {i, I_int}=I, Extra]) ->
    AlgState = AlgorithmModule:init(Uid, I, Extra),
    {ok, #state{
            algorithm_state=AlgState,
            i=I_int,
            module=AlgorithmModule
           }
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: state()) ->
                                  {reply, term(), state()}.
handle_call({prep, {round, _Round}}, _From, #state{stop=true}=State) ->
    {reply, already_stopped, State};
handle_call({prep, {round, Round}}, _From,
            #state{algorithm_state=AlgState, module=Module, round=LastRound}=State)
  when Round - LastRound =:= 1 ->
    {ContinueOrStop, NewState} = handle_prep_response(
                                   Module:step({round, Round}, AlgState),
                                   State#state{round=Round}),
    {reply, ContinueOrStop, NewState};
handle_call({msg, From, {round, Round}, Msg}, _From,
            #state{algorithm_state=AlgState, module=Module, round=Round}=State) ->
    {ContinueOrStop, NewAlgState} =
        Module:handle_message(Msg, From, {round, Round}, AlgState),
    {reply, ContinueOrStop, State#state{algorithm_state=NewAlgState}};
handle_call(dump, _From, #state{module=Module,algorithm_state=AlgState}=State) ->
    {reply, Module:dump(AlgState), State};
handle_call({step, {round, Round}}, _From,
            #state{round=Round, i=I, messages=Messages}=State) ->
    %% Describe the "From" for this message in the same terms as the
    %% "To". If the sender describes a relative position (left or
    %% right with a number of servers) then pass the source for the
    %% message the same way.
    MsgTuples = lists:map(fun({{i, _D}=Dest, Message}) ->
                                  {Dest, {i, I},
                                   {round, Round}, Message};
                             ({{i, _D, left, Rel}=Dest, Message}) ->
                                  {Dest, {i, I, right, Rel},
                                   {round, Round}, Message};
                             ({{i, _D, right, Rel}=Dest, Message}) ->
                                  {Dest, {i, I, left, Rel},
                                   {round, Round}, Message}
                          end,
                          Messages),
    {reply, MsgTuples, State#state{messages=[],round=Round}}.

-spec handle_prep_response({'messages',
                            Messages :: list(message()),
                            AlgState :: term()} |
                           {'noreply', AlgState :: term()} |
                           {'stop', AlgState :: term()},
                           State :: state()) ->
                                  {'continue'|'stop', NewState :: state()}.
handle_prep_response({messages, Messages, AlgState}, State) ->
    {continue, State#state{algorithm_state=AlgState, messages=Messages}};
handle_prep_response({noreply, AlgState}, State) ->
    {continue, State#state{algorithm_state=AlgState}};
handle_prep_response({stop, AlgState}, State) ->
    {stop, State#state{algorithm_state=AlgState, stop=true}}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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

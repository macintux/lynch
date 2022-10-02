%% For a directed graph of synchronous nodes, Lynch differentiates
%% between UID and i, with the former a unique sortable value, and the
%% latter an integer representing the process's position in the graph.

%% The Erlang PID could serve as either, but it's helpful to treat i
%% as the process's position in runtime's list of processes, and UID
%% as a random value to make the algorithms work harder than if the
%% PIDs (a sequential set of values) were used.

%% Technically i can only be a positive integer, but during
%% messaging the process module can send zero or a negative integer
%% which the runtime will translate appropriately back into the UID
%% range.
-type ring_dir() :: 'left'|'right'.
-type i() :: {i, integer()} | {i, integer(), ring_dir(), integer()}.
-type uid() :: {uid, non_neg_integer()}.

-type loc() :: i() | 'all'.
-type message() :: {loc(), term()}.

-type round_id() :: {round, non_neg_integer()}.

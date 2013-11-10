%% Technically a UID can only be a positive integer, but during
%% messaging the process module can send zero or a negative integer
%% which the runtime will translate appropriately back into the UID
%% range.
-type uid() :: {uid, integer()}.

-type msg_dest() :: uid() | 'all'.
-type message() :: tuple(msg_dest(), term()).

-type round_id() :: {round, non_neg_integer()}.

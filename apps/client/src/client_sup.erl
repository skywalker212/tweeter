%%%-------------------------------------------------------------------
%% @doc tweeter client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(client_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(N) ->
    % store words to be used in tweets in an ETS table
    generator:save_words(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [N]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([N]) ->
    ChildSpecs = lists:map(
        fun(X) ->
            ID = integer_to_list(X),
            #{
                id => "client" ++ ID,
                start => {client, start_link, [ID, N]}
            }
        end,
        lists:seq(1, N)
    ),
    SupFlags = #{intensity => N},
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

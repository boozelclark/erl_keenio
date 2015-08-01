%%%-------------------------------------------------------------------
%% @doc erl_keenio top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erl_keenio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  ChildSpec = {erl_keenio_serv,
      {erl_keenio_serv, start_link, []},
      permanent,
      5000,
      worker,
      [erl_keenio_serv]},
    {ok, { {one_for_one, 5, 60}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================

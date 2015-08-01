%%%-------------------------------------------------------------------
%% @doc erl_keenio public API
%% @end
%%%-------------------------------------------------------------------

-module(keenio).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API calls
-export([add_event/2, add_events/1, report_periodically/2, cancel_periodic_report/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    erl_keenio_sup:start_link().

stop(_State) ->
    ok.

add_event(EventCollection,Event) ->
  erl_keenio_serv:add_event(EventCollection,Event).

add_events(Events) ->
  erl_keenio_serv:add_events(Events).

report_periodically(Seconds,Parameters) ->
  erl_keenio_serv:report_periodically(Seconds,Parameters).

cancel_periodic_report() ->
  erl_keenio_serv:cancel_periodic_report().

%%====================================================================
%% Internal functions
%%====================================================================

%%%-------------------------------------------------------------------
%% @doc erl_keenio public API
%% @end
%%%-------------------------------------------------------------------

-module(keenio).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, test_f/0]).

%% API calls
-export([add_event/2, add_events/1, report_periodically/2, report_periodically/3, cancel_periodic_report/0]).

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
  erl_keenio_serv:report_periodically(Seconds,Parameters,none).

report_periodically(Seconds,Parameters,Function) ->
  erl_keenio_serv:report_periodically(Seconds,Parameters,Function).

cancel_periodic_report() ->
  erl_keenio_serv:cancel_periodic_report().

%%====================================================================
%% Internal functions
%%====================================================================

%%Fn = fun() -> keenio:test_f() end

test_f() ->
  lager:info("CALLED!!!!!!!!!!!!!!!"),
  {[{test, [{[{node,node()} | erlang:memory()]}]}]}.
%%

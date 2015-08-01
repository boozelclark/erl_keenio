%%%-------------------------------------------------------------------
%% @doc erl_keenio_metrics.
%% @end
%%%-------------------------------------------------------------------


-module(erl_keenio_metrics).

-export([get_memory/0]).


% api

get_memory() ->
    erlang:memory().

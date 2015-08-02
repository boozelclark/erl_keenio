%%%-------------------------------------------------------------------
%% @doc erl_keenio_metrics.
%% @end
%%%-------------------------------------------------------------------
-module(erl_keenio_metrics).

-export([get_metrics/1]).

-define(STATISTICS, [
                     context_switches,
                     garbage_collection,
                     io,
                     reductions,
                     run_queue,
                     runtime,
                     wall_clock
                    ]).

%%-------------------------------------------------------------------
%% api
%%-------------------------------------------------------------------
get_metrics(all) ->
  get_metrics([vm_memory, vm_statistics, vm_system_info]);
get_metrics(Metrics) ->
  [{Metric, [{get_metric(Metric)}]} || Metric <- Metrics].

%%-------------------------------------------------------------------
%% internal
%%-------------------------------------------------------------------
get_metric(vm_memory) ->
  [node() | erlang:memory()];
get_metric(vm_statistics) ->
  [node() |
    [{Key, convert_statistics(Key, get_statistics(Key))} || Key <- ?STATISTICS]
  ].

get_statistics(Key) ->
    try erlang:statistics(Key) catch
                error:badarg->undefined
    end.

%% conversion functions for erlang:statistics(Key)
convert_statistics(context_switches, {ContextSwitches, 0}) ->
    ContextSwitches;
convert_statistics(garbage_collection, {NumberofGCs, WordsReclaimed, 0}) ->
    {[{number_of_gcs, NumberofGCs}, {words_reclaimed, WordsReclaimed}]};
convert_statistics(io, {Input, Output}) ->
    {[Input, Output]};
convert_statistics(reductions, {TotalReductions, ReductionsSinceLastCall}) ->
    {[{total_reductions, TotalReductions},
     {reductions_since_last_call, ReductionsSinceLastCall}]};
convert_statistics(runtime, {TotalRunTime, TimeSinceLastCall}) ->
    {[{total_run_time, TotalRunTime}, {time_since_last_call, TimeSinceLastCall}]};
convert_statistics(wall_clock, {TotalWallclockTime, WallclockTimeSinceLastCall}) ->
    {[{total_wall_clock_time, TotalWallclockTime},
     {wall_clock_time_since_last_call, WallclockTimeSinceLastCall}]};
convert_statistics(_, Value) ->
    Value.

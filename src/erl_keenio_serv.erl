%%%-------------------------------------------------------------------
%% @doc erl_keenio server.
%% @end
%%%-------------------------------------------------------------------


-module(erl_keenio_serv).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(BASE_URL(ProjectId),lists:append(["https://api.keen.io/3.0/projects/", ProjectId, "/events"])).
-define(EVENTS_URL(ProjectId, WriteKey),lists:append([?BASE_URL(ProjectId),"?api_key=",WriteKey])).
-define(EVENT_URL(ProjectId, EventCollection, WriteKey),lists:append([?BASE_URL(ProjectId),"/", EventCollection,"?api_key=",WriteKey])).


%% API
-export([start_link/0, add_event/2, add_events/1, send_event/2, send_events/1, report_periodically/2, cancel_periodic_report/0, vm_stats_report/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {tref=not_set}).


%% ---------------------------------------------------------------------------
%% API Function Definitions
%% ---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_event(EventCollection,Event) ->
  gen_server:cast(?SERVER,{event,EventCollection, Event}).

add_events(Events) ->
  gen_server:cast(?SERVER,{events, Events}).

report_periodically(Seconds,Parameters) ->
  gen_server:call(?SERVER,{periodic_report, add, {Seconds, Parameters}}).

cancel_periodic_report() ->
  gen_server:call(?SERVER,{periodic_report, remove}).

%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({periodic_report, add, {Seconds, Parameters}}, _From, S = #state{tref=TRef}) ->
  lager:info([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Periodic VM Stats reporting enabled every ~p second(s)",[Seconds]),
  case TRef of
    not_set ->
      {ok,NewTRef} = timer:send_interval(Seconds* 1000, ?SERVER,{periodic_report, Parameters}),
      ?SERVER ! {periodic_report, Parameters};
    _ ->
      timer:cancel(TRef),
      {ok,NewTRef}  = timer:send_interval(Seconds* 1000, ?SERVER,{periodic_report, Parameters}),
      ?SERVER!{periodic_report, Parameters}
  end,
  {reply, ok, S#state{ tref=NewTRef}};

handle_call({periodic_report, remove}, _From, S = #state{tref=TRef}) ->
  Result = timer:cancel(TRef),
  lager:info([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Removing Periodic Report ~p, ~p", [Result, TRef]),

  {reply, ok, S#state{tref=not_set}};

handle_call(Request, _From, State) ->
    lager:warning([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Unknown handle_call ~p",[Request]),
    {reply, ignored, State}.

handle_cast({event,EventCollection, Event}, State) ->
    lager:debug([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Sending Event ~p...",[Event]),
    monitor(process, spawn(?MODULE, send_event, [EventCollection, Event])),
    {noreply, State};

handle_cast({events, Events}, State) ->
    lager:debug([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Sending Events ~p...",[Events]),
    monitor(process, spawn(?MODULE, send_events, [Events])),
    {noreply, State};

handle_cast(Msg, State) ->
  lager:warning([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Unknown handle_cast ~p",[Msg]),
    {noreply, State}.

handle_info({periodic_report, Parameters}, State) ->
    lager:debug([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Periodic Report ~p",[Parameters]),
    monitor(process, spawn(?MODULE, vm_stats_report, [Parameters])),
    {noreply, State};

handle_info({'DOWN',_Ref,_,_PID,normal}, State) ->
  %Normal, nothing to see here
  {noreply, State};

handle_info({'DOWN',_Ref,_,_PID,Reason}, State) ->
  lager:info([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Something went wrong ~p",[Reason]),
  {noreply, State};

handle_info(Info, State) ->
  lager:warning([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Unknown handle_info ~p",[Info]),
    {noreply, State}.

terminate(Reason, _State) ->
  lager:info([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - terminate ~p",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------------------
vm_stats_report(Parameters) ->
  case Parameters of
    all ->
      Report = [{node,node()} | erlang:memory()],
      send_event(vm_memory,Report);
    [memory] ->
      Report = [{node,node()} | erlang:memory()],
      send_event(vm_memory,Report);
    _ ->
      lager:erros([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Unknown Report Parameters ~p",[Parameters])
  end.

send_event(EventCollection,Event) ->
  if is_list(Event) ->
      WholeEvent = {Event} ;
    true ->
      WholeEvent = {[Event]}
    end,

  try
    EventCollectionList = atom_to_list(EventCollection),
    Body = jiffy:encode(WholeEvent),
    post_event(EventCollectionList, Body)
  catch
    Error:Reason ->
    lager:error([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Could not parse your event to JSON due to ~p",[Reason]),
    {Error, Reason}
  end.

send_events(Events) ->
  try
    Body = jiffy:encode(Events),
    post_events(Body)
  catch
    Error:Reason ->
    lager:error([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Could not parse your events to JSON due to ~p",[Reason]),
    {Error, Reason}
  end.

post_event(EventCollection, Body) ->
  {ok,ProjectId} = application:get_env(erl_keenio,project_id),
  {ok,WriteKey} = application:get_env(erl_keenio,write_key),
  URL = ?EVENT_URL(ProjectId,EventCollection, WriteKey),
  post(URL, Body).

post_events(Body) ->
  {ok,ProjectId} = application:get_env(erl_keenio,project_id),
  {ok,WriteKey} = application:get_env(erl_keenio,write_key),
  URL = ?EVENTS_URL(ProjectId, WriteKey),
  post(URL,Body).

post(URL, Body) ->
    Method = post,
    Header = [],
    Type = "application/json",
    HTTPOptions = [],
    Options = [],
    R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
    lager:debug([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - post_event Raw Result ~p",[R]),
    {ok, {{"HTTP/1.1",ReturnCode, _State}, _Head, RBody}} = R,
    case ReturnCode of
      N when N >= 200, N < 300 ->
        ok;
      _ ->
        lager:error([{app,erl_keenio},{module,?MODULE}],"erl_keenio_server - Something went wrong ~p",[RBody])
    end.

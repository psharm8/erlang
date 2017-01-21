%%%-------------------------------------------------------------------
%%% @author Puneet Sharma
%%% @copyright (C) 2016, Stevens Institute of Technology
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2016 15:35
%%%-------------------------------------------------------------------
-module(watcher).
-author(["Puneet Sharma", "Kunal Dhaimade"]).

%% API
-export([setup/0]).

setup() ->
  io:format("Use watcher!shutdown. to stop.~n"),
  {ok, [N]} = io:fread("enter number of sensors> ", "~d"),
  if N =< 1 ->
    io:fwrite("setup: range must be at least 2~n", []);
    true ->
      NumWatchers = 1 + ((N - 1) div 10),
      L = spawn(fun() -> setupLoop(N, NumWatchers) end),
      register(watcher, L)
  end, ok.

setupLoop(N, NumWatchers) ->
  W = startWatcher(N, NumWatchers, 1, []),
  waitToQuit(W).

waitToQuit(W) ->
  receive
    shutdown ->
      lists:foreach(fun({PID, _}) -> PID ! shutdown end, W);
    _ -> waitToQuit(W)
  end.

startWatcher(NumSensors, NumWatchers, WatcherNumber, Watchers) when WatcherNumber =< NumWatchers ->
  SensorStart = 10 * (WatcherNumber - 1),
  SensorEnd = min(SensorStart + 9, NumSensors - 1),
  WatcherPID = spawn_link(fun() -> startAndWatch(WatcherNumber, SensorStart, SensorEnd) end),
  startWatcher(NumSensors, NumWatchers, WatcherNumber + 1, Watchers ++ [{WatcherPID, WatcherNumber}]);
startWatcher(_, _, _, Watchers) ->
  Watchers.

startAndWatch(WatcherNumber, SensorStart, SensorEnd) ->
  io:format("[Watcher ~w][Sensor Range] ~w ~n", [WatcherNumber, [SensorStart, SensorEnd]]),
  process_flag(trap_exit, true),
  P = startSensor(SensorStart, SensorEnd, []),
  io:format("[Watcher ~w][Sensor Up] ~w : ~w~n", [WatcherNumber, {WatcherNumber, self()}, P]),
  watch(WatcherNumber, P).

watch(WatcherNumber, P) ->
  receive
    {SID, M} ->
      io:format("[Watcher ~w][Reading] Sensor ~w : Value ~w~n", [WatcherNumber, SID, M]),
      watch(WatcherNumber, P);
    {'DOWN', _Ref, process, SPid, Reason} ->
      {SPid, SID} = lists:keyfind(SPid, 1, P),
      io:format("[Watcher ~w][Sensor Down] Sensor ~w : ~w~n", [WatcherNumber, SID, Reason]),
      {Pid, _} = spawn_monitor(sensor, start, [self(), SID]),
      P1 = lists:keyreplace(SPid, 1, P, {Pid, SID}),
      io:format("[Watcher ~w][Sensor Up] ~w : ~w~n", [WatcherNumber, {WatcherNumber, self()}, P1]),
      watch(WatcherNumber, P1);
    shutdown ->
      lists:foreach(fun({SPid, _}) -> exit(SPid, normal) end, P),
      io:format("[Watcher ~w][Shutdown] ~w~n", [WatcherNumber, {WatcherNumber, self()}]),
      exit(normal)
  end.

startSensor(SID, TSensor, P) when SID =< TSensor ->
  {Pid, _} = spawn_monitor(sensor, start, [self(), SID]),
  startSensor(SID + 1, TSensor, P ++ [{Pid, SID}]);
startSensor(_, _, P) -> P.
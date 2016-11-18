%%%-------------------------------------------------------------------
%%% @author Puneet Sharma
%%% @copyright (C) 2016, Stevens Institute of Technology
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2016 18:29
%%%-------------------------------------------------------------------
-module(sensor).
-author("Puneet Sharma").

%% API
-export([start/2]).

start(Wid, ID)->
  measure(Wid, ID).

measure(Wid,ID) ->
  Measurement = rand:uniform(11),
  report(Wid, ID, Measurement),
  Sleep_time = rand:uniform(10000) ,
  timer:sleep(Sleep_time),
  measure(Wid, ID).

report(Wid, ID, M) when M < 11 ->
  Wid ! {ID, M};
report(_Wid, _ID, _M) ->
  exit(anomalous_reading).
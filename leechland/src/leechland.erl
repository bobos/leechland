-module(leechland).

-export([start/0,
         stop/0]).

start() ->
    application:start(leechland).

stop() ->
    application:stop(leechland).

-module(leechland_regression).

-export([start/0]).

start() ->
    TestFiles = string:tokens(os:cmd("find \\./ -name \\*_test.erl"),"\n"),
    TestList = 
    [list_to_atom(filename:rootname(filename:basename(F)))||F<-TestFiles],
    io:format("test suites are:~p~n",[TestList]),
    start_int(TestList),
    io:format("REGRESSION FINISHED~n").

start_int([]) ->
    ok;
start_int([M|Rest]) ->
    ok = eunit:test(M),
    start_int(Rest).

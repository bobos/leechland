-module(biglist_test_client).
-export([start/0]).

start() ->
    Task = gen_list(10000,[]),
    {ok, Master} = leechland_client:get_master(),
    leechland_client:send(Master, Task,{callback_info, biglist_test, biglist_test, "/home/eboosun/frodo/src/leechland/src/eunit/src/ebin"}).

gen_list(0, Acc) ->
    Acc;
gen_list(N, Acc) when (N rem 2) == 0 ->
    gen_list(N-1, [{N,"tc_00001_normal_should_be_testcase_tittle_length",ok}|Acc]);
gen_list(N, Acc) ->
    gen_list(N-1, [{N,"tc_00001_normal_should_be_testcase_tittle_length",nok}|Acc]).


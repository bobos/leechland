-module(test_master).

-export([start_master/0,
         start/0]).

-include("leechland.hrl").
-include("leechland_test.hrl").

start_master() ->
    slave:start_link(?HOST, list_to_atom(?TEST_NODE), erl_sys_args()).

start() ->
    register(?MASTERPROC, self()),
    write_to_deployfile_int(),
    main_loop().

main_loop() ->
    receive 
        {is_alive, {Pid, _Node}} ->
            Pid ! {alive, state},
            main_loop();

        {client_request, {{Client,_}, _, [normal_task|Ret], _C}} ->
            file:write_file("/home/eboosun/tmp", list_to_binary("here")),
            Tasks = convert_to_tasks([normal_task|Ret], 1, []),
            Last = lists:last(Tasks),
            lists:foreach(fun(T) -> Client ! {result, T} end, 
                          Tasks -- [Last]),
            Client ! {done, Last},
            main_loop();

        {client_request, {{Client,_}, _, [disorder_task|Ret], _C}} ->
            file:write_file("/home/eboosun/tmp", list_to_binary("there")),
            Tasks = convert_to_tasks([disorder_task|Ret], 1, []),
            Last = lists:last(Tasks),
            Client ! {done, Last},
            lists:foreach(fun(T) -> Client ! {result, T} end, 
                          Tasks -- [Last]),
            main_loop();

        {client_request, {{Client,_}, _, [timeout_task|Ret], _C}} ->
            Tasks = convert_to_tasks([timeout_task|Ret], 1, []),
            Last = lists:last(Tasks),
            lists:foreach(fun(T) -> Client ! {result, T} end, 
                          Tasks -- [Last]),
            % make it time out
            timer:sleep(1000),
            Client ! {done, Last},
            main_loop();

        {check_group, _, _, Pid} ->
            Pid ! ok,
            main_loop()
    end.

erl_sys_args() ->
  lists:append(["-rsh ssh -noshell -setcookie ",
                atom_to_list(erlang:get_cookie()),
                " -s ", ?TEST_NODE, " start -slavename slave", 
                " -logs ./ -pa ./ebin", " -slave_group ", atom_to_list(?GENSLAVE)]).

convert_to_tasks([], _N, Acc) ->
    Acc;
convert_to_tasks([R|Res], N, Acc) ->
    convert_to_tasks(Res, N+1, [{N, R}|Acc]).


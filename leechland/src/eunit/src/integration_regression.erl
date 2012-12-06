-module(integration_regression).

%% ALL functions in this module are exported!
-export([start/0,
         client_add_g1_no_group/1,
         client_mul_timeout_g1/1]).

%% test cases
-export([pressure_test/0,
         normal_with_application_changed_test/0,
         timeout_for_mult_test/0,
         remove_server_test/0,
         add_server_test/0,
         delete_group_test/0]).

-include("leechland.hrl").
-define(TESTCASES, [timeout_for_mult_test,
                    normal_with_application_changed_test,
                    timeout_for_mult_test,
                    remove_server_test,
                    delete_group_test,
                    add_server_test,
                    pressure_test]).

-define(G1, {custom_group1, "group1"}).
-define(G2, {custom_group2, "group2"}).
-define(HOST, net_adm:localhost()).

start() ->
    %% wait for test_master up
    timer:sleep(5000),
    io:format("Testcase found:~n"),
    lists:foreach(fun(Test)-> io:format("~p~n", [Test]) end, ?TESTCASES),
    %% preparation
    %% create two group first
    {GroupName, Pincode} = ?G1,
    ok = leechland_client:create_group(GroupName, Pincode, ?DEPLOYFILE),
    {GroupName1, Pincode1} = ?G2,
    ok = leechland_client:create_group(GroupName1, Pincode1, ?DEPLOYFILE),
    %% add nodes to group, 2 each
    Ret = leechland_client:add_server(GroupName,?HOST,4,?DEPLOYFILE),
    timer:sleep(5000),
    Ret1 = leechland_client:add_server(GroupName1,?HOST,6,?DEPLOYFILE),
    4 = length([R||{_,R}<-Ret, R==ok]),
    6 = length([R||{_,R}<-Ret1, R==ok]),

    lists:foreach(fun(Test)-> io:format("executing case:~p...~n", [Test]), 
                              ?MODULE:Test() end, ?TESTCASES),
    io:format("All ~p testcases passed!~n", [length(?TESTCASES)]),
    %% delete group 
    ok = delete_group(GroupName, Pincode),
    ok = delete_group(GroupName1, Pincode1).


%%%%%%%%%%%%%%% cases %%%%%%%%%%%%%%%%%
pressure_test() ->
    Pids1 = spawn_tests(client_add_gen, 100),
    Pids2 = spawn_tests(client_add_g1, 100),
    Pids3 = spawn_tests(client_add_g2, 100),
    ok = check_result(Pids1++Pids2++Pids3).
    
normal_with_application_changed_test() ->
    %% this will trigger an application reload in slave
    %% since there are 2 slaves each group, all of them 
    %% load the application for client_add, once client_mul 
    %% task comes then app reload is triggered
    Parent = self(),
    Pid = spawn_link(fun() -> client_add_gen(Parent) end),
    Pid2 = spawn_link(fun() -> client_mul_gen(Parent) end),
    Pid3 = spawn_link(fun() -> client_add_g1(Parent) end),
    Pid4 = spawn_link(fun() -> client_mul_g1(Parent) end),
    Pid5 = spawn_link(fun() -> client_add_g2(Parent) end),
    Pid6 = spawn_link(fun() -> client_mul_g2(Parent) end),
    ok = check_result([Pid,Pid2,Pid3,Pid4,Pid5,Pid6]).

timeout_for_mult_test() ->
    %% spawn 10 clients for add operation to over-fill the queue for each group
    %% then the following 1 client for mult will timeout since there is 
    %% a 4s timer for mult operation, and each client no matter for add or mult
    %% will take about 1s to finish and we only have 2 slaves running each group 
    %% so just do the math
    %% test group1
    Pids = spawn_tests(client_add_g1, 10),
    Pids1 = spawn_tests(client_mul_timeout_g1, 4) ++ Pids,
    NewPids = spawn_tests(client_add_gen, 10) ++ Pids1,
    ok = check_result(NewPids),
    %% let's check if all tasks are aborted
    {alive, State} = is_alive_check(),
    %% no slave should be in running state
    [] = [S||S<-State#main_state.slave_groups, SI<-S#slave_group.slaves, 
          SI#slave_info.state == running],
    %% no tasks should be in the queue
    [] = [Q||Q<-State#main_state.task_queue, Q#leech_tasks.tasks == []],
    [] = [Q||Q<-State#main_state.ongoing_queue, Q#ongoing_tasks.tasks == []].

remove_server_test() ->
    %% first we pile up the tasks in the master's queue
    Pids = spawn_tests(client_add_gen, 10),
    Pids1 = spawn_tests(client_add_g1, 20),
    Pids2 = spawn_tests(client_add_g2, 10),
    %% then we remove all slaves for group1
    {G, P} = ?G1,
    ok = remove_server(G, P),
    %% check if all slaves are gone
    timer:sleep(2000),
    {alive, State} = is_alive_check(),
    [] = (lists:keyfind(G, 2, State#main_state.slave_groups))#slave_group.slaves,
    %% then we add 2 slaves back
    Ret = add_server(G,4),
    4 = length([R||{_,R}<-Ret, R==ok]),
    timer:sleep(2000),
    {alive, State1} = is_alive_check(),
    2 = length((lists:keyfind(G, 2, State1#main_state.slave_groups))#slave_group.slaves),
    ok = check_result(Pids++Pids1++Pids2).

delete_group_test() ->
    %% first we pile up the tasks in the master's queue
    Pids = spawn_tests(client_add_gen, 10),
    Pids1 = spawn_tests(client_add_g1_no_group, 20),
    Pids2 = spawn_tests(client_add_g2, 10),
    %% then we delete group1
    {G, P} = ?G1,
    ok = delete_group(G, P),
    %% check if group is deleted
    timer:sleep(2000),
    {alive, State} = is_alive_check(),
    false = lists:keyfind(G, 2, State#main_state.slave_groups),
    false = lists:keyfind(G, 2, State#main_state.ongoing_queue),
    false = lists:keyfind(G, 2, State#main_state.task_queue),
    %% then we create it back with slave
    ok = create_group(G,P),
    %% we add 2 slaves back
    Ret = add_server(G,4),
    4 = length([R||{_,R}<-Ret, R==ok]),
    timer:sleep(2000),
    {alive, State1} = is_alive_check(),
    2 = length((lists:keyfind(G, 2, State1#main_state.slave_groups))#slave_group.slaves),
    ok = check_result(Pids++Pids1++Pids2).

add_server_test() ->
    Pids = spawn_tests(client_add_gen, 10),
    Pids1 = spawn_tests(client_add_g1, 20),
    Pids2 = spawn_tests(client_add_g2, 10),
    %% then we add 2 more slaves for group1
    {G, _P} = ?G1,
    Ret = add_server(G, 8),
    8 = length([R||{_,R}<-Ret, R==ok]),
    timer:sleep(2000),
    {alive, State} = is_alive_check(),
    %% now it should have 4 slaves
    4 = length((lists:keyfind(G, 2, State#main_state.slave_groups))#slave_group.slaves),
    ok = check_result(Pids++Pids1++Pids2).

%%%%%%  Help function %%%%
is_alive_check() ->
    Parent = self(),
    spawn_link(fun() -> is_alive_int(Parent) end),
    wait_loop1().

wait_loop1() ->
    receive
        {alive, State} ->
            {alive, State};
        master_unreachable ->
            master_unreachable;
        Other ->
            self() ! Other,
            wait_loop1()
    end.

add_server(G, N) ->
    Parent = self(),
    spawn_link(fun() -> add_server_int(G, N, Parent) end),
    wait_loop2().

wait_loop2() ->
    receive
        [{Node,State}|Rest] ->
            [{Node,State}|Rest];
        Ret when Ret == group_not_found;
                 Ret == master_timeout ->
            Ret;
        Other ->
            self() ! Other,
            wait_loop2()
    end.

remove_server(G, P) ->
    Parent = self(),
    spawn_link(fun() -> remove_server_int(G, P, Parent) end),
    wait_loop3().

wait_loop3() ->
    receive
        Ret when Ret == ok;
                 Ret == no_such_group;
                 Ret == wrong_pincode;
                 Ret == master_timeout ->
            Ret;
        Other ->
            self() ! Other,
            wait_loop3()
    end.

create_group(G, P) ->
    Parent = self(),
    spawn_link(fun() -> create_group_int(G, P, Parent) end),
    wait_loop4().

wait_loop4() ->
    receive
        Ret when Ret == ok;
                 Ret == already_exist;
                 Ret == master_timeout ->
            Ret;
        Other ->
            self() ! Other,
            wait_loop4()
    end.

delete_group(G, P) ->
    Parent = self(),
    spawn_link(fun() -> delete_group_int(G, P, Parent) end),
    wait_loop5().

wait_loop5() ->
    receive
        Ret when Ret == ok;
                 Ret == no_such_group;
                 Ret == wrong_pincode;
                 Ret == master_timeout ->
            Ret;
        Other ->
            self() ! Other,
            wait_loop5()
    end.

is_alive_int(Parent) ->
    Parent ! leechland_client:is_alive().
add_server_int(G, N, Parent) ->
    Parent ! leechland_client:add_server(G, ?HOST, N, ?DEPLOYFILE).
remove_server_int(G, P, Parent) ->
    Parent ! leechland_client:remove_server(G, P, ?HOST, ?DEPLOYFILE).
create_group_int(G, P, Parent) ->
    Parent ! leechland_client:create_group(G, P, ?DEPLOYFILE).
delete_group_int(G, P, Parent) ->
    Parent ! leechland_client:delete_group(G, P, ?DEPLOYFILE).

spawn_tests(Fun, Number) ->
    Parent = self(),
    spawn_test(Fun, Number, Parent, []).

spawn_test(_F, 0, _P, Acc) ->
    Acc;
spawn_test(F, N, P, Acc) ->
    spawn_test(F,N-1,P,[spawn_link(fun() -> ?MODULE:F(P) end)|Acc]).

check_result(Pids) ->
    Pred = fun({Result, _, _, _}) -> Result == false end,
    Ret = loop(Pids,[]),
    case lists:filter(Pred, Ret) of
        [] ->  ok;
        UnmatchList -> 
            lists:foreach(
                fun({_ ,Expect, Actual, Line})->
                    io:format("unmatch at ~p ~p:~nexpected:[~p]~nactual:[~p]~n",
                        [?MODULE, Line, Expect, Actual]) end, UnmatchList),
            nok
    end.

loop([], Res) ->
    Res;
loop(Pids, Res) ->
    receive 
        {Pid, Result, Expect, Actual, Line} ->
            Ret = [{Result, Expect, Actual, Line}|Res],
            loop(Pids -- [Pid], Ret)
    end.

%%%%%%%    Clients  %%%%%%%
client_add_gen(P) ->
    Match = lists:sort([4+5, 6+7]),
    {ok, Ret} = test_adder_client:add(?GENSLAVE, "", [{4,5},{6,7}]),
    P ! {self(), Match == lists:sort(Ret), Match, lists:sort(Ret), ?LINE}.

client_add_g1(P) ->
    {G,Pwd} = ?G1,
    Match = lists:sort([8+9, 9+10]),
    {ok, Ret} = test_adder_client:add(G, Pwd, [{8,9},{9,10}]),
    P ! {self(), Match == lists:sort(Ret), Match, lists:sort(Ret), ?LINE}.

client_add_g1_no_group(P) ->
    {G,Pwd} = ?G1,
    Match = lists:sort([8+9, 9+10]),
    Result = test_adder_client:add(G, Pwd, [{8,9},{9,10}]), 
    Ret = 
    case Result of
        {ok, Ret1} ->
            case lists:sort(Ret1) of
                Match ->
                    true;
                _ ->
                    false
            end;
        {communication_lost, {[], [{8,9},{9,10}]}} ->
            true;
        {error,no_such_group} ->
            true;
        _ ->
            false
    end,
    P ! {self(), Ret, not_important, Result, ?LINE}.

client_add_g2(P) ->
    {G,Pwd} = ?G2,
    Match = lists:sort([3+4, 5+6]),
    {ok, Ret} = test_adder_client:add(G, Pwd, [{3,4},{5,6}]),
    P ! {self(), Match == lists:sort(Ret), Match, lists:sort(Ret), ?LINE}.

client_mul_gen(P) ->
    Match = lists:sort([4*5, 6*7]),
    {ok, Ret} = test_multer_client:mult(?GENSLAVE, "", [{4,5},{6,7}]),
    P ! {self(), Match == lists:sort(Ret), Match, lists:sort(Ret), ?LINE}.

client_mul_g1(P) ->
    {G,Pwd} = ?G1,
    Match = lists:sort([4*5, 6*7]),
    {ok, Ret} = test_multer_client:mult(G, Pwd, [{4,5},{6,7}]),
    P ! {self(), Match == lists:sort(Ret), Match, lists:sort(Ret), ?LINE}.

client_mul_g2(P) ->
    {G,Pwd} = ?G2,
    Match = lists:sort([4*5, 6*7]),
    {ok, Ret} = test_multer_client:mult(G, Pwd, [{4,5},{6,7}]),
    P ! {self(), Match == lists:sort(Ret), Match, lists:sort(Ret), ?LINE}.

client_mul_timeout_g1(P) ->
    {G,Pwd} = ?G1,
    Match = {error, timeout},
    Ret = test_multer_client:mult(G, Pwd, [{4,5},{6,7}]),
    P ! {self(), Match == Ret, Match, Ret, ?LINE}.


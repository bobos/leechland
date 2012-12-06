-module(leechland_master_test).

-include_lib("eunit/include/eunit.hrl").
-include("leechland.hrl").
-include("leechland_test.hrl").

-define(ONGNT1, #ongoing_task{task=#leech_task{client={svr,svr},taskId=1,data=t1}, 
                              slave=node1}).
-define(ONGNT2, #ongoing_task{task=#leech_task{client={svr1,svr1},taskId=1,data=t1}, 
                              slave=node2}). 
-define(ONGNT3, #ongoing_task{task=#leech_task{client={svr,svr},taskId=2,data=t2},
                              slave=node3}). 

-define(ONGNT4, #ongoing_task{task=#leech_task{client={svr2,svr2},taskId=1,data=t1}, 
                              slave=node4}).
-define(ONGNT5, #ongoing_task{task=#leech_task{client={svr3,svr3},taskId=1,data=t1}, 
                              slave=node5}). 
-define(ONGNT6, #ongoing_task{task=#leech_task{client={svr2,svr2},taskId=2,data=t2},
                              slave=node6}).

-define(GTASK1,#ongoing_tasks{group_type=?GENSLAVE,
                              tasks=[?ONGNT1,?ONGNT2,?ONGNT3]}).
-define(TASKQ1,#leech_tasks{group_type=?GENSLAVE,
                            tasks=[#leech_task{client={svr,svr},taskId=3,data=t1},
                                   #leech_task{client={svr1,svr1},taskId=2,data=t1},
                                   #leech_task{client={svr1,svr1},taskId=3,data=t1}]}).

-define(GTASK2,#ongoing_tasks{group_type=group1,
                              tasks=[?ONGNT4,?ONGNT5,?ONGNT6]}).
-define(TASKQ2,#leech_tasks{group_type=group1,
                            tasks=[#leech_task{client={svr2,svr2},taskId=3,data=t1},
                                   #leech_task{client={svr3,svr3},taskId=2,data=t1},
                                   #leech_task{client={svr4,svr4},taskId=1,data=t1}]}).

-define(GROUP1, #slave_group{group_type=?GENSLAVE, 
                             pincode= <<>>, 
                             slaves=[#slave_info{node=node1,state=running},
                                     #slave_info{node=node2,state=running},
                                     #slave_info{node=node3,state=running},
                                     #slave_info{node=node0,state=idle}]}).
                                   
-define(PWD, "letmein").
-define(GROUP2, #slave_group{group_type=group1, 
                             pincode=term_to_binary(?PWD), 
                             slaves=[#slave_info{node=node4,state=running},
                                     #slave_info{node=node5,state=running},
                                     #slave_info{node=node6,state=running},
                                     #slave_info{node=node7,state=idle}]}).

-define(ONGOING, [?GTASK1,?GTASK2]).
%==============================================================================
% cases
%==============================================================================
create_tasks_test() ->
    Tasks = [{{svr,svr}, cb, t1},{{svr,svr}, cb, t2},{{svr,svr}, cb, t3},{{svr,svr}, cb, t4}],
    Match = [#leech_task{client={svr,svr},taskId=1,callback_info=cb,data=t1},
             #leech_task{client={svr,svr},taskId=2,callback_info=cb,data=t2},
             #leech_task{client={svr,svr},taskId=3,callback_info=cb,data=t3},
             #leech_task{client={svr,svr},taskId=4,callback_info=cb,data=t4}],
    ?assertEqual(Match, leechland_master:create_leech_tasks(Tasks,1,[])).

%% remove ongoing tasks for one slave
remove_ongoing_task1_test() ->
    MainState = #main_state{ongoing_queue=?ONGOING},
    NewTasks = ?GTASK2#ongoing_tasks{tasks=[?ONGNT4,?ONGNT6]},
    Slave = #slave{group_type=group1, node=node5},
    Match = {[#leech_task{client={svr3,svr3},taskId=1,data=t1}],
              MainState#main_state{ongoing_queue=[?GTASK1,NewTasks]}},
    ?assertEqual(Match, leechland_master:remove_ongoing_task(Slave,MainState)).

%% remove ongoing tasks for one slave with wrong group name
remove_ongoing_task2_test() ->
    MainState = #main_state{ongoing_queue=?ONGOING},
    Slave = #slave{group_type=group_not_exist, node=node5},
    Match = {[], MainState},
    ?assertEqual(Match, leechland_master:remove_ongoing_task(Slave,MainState)).

%% remove ongoing tasks for non-existing slave 
remove_ongoing_task3_test() ->
    MainState = #main_state{ongoing_queue=?ONGOING},
    Slave = #slave{group_type=group1, node=nonode},
    Match = {[], MainState},
    ?assertEqual(Match, leechland_master:remove_ongoing_task(Slave,MainState)).

%% remove ongoing tasks for one task 
remove_ongoing_task4_test() ->
    MainState = #main_state{ongoing_queue=?ONGOING},
    Slave = #slave{group_type=?GENSLAVE, node=node2},
    Match = MainState#main_state{ongoing_queue=
            [?GTASK1#ongoing_tasks{tasks=[?ONGNT1,?ONGNT3]},?GTASK2]},
    Task = {{svr1,svr1}, 1, Slave},
    ?assertEqual(Match, leechland_master:remove_ongoing_task(Task,MainState)).

%% remove ongoing tasks for non-existing slave 
remove_ongoing_task5_test() ->
    MainState = #main_state{ongoing_queue=?ONGOING},
    Slave = #slave{group_type=?GENSLAVE, node=nonode},
    Task = {{svr1,svr1}, 1, Slave},
    ?assertEqual(MainState, leechland_master:remove_ongoing_task(Task,MainState)).

%% remove ongoing tasks for non-existing group 
remove_ongoing_task6_test() ->
    MainState = #main_state{ongoing_queue=?ONGOING},
    Slave = #slave{group_type=no_such_group, node=node2},
    Task = {{svr1,svr1}, 1, Slave},
    ?assertEqual(MainState, leechland_master:remove_ongoing_task(Task,MainState)).

%% abort task for group1
abort_task_test() ->
    P = self(),
    TASKQ1=#leech_tasks{group_type=?GENSLAVE,
        tasks=[#leech_task{client={P,svr},taskId=3,data=t1},
               #leech_task{client={P,svr1},taskId=2,data=t1},
               #leech_task{client={P,svr1},taskId=3,data=t1}]},
    TASKQ2=#leech_tasks{group_type=group1,
         tasks=[#leech_task{client={P,svr2},taskId=3,data=t1},
                #leech_task{client={P,svr3},taskId=2,data=t1},
                #leech_task{client={P,svr4},taskId=1,data=t1}]},
    TaskQueue = [TASKQ1,TASKQ2],

    ONGNT1=#ongoing_task{task=#leech_task{client={P,svr},taskId=1,data=t1}, 
                         slave=node1},
    ONGNT2=#ongoing_task{task=#leech_task{client={P,svr1},taskId=1,data=t1}, 
                         slave=node2},
    ONGNT3=#ongoing_task{task=#leech_task{client={P,svr},taskId=2,data=t2},
                         slave=node3},
    ONGNT4=#ongoing_task{task=#leech_task{client={P,svr2},taskId=1,data=t1}, 
                         slave=node4},
    ONGNT5=#ongoing_task{task=#leech_task{client={P,svr3},taskId=1,data=t1}, 
                         slave=node5}, 
    ONGNT6=#ongoing_task{task=#leech_task{client={P,svr2},taskId=2,data=t2},
                         slave=node6},
    GTASK1=#ongoing_tasks{group_type=?GENSLAVE,
                          tasks=[ONGNT1,ONGNT2,ONGNT3]},
    GTASK2=#ongoing_tasks{group_type=group1,
                          tasks=[ONGNT4,ONGNT5,ONGNT6]},

    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=[GTASK1,GTASK2],
                            slave_groups=[?GROUP1,?GROUP2]},

    NewTaskQ = TASKQ2#leech_tasks{
                       tasks=[#leech_task{client={P,svr3},taskId=2,data=t1},
                              #leech_task{client={P,svr4},taskId=1,data=t1}]},
    NewOngnQ = GTASK2#ongoing_tasks{tasks=[ONGNT5]},
    Match = #main_state{task_queue=[TASKQ1,NewTaskQ],
                        ongoing_queue=[GTASK1,NewOngnQ],
                        slave_groups=[?GROUP1,?GROUP2]},
    ?assertEqual(Match, leechland_master:abort_tasks(group1, term_to_binary(?PWD), 
                 {P,svr2}, MainState)).

%% abort task for gen_slave group with wrong pwd
abort_task1_test() ->
    P = self(),
    TASKQ1=#leech_tasks{group_type=?GENSLAVE,
        tasks=[#leech_task{client={P,svr},taskId=3,data=t1},
               #leech_task{client={P,svr1},taskId=2,data=t1},
               #leech_task{client={P,svr1},taskId=3,data=t1}]},
    TASKQ2=#leech_tasks{group_type=group1,
         tasks=[#leech_task{client={P,svr2},taskId=3,data=t1},
                #leech_task{client={P,svr3},taskId=2,data=t1},
                #leech_task{client={P,svr4},taskId=1,data=t1}]},
    TaskQueue = [TASKQ1,TASKQ2],

    ONGNT1=#ongoing_task{task=#leech_task{client={P,svr},taskId=1,data=t1}, 
                         slave=node1},
    ONGNT2=#ongoing_task{task=#leech_task{client={P,svr1},taskId=1,data=t1}, 
                         slave=node2},
    ONGNT3=#ongoing_task{task=#leech_task{client={P,svr},taskId=2,data=t2},
                         slave=node3},
    ONGNT4=#ongoing_task{task=#leech_task{client={P,svr2},taskId=1,data=t1}, 
                         slave=node4},
    ONGNT5=#ongoing_task{task=#leech_task{client={P,svr3},taskId=1,data=t1}, 
                         slave=node5}, 
    ONGNT6=#ongoing_task{task=#leech_task{client={P,svr2},taskId=2,data=t2},
                         slave=node6},
    GTASK1=#ongoing_tasks{group_type=?GENSLAVE,
                          tasks=[ONGNT1,ONGNT2,ONGNT3]},
    GTASK2=#ongoing_tasks{group_type=group1,
                          tasks=[ONGNT4,ONGNT5,ONGNT6]},

    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=[GTASK1, GTASK2],
                            slave_groups=[?GROUP1,?GROUP2]},
    NewTaskQ = TASKQ1#leech_tasks{
                            tasks=[#leech_task{client={P,svr},taskId=3,data=t1}]},
    NewOngnQ = GTASK1#ongoing_tasks{tasks=[ONGNT1,ONGNT3]},
    Match = #main_state{task_queue=[NewTaskQ,TASKQ2],
                        ongoing_queue=[NewOngnQ,GTASK2],
                        slave_groups=[?GROUP1,?GROUP2]},
    ?assertEqual(Match, leechland_master:abort_tasks(?GENSLAVE,term_to_binary(?PWD),
                 {P,svr1},MainState)).

%% abort task for non-existing group
abort_task2_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=?ONGOING,
                            slave_groups=[?GROUP1,?GROUP2]},
    ?assertEqual(MainState, leechland_master:abort_tasks(no_such_group, <<>>, 
                 {svr2,svr2}, MainState)).

%% abort task for group1 with wrong pwd
abort_task3_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=?ONGOING,
                            slave_groups=[?GROUP1,?GROUP2]},
    ?assertEqual(MainState, leechland_master:abort_tasks(group1, <<>>, {svr2,svr2}, MainState)).

%% abort task for group1 with correct pwd but non-existing slave
abort_task4_test() ->
    P = self(),
    TASKQ1=#leech_tasks{group_type=?GENSLAVE,
        tasks=[#leech_task{client={P,svr},taskId=3,data=t1},
               #leech_task{client={P,svr1},taskId=2,data=t1},
               #leech_task{client={P,svr1},taskId=3,data=t1}]},
    TASKQ2=#leech_tasks{group_type=group1,
         tasks=[#leech_task{client={P,svr2},taskId=3,data=t1},
                #leech_task{client={P,svr3},taskId=2,data=t1},
                #leech_task{client={P,svr4},taskId=1,data=t1}]},
    TaskQueue = [TASKQ1,TASKQ2],

    ONGNT1=#ongoing_task{task=#leech_task{client={P,svr},taskId=1,data=t1}, 
                         slave=node1},
    ONGNT2=#ongoing_task{task=#leech_task{client={P,svr1},taskId=1,data=t1}, 
                         slave=node2},
    ONGNT3=#ongoing_task{task=#leech_task{client={P,svr},taskId=2,data=t2},
                         slave=node3},
    ONGNT4=#ongoing_task{task=#leech_task{client={P,svr2},taskId=1,data=t1}, 
                         slave=node4},
    ONGNT5=#ongoing_task{task=#leech_task{client={P,svr3},taskId=1,data=t1}, 
                         slave=node5}, 
    ONGNT6=#ongoing_task{task=#leech_task{client={P,svr2},taskId=2,data=t2},
                         slave=node6},
    GTASK1=#ongoing_tasks{group_type=?GENSLAVE,
                          tasks=[ONGNT1,ONGNT2,ONGNT3]},
    GTASK2=#ongoing_tasks{group_type=group1,
                          tasks=[ONGNT4,ONGNT5,ONGNT6]},
    TaskQueue = [TASKQ1,TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=[GTASK1,GTASK2],
                            slave_groups=[?GROUP1,?GROUP2]},
    ?assertEqual(MainState, leechland_master:abort_tasks(group1,term_to_binary(?PWD),{P,no_svr},MainState)).

%% dispatch tasks test
dispatch_tasks_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=?ONGOING,
                            slave_groups=[?GROUP1,?GROUP2]},
    NewGroup = ?GROUP2#slave_group{
                             slaves=[#slave_info{node=node4,state=running},
                                     #slave_info{node=node5,state=running},
                                     #slave_info{node=node6,state=running},
                                     #slave_info{node=node7,state=running}]},

    OngnT = #ongoing_task{task=#leech_task{client={svr2,svr2},taskId=3,data=t1}, slave=node7},
    NewOngnQ = ?GTASK2#ongoing_tasks{tasks=[?ONGNT4,?ONGNT5,?ONGNT6,OngnT]},
    NewTaskQ = ?TASKQ2#leech_tasks{tasks=[#leech_task{client={svr3,svr3},taskId=2,data=t1},
                                   #leech_task{client={svr4,svr4},taskId=1,data=t1}]},
    NewState = MainState#main_state{task_queue=[?TASKQ1,NewTaskQ],
                                    ongoing_queue=[?GTASK1,NewOngnQ],
                                    slave_groups=[?GROUP1,NewGroup]},

    ?assertEqual(NewState, leechland_master:dispatch_tasks(group1,MainState)).

%% dispatch tasks test
dispatch_tasks1_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    Group1 = ?GROUP2#slave_group{
                             slaves=[#slave_info{node=node4,state=running},
                                     #slave_info{node=node5,state=idle},
                                     #slave_info{node=node6,state=idle},
                                     #slave_info{node=node7,state=idle},
                                     #slave_info{node=node8,state=idle}]},
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=[?GTASK1,#ongoing_tasks{group_type=group1,
                                                                  tasks=[?ONGNT4]}],
                            slave_groups=[?GROUP1,Group1]},

    NewOngnQ = #ongoing_tasks{group_type=group1,
                              tasks=[?ONGNT4,
                                     #ongoing_task{task=#leech_task{client={svr2,svr2},taskId=3,data=t1},
                                                   slave=node5},
                                     #ongoing_task{task=#leech_task{client={svr3,svr3},taskId=2,data=t1},
                                                   slave=node6},
                                     #ongoing_task{task=#leech_task{client={svr4,svr4},taskId=1,data=t1},
                                                   slave=node7}]},
    NewGroup = ?GROUP2#slave_group{
                             slaves=[#slave_info{node=node4,state=running},
                                     #slave_info{node=node5,state=running},
                                     #slave_info{node=node6,state=running},
                                     #slave_info{node=node7,state=running},
                                     #slave_info{node=node8,state=idle}]},
    NewState = MainState#main_state{task_queue=[?TASKQ1],
                                    ongoing_queue=[?GTASK1,NewOngnQ],
                                    slave_groups=[?GROUP1,NewGroup]},
    ?assertEqual(NewState, leechland_master:dispatch_tasks(group1,MainState)).

create_group_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=?ONGOING,
                            slave_groups=[?GROUP1,?GROUP2]},
    NewGroup = #slave_group{group_type=new_group, pincode= <<>>},
    NewState = MainState#main_state{slave_groups=[?GROUP1,?GROUP2,NewGroup]},
    ?assertEqual(NewState, leechland_master:create_group_int(new_group, <<>>, MainState)).

%% delete group test
delete_group_test() ->
    NewQ =
    ?TASKQ2#leech_tasks{tasks=[#leech_task{client={self(),svr2},taskId=3,data=t1}]},
    TaskQueue = [?TASKQ1,NewQ],
    ONGNT4 = #ongoing_task{task=#leech_task{client={self(),svr2},taskId=1,data=t1},slave=node4},
    ONGNT5 = #ongoing_task{task=#leech_task{client={self(),svr3},taskId=1,data=t1},slave=node5},

    NewOngnQ = #ongoing_tasks{group_type=group1,
                              tasks=[ONGNT4,ONGNT5]},

    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=[?GTASK1,NewOngnQ],
                            slave_groups=[?GROUP1,?GROUP2]},

    NewState = MainState#main_state{task_queue=[?TASKQ1],ongoing_queue=[?GTASK1],
                                    slave_groups=[?GROUP1]},
    ?assertEqual(NewState, leechland_master:delete_group(group1, MainState)).

delete_group1_test() ->
    NewQ = ?TASKQ2#leech_tasks{tasks=[]},
    TaskQueue = [?TASKQ1,NewQ],
    NewOngnQ = #ongoing_tasks{group_type=group1, tasks=[]},

    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=[?GTASK1,NewOngnQ],
                            slave_groups=[?GROUP1,?GROUP2]},

    NewState = MainState#main_state{task_queue=[?TASKQ1],ongoing_queue=[?GTASK1],
                                    slave_groups=[?GROUP1]},
    ?assertEqual(NewState, leechland_master:delete_group(group1, MainState)).

%% get group name test
get_group_name_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=?ONGOING,
                            slave_groups=[?GROUP1,?GROUP2]},
    ?assertEqual(?GENSLAVE, leechland_master:get_group_name(node3, MainState)).

%% get group name test
get_group_name1_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=?ONGOING,
                            slave_groups=[?GROUP1,?GROUP2]},
    ?assertEqual({error,not_found}, leechland_master:get_group_name(nonode, MainState)).

remove_from_group_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=?ONGOING,
                            slave_groups=[?GROUP1,?GROUP2]},
    Slave = #slave{group_type=group1, node=node6},
    NewGroup = ?GROUP2#slave_group{slaves=[#slave_info{node=node4,state=running},
                                     #slave_info{node=node5,state=running},
                                     #slave_info{node=node7,state=idle}]},
    NewState = MainState#main_state{slave_groups=[?GROUP1,NewGroup]},
    ?assertEqual(NewState, leechland_master:remove_from_group(Slave, MainState)).

remove_from_group1_test() ->
    TaskQueue = [?TASKQ1,?TASKQ2],
    MainState = #main_state{task_queue=TaskQueue,
                            ongoing_queue=?ONGOING,
                            slave_groups=[?GROUP1,?GROUP2]},
    Slave = #slave{group_type=group1, node=nonode},
    ?assertEqual(MainState, leechland_master:remove_from_group(Slave, MainState)).



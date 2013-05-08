-module(leechland_master).

-export([start/0]).

%% functions exported only for leechland_client.erl to handle main_state easily
-export([get_group/2,
         get_taskQ/2,
         get_ongnQ/2]).

-include("leechland.hrl").

%%----------------------------------------------------------------------------
%%  Interface
%%----------------------------------------------------------------------------
start() ->
    register(?MASTERPROC, self()),
    clean_logs(),
    {Servers, SlaveName} = get_slaves(),
    %% start all generic slaves
    case start_slaves(?GENSLAVE, SlaveName, Servers, []) of
        [] ->
            ?DBG("All slaves refuse to start working, exit~n"),
            io:format("All slaves refuse to start working, exit~n"),
            halt();
        _ ->
            write_to_deployfile(),
            State=create_group_int(?GENSLAVE,<<>>,
                                   #main_state{master_node = node()}),
            main_loop(State)
    end.

%%----------------------------------------------------------------------------
%%  Main
%%----------------------------------------------------------------------------
-spec main_loop(#main_state{}) -> #main_state{}.
main_loop(State)->
    receive
        {client_request, {Client, GroupName, TaskList, CallbackInfo}} ->
            ?DBG("received client request:[~p] to group ~p from ~p with 
                  callback ~p~n", [TaskList, GroupName, Client, CallbackInfo]),
            %% recheck the group just in case when group is deleted right before
            %% request arrives
            Fun = fun() -> group_delete_notify([Client]), State end,
            Fun1 = fun(_) ->
                    TaskList1=[{Client, CallbackInfo, Task}||Task <- TaskList],
                    %% task id starts from 1 because in leechland_client,
                    %% client will check integrity of result based on task id 
                    %% starts from 1, so don't change it
                    Tasks = create_leech_tasks(TaskList1, 1, []),
                    NewState = add_taskQ(GroupName, Tasks, State),
                    dispatch_tasks(GroupName, NewState) end,
            main_loop(get_group_handler(GroupName,State,Fun,Fun1));

        {slave_response, {Slave, Client, TaskId, Response}} ->
            %?DBG("received slave response:~p with taskid ~p for client ~p from 
            %      [~p] group [~p]~n", [Response,TaskId,Client,Slave#slave.node,
            %                           Slave#slave.group_type]),
            Fun = fun() -> group_delete_notify([Client]), State end,
            Fun1 = fun(Group) ->
                    NewState = 
                    case lists:keyfind(Slave#slave.node, 2, 
                                       Group#slave_group.slaves) of
                       false -> State;
                       _ -> continue_dispatch(Client, TaskId, Slave, State)
                    end,
                    process_response(Slave#slave.group_type,Client,TaskId,
                                     Response,NewState), NewState end,
            main_loop(get_group_handler(Slave#slave.group_type,State,Fun,Fun1));

        {nodedown, SlaveNode} ->
            GroupName = get_group_name(SlaveNode, State),
            ?DBG("slave [~p] group [~p] is down~n", [SlaveNode, GroupName]),
            Fun = fun() -> State end,
            Fun1 = fun(_) -> 
                    Slave = #slave{group_type=GroupName, node=SlaveNode},
                    {Task, NewState} = remove_ongoing_task(Slave, State),
                    NewState1 = add_taskQ(GroupName, Task, NewState),
                    NewState2 = remove_from_group(Slave, NewState1),
                    NewState3 = restart_slave(Slave, NewState2),
                    dispatch_tasks(GroupName, NewState3) end,
            main_loop(get_group_handler(GroupName,State,Fun,Fun1));

        {slave_up, Slave} ->
            ?DBG("slave [~p] group [~p] is up running~n", 
                 [Slave#slave.node, Slave#slave.group_type]),
            Fun = fun() -> State end,
            Fun1 = fun(_) ->
                    NewState = change_slaveState(idle, Slave, State),
                    dispatch_tasks(Slave#slave.group_type, NewState) end,
            main_loop(get_group_handler(Slave#slave.group_type,State,Fun,Fun1));

        {add_server, {Client,GroupName,SlaveName,ServerName,InstN}} ->
            ?DBG("add ~p slaves on [~p] for group [~p]~n", 
                 [InstN,ServerName,GroupName]),
            Fun = fun() -> {group_not_found, State} end,
            Fun1 = fun(_) -> 
                    add_slaves(GroupName,SlaveName,ServerName,InstN,[],State) 
                   end,
            {Respone, NewState} = get_group_handler(GroupName,State,Fun,Fun1),
            Client ! Respone,
            main_loop(NewState);

        {remove_server, {Client,GroupName,Pincode,ServerName}} ->
            ?DBG("remove server [~p] from group [~p]~n",[ServerName,GroupName]),
            Fun = fun(Reason) -> {Reason, State} end,
            Fun1 = fun() -> {ok, remove_slave(GroupName,ServerName,State)} end,
            {Resp,State1}=check_group_handler(GroupName,Pincode,State,Fun,Fun1),
            Client ! Resp,
            main_loop(State1);

        {abort, {GroupName, Pincode, Client}} ->
            ?DBG("recieved abort msg from [~p] for group [~p]~n", 
                 [Client, GroupName]),
            main_loop(abort_tasks(GroupName, Pincode, Client, State));

        {is_alive, {Pid, Node}} ->
            ?DBG("alive message from client ~p~n", [{Pid, Node}]),
            Pid ! {alive, State},
            main_loop(State);

        {create_group, {Client,GroupName,Pincode}} ->
            ?DBG("create group ~p~n", [GroupName]),
            Fun = fun() -> {ok, create_group_int(GroupName,Pincode,State)} end,
            Fun1 = fun(_) -> {already_exist, State} end,
            {Respone,NewState} = get_group_handler(GroupName,State,Fun,Fun1),
            Client ! Respone,
            main_loop(NewState);

        {delete_group, {Client,GroupName,Pincode}} ->
            ?DBG("delete group ~p~n", [GroupName]),
            Fun = fun(Reason) -> {Reason, State} end,
            Fun1 = fun() -> {ok, delete_group(GroupName, State)} end,
            {Resp,State1}=check_group_handler(GroupName,Pincode,State,Fun,Fun1),
            Client ! Resp,
            main_loop(State1);

        {reboot_group, {Client,GroupName,Pincode}} ->
            ?DBG("reboot group ~p~n", [GroupName]),
            Client ! ok,
            main_loop(reboot_group(GroupName, Pincode, State));

        {check_group, GroupName, Pincode, Client} ->
            ?DBG("check group [~p] from client ~p~n", [GroupName, Client]),
            Client ! check_group(GroupName,Pincode,State),
            main_loop(State);

        Unexpected ->
            ?DBG("unexpected msg received:~p~n", [Unexpected]),
            main_loop(State)
    end.

%%----------------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------------
%%----------------------------------------------------------------------------
%%  handle tasks
%%----------------------------------------------------------------------------
-spec create_leech_tasks([{server(), #callback_info{}, term()}], 
            non_neg_integer(), [#leech_task{}]) -> [#leech_task{}].
create_leech_tasks([], _TaskId, NewTasks) ->
    NewTasks;
create_leech_tasks([{Client, CallbackInfo, Task}|Rest], TaskId, NewTasks) ->
    create_leech_tasks(Rest, TaskId+1, 
                       lists:append(NewTasks,
                                    [#leech_task{client = Client, 
                                                 taskId = TaskId, 
                                                 callback_info = CallbackInfo, 
                                                 data = Task}])).

-spec add_taskQ(atom(), [#leech_task{}], #main_state{}) -> #main_state{}.
add_taskQ(_, [], State) ->
    State;
add_taskQ(GroupName, Tasks, State) ->
    TaskQ = get_taskQ(GroupName, State),
    NewQ = TaskQ#leech_tasks{tasks=lists:append(TaskQ#leech_tasks.tasks,Tasks)},
    put_taskQ(NewQ, State).

-spec get_taskQ(atom(), #main_state{}) -> #leech_tasks{}.
get_taskQ(GroupName, State) ->
    case lists:keysearch(GroupName, 2, State#main_state.task_queue) of
        false ->
            %% create a new queue
            #leech_tasks{group_type=GroupName, tasks=[]};
        {value, TaskQ} ->
            TaskQ
    end.

-spec put_taskQ(#leech_tasks{}, #main_state{}) -> #main_state{}.
put_taskQ(TaskQ, State)->
    NewQ =
    case TaskQ#leech_tasks.tasks of
        [] ->
            %% delete the empty queue to save some space
            lists:keydelete(TaskQ#leech_tasks.group_type, 2,
                            State#main_state.task_queue);
        _ ->
            lists:keystore(TaskQ#leech_tasks.group_type, 2,
                           State#main_state.task_queue, TaskQ)
    end,
    State#main_state{task_queue = NewQ}.

-spec get_ongnQ(atom(), #main_state{}) -> #ongoing_tasks{}.
get_ongnQ(GroupName, State) ->
    case lists:keysearch(GroupName, 2, State#main_state.ongoing_queue) of
        false ->
            %% create a new queue
            #ongoing_tasks{group_type=GroupName, tasks=[]};
        {value, OngnQ} ->
            OngnQ
    end.

-spec put_ongnQ(#ongoing_tasks{}, #main_state{}) -> #main_state{}.
put_ongnQ(OngnQ, State)->
    NewQ =
    case OngnQ#ongoing_tasks.tasks of
        [] ->
            %% delete the empty queue to save some space
            lists:keydelete(OngnQ#ongoing_tasks.group_type, 2,
                            State#main_state.ongoing_queue);
        _ ->
            lists:keystore(OngnQ#ongoing_tasks.group_type, 2,
                           State#main_state.ongoing_queue, OngnQ)
    end,
    State#main_state{ongoing_queue = NewQ}.

-spec remove_ongoing_task
        (#slave{}, #main_state{}) -> {[#leech_task{}]|[],#main_state{}};
        ({server(), non_neg_integer(), #slave{}}, #main_state{}) -> #main_state{}.
remove_ongoing_task(Slave, State) when is_record(Slave, slave)->
    OngnQ = get_ongnQ(Slave#slave.group_type, State),
    if 
        OngnQ#ongoing_tasks.tasks == [] ->
            {[],State};
        true ->
            RmvTask = [OngnTask||OngnTask<-OngnQ#ongoing_tasks.tasks,
                       OngnTask#ongoing_task.slave == Slave#slave.node],
            if RmvTask == [] ->
                   {[],State};
               true ->
                   NewQ=OngnQ#ongoing_tasks{tasks=
                                            OngnQ#ongoing_tasks.tasks--RmvTask},
                   [Task] = RmvTask,
                   {[Task#ongoing_task.task], put_ongnQ(NewQ, State)}
            end
    end;
remove_ongoing_task({Client, TaskId, Slave}, State) ->
    OngnQ = get_ongnQ(Slave#slave.group_type, State),
    Pred = fun({_, Task, N}) ->
                    #leech_task{client = C,
                                taskId = T} = Task,
                    {C, T, N} /= {Client, TaskId, Slave#slave.node} end,
    NewOngnQ = OngnQ#ongoing_tasks{tasks= 
                             lists:filter(Pred, OngnQ#ongoing_tasks.tasks)},
    put_ongnQ(NewOngnQ, State).

-spec continue_dispatch(server(), non_neg_integer(), #slave{}, #main_state{}) 
        -> #main_state{}.
continue_dispatch(Client, TaskId, Slave, State) ->
    State1 = remove_ongoing_task({Client, TaskId, Slave}, State),
    State2 = change_slaveState(idle, Slave, State1),
    dispatch_task(Slave#slave.group_type, 
                  get_taskQ(Slave#slave.group_type, State2), 
                  State2, [Slave#slave.node]).

-spec dispatch_tasks(atom(), #main_state{}) -> #main_state{}.
dispatch_tasks(GroupName, State) ->
    Nodes = find_available_slaves(GroupName, State),
    dispatch_task(GroupName, get_taskQ(GroupName, State), State, Nodes).

-spec dispatch_task(atom(),#leech_tasks{},#main_state{},[node()]) -> #main_state{}.
dispatch_task(_, _, State, []) ->
    State;
dispatch_task(_, TaskQ, State, _) 
    when TaskQ#leech_tasks.tasks == [] ->
    State;
dispatch_task(GroupName, _TaskQ, State, [Node|Rest]) ->
    {Task, TaskQ, NewState} = pick_one_task(GroupName, State, Node),
    dispatch_to_slave(GroupName, Node, Task),
    NewState1 = change_slaveState(running, 
                  #slave{group_type=GroupName,node=Node}, NewState),
    dispatch_task(GroupName, TaskQ, NewState1, Rest).

-spec pick_one_task(atom(), #main_state{}, node()) -> 
        {#leech_task{}, #leech_tasks{}, #main_state{}}.
pick_one_task(GroupName, State, Node) ->
    TaskQ = get_taskQ(GroupName, State),
    OngnQ = get_ongnQ(GroupName, State),
    [Task|Rest] = TaskQ#leech_tasks.tasks,
    NewTaskQ = TaskQ#leech_tasks{tasks=Rest},
    NewTasks = 
    lists:append(OngnQ#ongoing_tasks.tasks,
                 [#ongoing_task{task=Task,slave=Node}]),
    NewOngnQ = OngnQ#ongoing_tasks{tasks=NewTasks},
    State1 = put_ongnQ(NewOngnQ, put_taskQ(NewTaskQ, State)),
    {Task, NewTaskQ, State1}.

-spec dispatch_to_slave(atom(), node(), #leech_task{}) -> no_return().
dispatch_to_slave(_GroupName, Node, Task) ->
  % ?DBG("Dispath task ~p to slave [~p] group [~p]~n", [Task, Node, GroupName]),
    {?SLAVEPROC, Node} ! {leech_task, Task}.

-spec abort_tasks(atom(), binary(), server(), #main_state{}) -> #main_state{}.
abort_tasks(GroupName, PinCode, {ExtPid, Node}, State) ->
    case check_group(GroupName,PinCode,State) of
        ok ->
           TaskQ = get_taskQ(GroupName, State),
           OngnQ = get_ongnQ(GroupName, State),
           %% remove tasks from task queue
           Pid =
           case ExtPid of
               ExtPid when is_pid(ExtPid) ->
                   %% transfer pid to string form
                   pid_to_list(ExtPid);
               _ ->
                   ExtPid
           end,
           List=[T||T<-TaskQ#leech_tasks.tasks, 
                 begin {P,CN} = T#leech_task.client, 
                       {pid_to_list(P),CN} == {Pid, Node}
                 end],
           List1=[{R, T, N}||{R, T, N}<-OngnQ#ongoing_tasks.tasks,
                  begin {P,CN} = T#leech_task.client,
                        {pid_to_list(P),CN} == {Pid, Node}
                  end],
           NewTaskQ=TaskQ#leech_tasks{tasks=TaskQ#leech_tasks.tasks--List},
           NewOngnQ=OngnQ#ongoing_tasks{tasks=OngnQ#ongoing_tasks.tasks--List1},
           %% stop all slaves
           lists:foreach(fun({_,_,Slave}) -> stop_slave(Slave) end, List1),
           put_ongnQ(NewOngnQ, put_taskQ(NewTaskQ, State));
        _ ->
           State
    end.

-spec remove_all_tasks(atom(), #main_state{}) -> #main_state{}.
remove_all_tasks(GroupName, State) ->
    C = lists:usort([T#leech_task.client||T<-
                     (get_taskQ(GroupName, State))#leech_tasks.tasks]),
    C1 = lists:usort([(T#ongoing_task.task)#leech_task.client||T<-
                     (get_ongnQ(GroupName, State))#ongoing_tasks.tasks]), 
    group_delete_notify(lists:usort(C++C1)),
    NewTaskQ = lists:keydelete(GroupName, 2, State#main_state.task_queue),
    NewOngnQ = lists:keydelete(GroupName, 2, State#main_state.ongoing_queue),
    State#main_state{task_queue=NewTaskQ, ongoing_queue=NewOngnQ}.

%%----------------------------------------------------------------------------
%%  handle slaves
%%----------------------------------------------------------------------------
-spec start_slaves(atom(), string(), [{string(),non_neg_integer()}], [node()]) 
          -> [node()].
start_slaves(_SlaveGroup, _Slave, [], SlaveNodes) ->
    SlaveNodes;
start_slaves(Group, Slave, [Server|Rest], SlaveNodes) ->
    start_slaves(Group, Slave, Rest,
                 start_slaves_on_server(Group, Slave, Server, SlaveNodes)).

-spec start_slaves_on_server(atom(), string(), {string(), non_neg_integer()},
                             [node()]) -> [node()].
start_slaves_on_server(_Group, _Slave, {_Server,0}, SlaveNodes) ->
    SlaveNodes;
start_slaves_on_server(Group, Slave, {Server,InstN}, SlaveNodes) ->
    SlaveName = list_to_atom(Slave++integer_to_list(InstN)),
    start_slaves_on_server(Group, Slave, {Server,InstN-1}, 
                SlaveNodes++start_slave(Group, SlaveName, Server)).

-spec start_slave(atom(), atom(), string()) -> [node()]|[].
start_slave(GroupName, SlaveName, Server) ->
    Args = erl_sys_args(GroupName),
    ?DBG("start slave ~p@~s with command ~p~n", [SlaveName, Server, Args]),
    case slave:start_link(Server, SlaveName, Args) of
        {ok, Node} ->
            monitor_node(Node, true),
            [Node];
        _ ->
            os:cmd("expect auto_connect.sh "++Server),
            %% try again
            case slave:start_link(Server, SlaveName, Args) of
                {ok, Node} ->
                    monitor_node(Node, true),
                    [Node];
                Failed ->
                    %% give up this slave
                    ?DBG("slave ~p@~s can not be started with reason:~p, "
                         "give up~n", [SlaveName,Server,Failed]),
                    []
            end
    end.

-spec find_available_slaves(atom(), #main_state{}) -> [node()].
find_available_slaves(GroupName, State) ->
    {ok, Group} = get_group(GroupName, State),
    Pred = fun({_, _Node, NodeState}) -> NodeState == idle end,
    [Node || {_,Node,_} <- lists:filter(Pred, Group#slave_group.slaves)].

-spec change_slaveState(SlaveState::idle|running, Slave::#slave{}, 
            State::#main_state{}) -> #main_state{}.
change_slaveState(SlaveState, Slave, State) ->
    %?DBG("Change state for ~p to ~p~n", [Slave#slave.node,SlaveState]),
    NewSlave = #slave_info{node=Slave#slave.node, state=SlaveState},
    {ok,Group} = get_group(Slave#slave.group_type, State),
    Slaves=lists:keystore(Slave#slave.node,2,Group#slave_group.slaves,NewSlave),
    put_group(Group#slave_group{slaves=Slaves}, State).

-spec restart_slave(#slave{}, #main_state{}) -> #main_state{}.
restart_slave(Slave, State) ->
    case lists:member(Slave#slave.node,State#main_state.removed_nodes_tmp) of
        true ->
            %% intentionally shutdown, remove it from list
            State#main_state{removed_nodes_tmp=
                                          State#main_state.removed_nodes_tmp --
                                          [Slave#slave.node]};
        false ->
            [SlaveName, Server] = 
            string:tokens(atom_to_list(Slave#slave.node), "@"),
            case start_slave(Slave#slave.group_type, 
                             list_to_atom(SlaveName), Server) of
                [] ->
                    %% try again
                    start_slave(Slave#slave.group_type, 
                                list_to_atom(SlaveName), Server);
                _ ->
                    ok
            end,
            State
    end.

-spec add_slaves(atom(),string(),string(),non_neg_integer(),
      [{node(),failed|ok}], #main_state{}) -> {[{node(), ok|failed}], #main_state{}}.
add_slaves(_, _, _, 0, Resp, State) ->
    {Resp, State};
add_slaves(GroupName, SlaveName, ServerName, InstN, Resp, State) ->
    Slave = SlaveName++integer_to_list(InstN),
    Node = list_to_atom(Slave++"@"++ServerName),
    {NewResp, NewState} =
    case get_group_name(Node, State) of
        {error, not_found} ->
            Removed = State#main_state.removed_nodes_tmp--[Node],
            case start_slave(GroupName, list_to_atom(Slave), ServerName) of
                [] ->
                    {[{Node,failed}|Resp],
                     State#main_state{removed_nodes_tmp = Removed}};
                [Node] ->
                    {[{Node,ok}|Resp],
                     State#main_state{removed_nodes_tmp = Removed}}
            end;
        Group1 ->
            ?DBG("slave ~p is already up in group [~p]~n", [Node, Group1]),
            {[{Node,ok}|Resp],State}
    end,
    add_slaves(GroupName, SlaveName, ServerName, InstN-1, NewResp, NewState).

-spec remove_slave(atom(), string(), #main_state{}) -> #main_state{}.
remove_slave(GroupName, Server, State) ->
    {ok, Group} = get_group(GroupName, State),
    Nodes = 
    [N||{_,N,_}<-Group#slave_group.slaves, 
        begin [_, S] = string:tokens(atom_to_list(N),"@"), S == Server end],
    lists:foreach(fun(N)-> stop_slave(N) end, Nodes),
    Removed = lists:usort(Nodes++State#main_state.removed_nodes_tmp),
    State#main_state{removed_nodes_tmp = Removed}.

-spec stop_slave(node()) -> no_return().
stop_slave(Node) ->
    ?DBG("stop slave [~p]~n", [Node]),
    {?SLAVEPROC, Node} ! stop.

%%----------------------------------------------------------------------------
%%  handle groups
%%----------------------------------------------------------------------------
-spec create_group_int(atom(),binary(),#main_state{}) -> #main_state{}.
create_group_int(GroupName,Pincode,State) ->
    Group = #slave_group{group_type=GroupName, pincode=Pincode},
    put_group(Group, State).

-spec delete_group(atom(), #main_state{}) -> #main_state{}.
delete_group(GroupName, State) ->
    {ok, Group} = get_group(GroupName, State),
    case Group#slave_group.slaves of
        [] ->
            ok;
        Slaves ->
            Nodes = [N||{_,N,_}<-Slaves],
            lists:foreach(fun(N)-> stop_slave(N) end, Nodes)
    end,
    State1 = remove_all_tasks(GroupName, State),
    State1#main_state{slave_groups=State1#main_state.slave_groups--[Group]}.

-spec get_group(atom(), #main_state{}) -> {ok, #slave_group{}}|not_found.
get_group(GroupName, State) ->
    case lists:keysearch(GroupName, 2, State#main_state.slave_groups) of
        {value, Group} ->
            {ok, Group};
        false ->
            not_found
    end.

-spec put_group(#slave_group{}, #main_state{}) -> #main_state{}.
put_group(Group, State) ->
    NewGroups = lists:keystore(Group#slave_group.group_type, 2,
                               State#main_state.slave_groups, Group),
    State#main_state{slave_groups = NewGroups}.

-spec check_group(atom(),binary(),#main_state{}) 
                   -> ok|no_such_group|wrong_pincode.
check_group(?GENSLAVE,_,_) ->
    ok;
check_group(GroupName,Pincode,State) ->
    case get_group(GroupName, State) of
        {ok, Group} ->
            if Group#slave_group.pincode == Pincode ->
                    ok;
               true ->
                    wrong_pincode
            end;
        _ ->
            no_such_group
    end.

-spec get_group_name(node(), #main_state{}) -> {error,not_found}|atom().
get_group_name(Node, State) ->
    case [G||G<-State#main_state.slave_groups,
          N<-G#slave_group.slaves, N#slave_info.node==Node] of
        [] ->
            {error,not_found};
        [Group] ->
            Group#slave_group.group_type
    end.

-spec remove_from_group(#slave{}, #main_state{}) -> #main_state{}.
remove_from_group(Slave, State) ->
    case get_group(Slave#slave.group_type, State) of
        not_found ->
            State;
        {ok, Group} ->
            Slaves = lists:keydelete(Slave#slave.node, 2,
                                     Group#slave_group.slaves),
            put_group(Group#slave_group{slaves=Slaves}, State)
    end.

-spec reboot_group(atom(), binary(), #main_state{}) -> #main_state{}.
reboot_group(GroupName, PinCode, State) ->
    case check_group(GroupName, PinCode, State) of
        ok ->
           {ok, Group} = get_group(GroupName, State),
           %% stop all slaves
           lists:foreach(fun({_,Slave,_}) -> stop_slave(Slave) end, 
                         Group#slave_group.slaves),
           remove_all_tasks(GroupName, State);
        _ ->
           State
    end.

%%----------------------------------------------------------------------------
%%  others
%%----------------------------------------------------------------------------
-spec process_response(atom(), server(), non_neg_integer(), any(), #main_state{})
                           -> no_return().
process_response(GroupName, Client, TaskId, Response, State) ->
    {ClientPid, _} = Client,
    TaskQ = get_taskQ(GroupName, State),
    OngnQ = get_ongnQ(GroupName, State),
    case
        {[T||{_,C,T,_,_} <- TaskQ#leech_tasks.tasks, C == Client],
         [T||{_,{_,C,T,_,_},_} <- OngnQ#ongoing_tasks.tasks, C == Client]} of
        {[],[]} ->
            ?DBG("send done notification to client ~p~n", [Client]),
            ClientPid ! {done, {TaskId, Response}};
        _ ->
            ClientPid ! {result, {TaskId, Response}}
    end.

-spec erl_sys_args(atom()) -> string().
erl_sys_args(GroupName) ->
  {ok,[[LogPath]]} = init:get_argument(logs),
  {ok,[[ErlPath]]} = init:get_argument(pa),
  lists:append(["-rsh ssh -noshell -setcookie ",
                atom_to_list(erlang:get_cookie()), " -connect_all false",
                " -s leechland_slave start -masterserver ", 
                atom_to_list(node()), " -logs ", LogPath,
                " -pa ", ErlPath, " -slave_group ", atom_to_list(GroupName)]).

-spec write_to_deployfile() -> no_return().
write_to_deployfile() ->
    spawn(fun() -> write_to_deployfile_int() end).

-spec get_slaves() -> {[{string(),non_neg_integer()}], string()}.
get_slaves() ->
    SlaveServers =
        case init:get_argument(slaves) of
            error -> 
                ?DBG("ERROR! No slave server is provided~n"
                     "Please make sure there is available server "
                     "in your config file~n"),
                erlang:error(no_slaves_found);
            {ok,[Slaves]} ->
                lists:map(
                    fun(Slave)-> 
                            [Server, Inst]=string:tokens(Slave, ","), 
                            InstN = list_to_integer(Inst),
                            if InstN < 0 ->
                                   {Server, 1};
                               true ->
                                   {Server, InstN} 
                            end end, Slaves)
        end,

    SlaveName =
        case init:get_argument(slavename) of
            error -> 
                ?DBG("ERROR! No slave name is provided~n"
                     "Please start leechland with --slave-name~n"),
                erlang:error(no_slavename_found);
            {ok,[[Slave]]} ->
                 Slave
        end,
    {SlaveServers, SlaveName}.

clean_logs() ->
    {ok,[[Path]]} = init:get_argument(logs),
    os:cmd("rm -f "++Path++"/*.log").

-spec group_delete_notify([server()]) -> ok.
group_delete_notify([]) ->
    ok;
group_delete_notify([{Pid, _}|Rest]) ->
    Pid ! ?GROUP_GONE,
    group_delete_notify(Rest).

%%----------------------------------------------------------------------------
%% generic functions
%%----------------------------------------------------------------------------
-spec get_group_handler(atom(), #main_state{}, fun(), fun()) -> any().
get_group_handler(GroupName, State, GroupNotFound, GroupFound) ->
    case get_group(GroupName, State) of
        not_found ->
            GroupNotFound();
        {ok, Group} ->
            GroupFound(Group)
    end.

-spec check_group_handler(atom(),binary(),#main_state{},fun(),fun()) -> any().
check_group_handler(GroupName,Pincode,State,Rejected,Validated) ->
    case check_group(GroupName,Pincode,State) of
        ok ->
            Validated();
        Reason ->
            Rejected(Reason)
    end.


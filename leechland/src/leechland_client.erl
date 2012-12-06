%%
%% This module is used by users together with a leechland configuration file
%% (which is generated when leechland master is started up) to communicate to
%% leechland master for group management and task handling
%%
%% How to use this module:
%%    1. copy the leechland_client.beam to the place where your Erlang code 
%%       can find it
%%    2. copy(and rename it if you want) the deployment.cfg file when it's 
%%       generated by master to anywhere you desire to
%%    3. directly call APIs in this module with given configuration file you
%%       copied at step 2 to get any leechland service you want
%%

-module(leechland_client).

-export([get_master/0,
         get_master/1,
         abort_tasks/4,
         is_alive/0,
         is_alive/2,
         get_master_state/2,
         create_group/3,
         delete_group/3,
         check_group/4,
         add_server/3,
         add_server/4,
         remove_server/2,
         remove_server/4,
         send/3,
         send/4,
         send/6]).

-include("leechland.hrl").

%%----------------------------------------------------------------------------
%% User interface
%%----------------------------------------------------------------------------

%% find out the master's node name and process name for communication based on
%% the given config file, the cookie for communication towards master will be
%% set in this function, note that, this is a 'MUST CALL' function before you 
%% try to send any task to master using 'send' API in this module
-spec get_master() -> {ok, server()}|{error, term()}.
get_master() ->
    get_master(?DEPLOYFILE).

-spec get_master(string()) -> {ok, server()}|{error, term()}.
get_master(DeployFile) when is_list(DeployFile) ->
    case fetch_deployinfo(DeployFile) of
        {error, Error} ->
            {error, Error};
        {MasterNode,MasterProc,Cookie,_,_,_,_} ->
            erlang:set_cookie(node(), list_to_atom(Cookie)),
            {ok, {list_to_atom(MasterProc), list_to_atom(MasterNode)}}
    end.

%% abort all tasks for given client
-spec abort_tasks(atom(), string(), string(), pid()) 
        -> ok|timeout|no_such_group|wrong_pincode|master_unreachable.
abort_tasks(GroupName, Pincode, DeployFile, Pid) when is_atom(GroupName),
                                                      is_list(Pincode),
                                                      is_list(DeployFile),
                                                      is_pid(Pid) ->
    {ok, Master} = leechland_client:get_master(DeployFile),
    case check_group(Master, GroupName, Pincode, 10*1000) of
        ok ->
            Pid ! kill_all,
            check_process(Pid, 100);
        Error ->
            Error
    end.

%% find out if master is alive or reachable, if so return the detailed state
%% information of master side
-spec is_alive() -> {alive,#main_state{}}|master_unreachable.
is_alive() ->
    is_alive(60*1000, ?DEPLOYFILE).

-spec is_alive(non_neg_integer(), string()) 
        -> {alive,#main_state{}}|master_unreachable.
is_alive(Timeout, DeloyFile) when Timeout > 0,
                                  is_list(DeloyFile) ->
    Msg = {is_alive, {self(), node()}},
    send_to_master(DeloyFile,Msg,Timeout).

%% get the detailed information from leechland master, the format for the return
%% info is like this:
%% {
%%  master server name,
%%  [groupname, number of slaves],
%%  [{groupname, [{slavename, idle|running}], [{task, slavename}], [tasks]}]
%% }
-spec get_master_state(non_neg_integer(), string()) 
        -> failed_reading_cfg|master_unreachable|
           {
            node(),
            [{atom(), non_neg_integer()}],
            [{atom(), [{node(), idle|running}], [{term(), node()}], [term()]}]
           }.
get_master_state(Timeout, DeloyFile) when Timeout > 0,
                                          is_list(DeloyFile) ->
    case get_master(DeloyFile) of
        {error, _Err} ->
            failed_reading_cfg;
        _ ->
            case is_alive(Timeout, DeloyFile) of
                master_unreachable ->
                    master_unreachable;
                {alive, State} ->
                    reform_state(State)
            end
    end.

%% create a custom slave group in leechland for specific use
-spec create_group(atom(), string(), string()) 
            -> ok|already_exist|master_unreachable.
create_group(GroupName, Pincode, DeployFile) when is_atom(GroupName),
                                                  is_list(Pincode),
                                                  is_list(DeployFile) ->
    Msg = {create_group, {self(),GroupName,term_to_binary(Pincode)}},
    send_to_master(DeployFile, Msg, 60*1000).

%% delete a custom group, all slaves belong to this group will be stopped 
%% all tasks for this group will be discarded, a group_is_gone msg will be
%% sent to all clients which are using this group
-spec delete_group(atom(), string(), string()) 
        -> ok|no_such_group|wrong_pincode|master_unreachable.
delete_group(GroupName, Pincode, DeployFile) when is_atom(GroupName),
                                                  is_list(Pincode),
                                                  is_list(DeployFile) ->
    Msg = {delete_group, {self(),GroupName,term_to_binary(Pincode)}},
    send_to_master(DeployFile, Msg, 60*1000).

%% check if the group is accessiable
-spec check_group(server(), atom(), string(), non_neg_integer()) 
        -> ok|no_such_group|wrong_pincode|master_unreachable.
check_group(Master, GroupName, PinCode, Timeout) when Timeout > 0 ->
    Msg = {check_group, GroupName, term_to_binary(PinCode), self()},
    send_to_master(Master, Msg, Timeout).

%% add slaves to group,how many slaves will be added on given server is decided 
%% by argument InstN
-spec add_server(string(), non_neg_integer(), string()) 
       -> [{node(), ok|failed}]|group_not_found|master_unreachable.
add_server(ServerName, InstN, DeployFile) when InstN > 0,
                                               is_list(ServerName),
                                               is_list(DeployFile) ->
    add_server(?GENSLAVE, ServerName, InstN, DeployFile).

-spec add_server(atom(), string(), non_neg_integer(), string()) 
        -> [{node(), ok|failed}]|group_not_found|master_unreachable.
add_server(GroupName, ServerName, InstN, DeployFile) when InstN > 0,
                                                          is_list(ServerName),
                                                          is_atom(GroupName),
                                                          is_list(DeployFile) ->
    {_,_,_,_,SlaveName,_,_} = fetch_deployinfo(DeployFile),
    Msg = {add_server, {self(),GroupName,SlaveName,ServerName,InstN}},
    send_to_master(DeployFile, Msg, 60*1000).

%% remove one server from slave group, all slaves belongs to this group on that
%% server will be stopped, tasks running on those slaves will be tranfered to 
%% other available slaves in this group
-spec remove_server(string(),string()) -> 
            ok|no_such_group|wrong_pincode|master_unreachable.
remove_server(ServerName,DeployFile) when is_list(ServerName),
                                          is_list(DeployFile) ->
    remove_server(?GENSLAVE, "", ServerName,DeployFile).

-spec remove_server(atom(),string(),string(),string()) -> 
            ok|no_such_group|wrong_pincode|master_unreachable.
remove_server(GroupName,Pincode,ServerName,DeployFile) 
    when is_atom(GroupName),
         is_list(Pincode),
         is_list(ServerName),
         is_list(DeployFile) ->
    Msg = {remove_server,{self(),GroupName,term_to_binary(Pincode),ServerName}},
    send_to_master(DeployFile, Msg, 60*1000).

%% send a list of task to master, master will dispatch task to slaves in the 
%% given group, note that before calling this function, function 'get_master'
%% must be called, which will set the cookie for communication.
%% information in callback_info are the application and path information which
%% will be used at slave side, slave will load the application specified in
%% callback_info and then use the callback module provided to process the task
%% so how task is handled depends on how given application is written, and 
%% callback module in callback_info must have an exported function 'call/1' and
%% an exported function 'stop/0', 'call/1' is used to handle task, 'stop/1' is
%% called when the group is deleted
-spec send(server(), [term()], #callback_info{})
   -> {ok,list()}|{error,term()}|{communication_lost,{list(),list()}}.
send(_, [], _) ->
    {ok, []};
send({Proc, Node} = Master, TaskList, CallbackInfo) 
    when is_atom(Proc),
         is_atom(Node),
         is_list(TaskList),
         is_record(CallbackInfo, callback_info) ->
    send(Master, ?GENSLAVE, "not_used", TaskList, CallbackInfo, infinity).

-spec send(server(), [term()], #callback_info{}, infinity|integer())
   -> {ok,list()}|{error,term()}|{communication_lost,{list(),list()}}.
send(_, [], _, _) ->
    {ok, []};
send({Proc, Node} = Master, TaskList, CallbackInfo, Timeout) 
    when is_atom(Proc),
         is_atom(Node),
         is_list(TaskList),
         is_record(CallbackInfo, callback_info),
         (Timeout > 0 orelse Timeout == infinity) ->
    send(Master, ?GENSLAVE, "not_used", TaskList, CallbackInfo, Timeout).

-spec send(server(), atom(), string(), [term()], #callback_info{}, infinity|integer())
   -> {ok,list()}|{error,term()}|{communication_lost,{list(),list()}}.
send(_, _, _, [], _, _) ->
    {ok, []};
send({Proc, Node}=Master, GroupName, PinCode, TaskList, CallbackInfo, Timeout) 
    when is_atom(Proc),
         is_atom(Node),
         is_atom(GroupName),
         is_list(PinCode),
         is_list(TaskList),
         is_record(CallbackInfo, callback_info),
         (Timeout > 0 orelse Timeout == infinity) ->
    Self = self(),
    Pid = spawn(fun()->send_request(Master, GroupName, PinCode, 
                                    TaskList, CallbackInfo, Self) end),
    receive_after(Timeout,fun()->timeout(Master, GroupName, PinCode, Pid) end).

%%----------------------------------------------------------------------------
%% internal send functions
%%----------------------------------------------------------------------------

-spec send_request(server(), atom(), string(), [term()], #callback_info{}, pid())
          -> no_return().
send_request(Master, GroupName, PinCode, TaskList, CallbackInfo, Parent) ->
    Ret =
    case check_group(Master, GroupName, PinCode, 60*1000) of
        ok ->
            send_request_int(GroupName, TaskList, CallbackInfo, 
                             Master, [], ?MAX_RETRY);
        Nok ->
            {error, Nok}
    end,
    Parent ! Ret.

-spec send_request_int(atom(),list(),#callback_info{},server(),list(),integer())
                       -> {ok, list()}|{communication_lost, {list(), list()}}.
send_request_int(_Group, [], _CallbackInfo, _Master, Result, _Retry) ->
    {ok, Result};
send_request_int(_Group, MissingTasks, _CallbackInfo, _Master, Result, 0) ->
    {communication_lost, {Result, MissingTasks}};
send_request_int(GroupName, TaskList, CallbackInfo, Master, Result, Retry) ->
    Client = {self(), node()},
    Master ! {client_request, {Client, GroupName, TaskList, CallbackInfo}},
    case receive_loop([]) of
        ?GROUP_GONE ->
            %% make it failed
            send_request_int(GroupName, TaskList, CallbackInfo, Master, [], 0);
        NewResult ->
            case check_result(NewResult, TaskList) of
                {StripedResult, []} ->
                    {ok, lists:append(Result, StripedResult)};
                {StripedResult, MissingTasks} ->
                    send_request_int(GroupName,MissingTasks,CallbackInfo,Master,
                                    lists:append(Result, StripedResult),Retry-1)
            end
    end.
    
-spec receive_loop(Data::term()) -> list()|group_is_gone.
receive_loop(Data) ->
    receive
        {result, Result} ->
            receive_loop([Result|Data]);
        {done, Result} ->
            [Result|Data];
        ?GROUP_GONE ->
            %% slave group has been deleted while processing client request
            ?GROUP_GONE
    end.

%%----------------------------------------------------------------------------
%% internal help functions
%%----------------------------------------------------------------------------

%% check integrity of results from master
-spec check_result([{integer(), term()}], [term()]) -> {list(), list()}.
check_result(Result, TaskList) ->
    StripedResult = [R || {_TaskId, R} <- Result],
    DoneList = lists:map(fun({N,_}) -> lists:nth(N, TaskList) end, Result),
    {StripedResult, TaskList--DoneList}.

%% once client is timeout, we notify master to abort all tasks for client
-spec timeout(server(), atom(), string(), pid()) -> {error, timeout}.
timeout(Master, GroupName, Pincode, Pid) ->
    Client = {Pid, node()},
    Master ! {abort, {GroupName, term_to_binary(Pincode), Client}},
    exit(Pid, kill),
    {error, timeout}.

-spec send_to_master(string(), term(), non_neg_integer()|infinity) -> any();
                    (server(), term(), non_neg_integer()|infinity) -> any().
send_to_master(DeployFile, Msg, Timeout) when is_list(DeployFile) ->
    {MasterNode,MasterProc,Cookie,_,_,_,_} = fetch_deployinfo(DeployFile),
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    Master = {list_to_atom(MasterProc), list_to_atom(MasterNode)},
    send_to_master(Master, Msg, Timeout);
send_to_master({Proc, Node} = Master, Msg, Timeout) when is_atom(Proc),
                                                         is_atom(Node) ->
    Master ! Msg,
    receive_after(Timeout, fun() -> master_unreachable end).

-spec receive_after(non_neg_integer()|infinity, fun()) -> any().
receive_after(Timeout, TimeoutFun) ->
    receive
        kill_all ->
            TimeoutFun();
        Ret ->
            Ret
    after Timeout ->
            TimeoutFun()
    end.

-spec check_process(pid(), non_neg_integer()) -> ok|timeout.
check_process(_, 0) ->
    timeout;
check_process(Pid, Times) ->
    case erlang:is_process_alive(Pid) of
        true ->
            timer:sleep(100),
            check_process(Pid, Times-1);
        false ->
            ok
    end.

%% used to refine the master state for better presenting
-spec reform_state(#main_state{}) 
          -> {
              node(),
              [{atom(), non_neg_integer()}],
              [{atom(), [{node(), idle|running}], [{term(), node()}], [term()]}]
             }.
reform_state(State) ->
    OverView = 
    [{Group#slave_group.group_type, length(Group#slave_group.slaves)}
     || Group<-State#main_state.slave_groups],
    {State#main_state.master_node, OverView, 
     [get_group_detail(GroupName, State)||{GroupName,_} <- OverView]}.

get_group_detail(GroupName, State) ->
    {ok, Group} = leechland_master:get_group(GroupName, State),
    SlaveInfo = 
      [{Slave#slave_info.node, Slave#slave_info.state} || 
       Slave<-Group#slave_group.slaves],
    OngnTasks = 
      [{(T#ongoing_task.task)#leech_task.data, T#ongoing_task.slave}
       ||T<-(leechland_master:get_ongnQ(GroupName, State))#ongoing_tasks.tasks],
    TaskInQ = 
      [T#leech_task.data || 
       T<-(leechland_master:get_taskQ(GroupName, State))#leech_tasks.tasks],

    {GroupName, SlaveInfo, OngnTasks, TaskInQ}.
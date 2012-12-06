-ifndef(LEECHLAND_HRL).
-define(LEECHLAND_HRL, true).

-export([fetch_deployinfo/1,
         write_to_deployfile_int/0,
         write_to_log/3]).

-define(MASTERPROC, leechland_lord). 
-define(SLAVEPROC, leechland_minion). 
-define(GENSLAVE, gen_slave). %% default slave group 
-define(GROUP_GONE, group_is_gone).
-define(MAX_RETRY, 5). %% retries for the case the communication lost between 
                       %% client and master
-define(DEPLOYFILE, "deployment.cfg"). 
-define(DBG(DebugInfo), ?DBG("~p", [DebugInfo])).
-define(DBG(Format, Args), spawn(fun()->write_to_log(Format, Args, ?LINE) end)).

%%% TYPE DEFINITION %%%
-type server() :: {pid()|atom()|string(), node()}.

-record(callback_info, {callbackModule::module(),
                        appName::atom(),
                        loadPath::string()}).

-record(leech_task, {client::server(),
                     taskId::integer(),
                     callback_info::#callback_info{},
                     data::term()}).

-record(leech_tasks, {group_type::atom(), tasks::[#leech_task{}]}).

-record(ongoing_task, {task::#leech_task{}, slave::node()}).

-record(ongoing_tasks, {group_type::atom(), tasks::[#ongoing_task{}]}).

-record(slave_info, {node::node(), state::running|idle}).

-record(slave_group, {group_type=?GENSLAVE::atom(), 
                      pincode::binary(), 
                      slaves=[]::[#slave_info{}]}).

-record(slave, {group_type::atom(), node::node()}).

-record(main_state, {master_node::node(),
                     slave_groups=[]::[#slave_group{}],
                     task_queue=[]::[#leech_tasks{}],
                     ongoing_queue=[]::[#ongoing_tasks{}],
                     removed_nodes_tmp=[]::[node()]
                    }).

-record(slave_state, {master::server(),
                      group_type::atom(),
                      initApps::[atom()],
                      initModules::[module()],
                      custom_module::module(),
                      custom_loadPath::string()}).

%%% COMMON FUNC %%%
write_to_deployfile_int()->
    {ok,[[Slave]]} = init:get_argument(slavename),
    {ok,[[LogPath]]} = init:get_argument(logs),
    {ok,[[ErlPath]]} = init:get_argument(pa),
    Content = lists:append(["{master_node, \"", atom_to_list(node()), "\"}.\n",
                            "{master_proc, \"", atom_to_list(?MASTERPROC), "\"}.\n",
                            "{cookie, \"", atom_to_list(erlang:get_cookie()), "\"}.\n",
                            "{slave_proc, \"", atom_to_list(?SLAVEPROC), "\"}.\n",
                            "{slave_name, \"", Slave, "\"}.\n",
                            "{logs, \"", LogPath, "\"}.\n",
                            "{erl_path, \"", ErlPath, "\"}.\n"]),
    file:write_file(?DEPLOYFILE, list_to_binary(Content)).

fetch_deployinfo(FileName) ->
    case file:consult(FileName) of
        {ok,[{master_node, MasterNode},
             {master_proc, MasterProc},
             {cookie, Cookie},
             {slave_proc, SlaveProc},
             {slave_name, SlaveName},
             {logs, LogPath},
             {erl_path, ErlPath}]} ->
            {MasterNode,MasterProc,Cookie,SlaveProc,SlaveName,LogPath,ErlPath};
        Error ->
            {error, Error}
    end.

write_to_log(Format, Args, Line) ->
    {ok,[[LogPath]]} = init:get_argument(logs),
    LogFile = lists:append([LogPath, "/", atom_to_list(node()), ".log"]),
    {ok, Fd} = file:open(LogFile, [append]),
    %% timestamp
    {_Date,{Hour,Minute,Second}} = erlang:localtime(),
    {_MegaSec,_Sec, Micro} = erlang:now(),
    TimeFormat = "~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0B ",
    io:fwrite(Fd,TimeFormat++"~p:~p:"++Format,
              [Hour,Minute,Second,Micro] ++ [?MODULE, Line] ++ Args),
    file:close(Fd).

-endif.

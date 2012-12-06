-module(leechland_slave).

-export([start/0]).
-include("leechland.hrl").

%%----------------------------------------------------------------------------
%%  Interface
%%----------------------------------------------------------------------------
start() ->
    ?DBG("slave starts on node:~p~n", [node()]),
    register(?SLAVEPROC, self()),
    {ok,[[Master]]} = init:get_argument(masterserver),
    {ok,[[Group]]} = init:get_argument(slave_group),
    MasterNode = list_to_atom(Master),
    SlaveGroup = list_to_atom(Group),
    {?MASTERPROC, MasterNode} ! {slave_up, 
                                 #slave{group_type=SlaveGroup, node=node()}},
    ?DBG("send slave up from ~p to ~p~n", 
         [node(), [{?MASTERPROC, MasterNode}]]),
    Apps = get_initial_apps(),
    Modules = get_initial_modules(),
    slave_loop(#slave_state{master={?MASTERPROC, MasterNode},
                            group_type=SlaveGroup,
                            initApps = Apps,
                            initModules = Modules}).

%%----------------------------------------------------------------------------
%%  Main
%%----------------------------------------------------------------------------
-spec slave_loop(#slave_state{}) -> #slave_state{}.
slave_loop(State) ->
    receive 
        {leech_task, Task} ->
            ?DBG("received leech_task:~p~n",[Task]),
            Cookie = erlang:get_cookie(),
            {Response, NewState} = process_data(Task, State),
            %% always set cookie back in case custom app changed cookie
            erlang:set_cookie(node(), Cookie),
            ?DBG("send response to master:~p~n",[Response]),
            State#slave_state.master ! {slave_response, Response},
            slave_loop(NewState);

        stop ->
            ?DBG("received stop msg from master~n"),
            %% tell custom application to stop
            Module = State#slave_state.custom_module,
            case code:is_loaded(Module) of
                false ->
                    ok;
                _ ->
                    Cookie = erlang:get_cookie(),
                    Module:stop(),
                    erlang:set_cookie(node(), Cookie)
            end,
            halt();

        Unexpected ->
            ?DBG("received unexpected msg:~p~n", [Unexpected]),
            slave_loop(State)
    end.

%%----------------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------------
-spec process_data(#leech_task{}, #slave_state{}) 
        -> {{#slave{}, server(), non_neg_integer(), any()}, #slave_state{}}.
process_data(Task, State) ->
    #leech_task{client=Client, taskId=TaskId,
                callback_info=CallbackInfo, data=Data} = Task,
    #callback_info{callbackModule = Module, appName = AppName,
                   loadPath = Path} = CallbackInfo,
    Slave = #slave{group_type=State#slave_state.group_type,
                   node=node()},
     
    case start_user_application(AppName, Path, State) of
        {error, Error} ->
            ?DBG("failed to start application ~p~n", [AppName]),
            {{Slave, Client, TaskId, Error}, State};
        ok ->
            ?DBG("application ~p is started~n", [AppName]),
            %% tell custom module to process data
            SafeRet = case catch Module:call(Data) of
                          Ret -> Ret end,
            {{Slave, Client, TaskId, SafeRet}, 
             State#slave_state{custom_module=Module,custom_loadPath=Path}}
   end.

-spec start_user_application(atom(), string(), #slave_state{}) 
        -> ok|{error, any()}.
start_user_application(AppName, Path, State) ->
    UserApps =
        [App||{App,_,_} <- application:loaded_applications()] 
        -- State#slave_state.initApps,
    case [A||A<-UserApps, A == AppName] of
        [] ->
            ?DBG("start application ~p~n", [AppName]),
            start_user_application_int(AppName, Path, State);
        [AppName] ->
            %% already there, no need to start it again
            ?DBG("application ~p found~n", [AppName]),
            ok
    end.

-spec start_user_application_int(atom(), string(), #slave_state{}) 
        -> ok|{error, any()}.
start_user_application_int(AppName, Path, State) ->
    case code:add_pathz(Path) of
        true ->
            %% stop last user application also unload all user modules from mem
            stop_user_application(State),
            Result = application:start(AppName),
            %% delete last time loaded custom code path
            code:del_path(State#slave_state.custom_loadPath),
            Result;
        Error ->
            Error
    end.

%% stop and unload all custom stuffs if it fails then crash the node
-spec stop_user_application(#slave_state{}) -> no_return().
stop_user_application(State) ->
    UserApps =
        [App||{App,_,_} <- application:loaded_applications()] 
        -- State#slave_state.initApps,
    ?DBG("found user applications ~p to stop~n", [UserApps]),
    lists:foreach(fun(App) -> 
                    ok = application:stop(App),
                    ok = application:unload(App) end, UserApps),
    UserModules =
        [Module||{Module,_} <- code:all_loaded()] 
        -- State#slave_state.initModules,
    ?DBG("found user modules ~p to delete~n", [UserModules]),
    lists:foreach(fun(M) -> 
                    true = code:delete(M) end, UserModules).

-spec get_initial_apps() -> [atom()].
get_initial_apps() ->
    [App||{App,_,_} <- application:loaded_applications()].

-spec get_initial_modules() -> [atom()].
get_initial_modules() ->
    [Module||{Module,_} <- code:all_loaded()].


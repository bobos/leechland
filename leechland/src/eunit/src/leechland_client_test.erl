-module(leechland_client_test).

-include_lib("eunit/include/eunit.hrl").
-include("leechland.hrl").
-include("leechland_test.hrl").

%==============================================================================
%  External APIs
%==============================================================================
%% use underscore to create a test expression to make timeout working
setup_env_test_() ->
    {timeout, 5, ?_assertEqual({ok, ?TEST_NODE_NAME}, 
            begin
                    test_master:start_master()
            end)}.

master_must_live_test_() ->
    {timeout, 5, ?_assertEqual({alive, state}, 
            begin 
                    leechland_client:is_alive() 
            end)}.

send_normal_test_() ->
    Tasks = [normal_task, task2, task3, task4],
    CallBack = {callback_info, module, app, "path"},
    {ok, Master} = leechland_client:get_master(),
    {timeout, 5, ?_assertEqual(lists:sort(Tasks), 
            begin 
                    {ok, Ret} = leechland_client:send(Master, Tasks, CallBack, 3000),
                    lists:sort(Ret)
            end)}.

send_empty_test_() ->
    Tasks = [],
    CallBack = {callback_info, module, app, "path"},
    {ok, Master} = leechland_client:get_master(),
    {timeout, 5, ?_assertEqual({ok, Tasks}, 
            begin 
                    leechland_client:send(Master, Tasks, CallBack, 3000)
            end)}.

send_disorder_test_() ->
    Tasks = [disorder_task, task2, task3, task4],
    CallBack = {callback_info, module, app, "path"},
    {ok, Master} = leechland_client:get_master(),
    {timeout, 5, ?_assertEqual(lists:sort(Tasks -- [disorder_task]), 
            begin 
                    {communication_lost, {[disorder_task], MissingTasks}}=
                    leechland_client:send_request_int
                      (?GENSLAVE, Tasks, CallBack, Master, [], 1),
                    lists:sort(MissingTasks)
            end)}.

%send_timeout_test_() ->
%    Tasks = [timeout_task, task2, task3, task4],
%    CallBack = {callback_info, module, app, "path"},
%    {timeout, 5, ?_assertEqual({error, timeout}, 
%            begin 
%                    leechland_client:send(Tasks, CallBack, 500)
%            end)}.

deployfile_test() ->
    Match = {atom_to_list(?TEST_NODE_NAME),atom_to_list(?MASTERPROC),
             atom_to_list(erlang:get_cookie()),
             atom_to_list(?SLAVEPROC),"slave","./","./ebin"},
    ?assertEqual(Match,fetch_deployinfo(?DEPLOYFILE)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

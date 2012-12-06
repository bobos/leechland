-module(leechland_server_gw).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(state, {pid}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% just in case big data sent to master, then old heap will pile up with a lot of useless data
    %% so set it to 10 to trigger full sweep as early as possible
    Pid = spawn_opt(leechland_master, start, [], [link, {fullsweep_after,10}]),
    {ok, #state{pid=Pid}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-module(test_multer).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         call/1,
         stop/0,
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call(Msg) ->
    gen_server:call(?MODULE, Msg, infinity).

stop() ->
    ok.

init([]) ->
    {ok, []}.

handle_call({L,R}, _From, State) ->
    timer:sleep(1000),
    {reply, L*R, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


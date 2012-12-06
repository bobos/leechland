-module(test_adder_client).
-export([add/3]).

add(GroupName, PinCode, List) ->
    {ok, Master} = leechland_client:get_master(),
    leechland_client:send(Master, GroupName, PinCode, List, 
                          {callback_info, test_adder, test_adder, "./ebin"}, infinity).

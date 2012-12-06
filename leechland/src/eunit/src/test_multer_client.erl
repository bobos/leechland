-module(test_multer_client).
-export([mult/3]).

mult(GroupName, PinCode, List) ->
    {ok, Master} = leechland_client:get_master(),
    leechland_client:send(Master, GroupName, PinCode, List, 
                          {callback_info, test_multer, test_multer, "./ebin"}, 4000).

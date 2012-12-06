-ifndef(LEECHLAND_TEST).
-define(LEECHLAND_TEST, true).

-define(TEST_NODE, "test_master").
-define(HOST, net_adm:localhost()).
-define(TEST_NODE_NAME, list_to_atom(?TEST_NODE++"@"++?HOST)).

-endif.

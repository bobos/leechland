-module(leechland_monitor).

-export([start/0]).

-include("leechland.hrl").
-define(MONITORDIR, "./monitor/").
-define(MONITORPAG, ?MONITORDIR++"leechland_monitor.html").

start() ->
    write_htm_head(?MONITORPAG),
    case leechland_client:get_master_state(10*1000, ?DEPLOYFILE) of
        Result when is_atom(Result) ->
            %% failed
            append_html(?MONITORPAG, list_to_binary(atom_to_list(Result)));
        State ->
            parse_state(State)
    end,
    write_htm_tail(?MONITORPAG).

write_htm_head(File) ->
    Head = 
    "<html>"
    "<head>"
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf8\">"
    "<title>Leechland Monitor</title>"
    "</head>"
    "<body>",
    file:write_file(File, list_to_binary(Head)).

write_htm_tail(File) ->
    Head = 
    "</body>"
    "</html>",
    append_html(File, list_to_binary(Head)).

% {
%  node(),
%  [{atom(), non_neg_integer()}],
%  [{atom(), [{node(), idle|running}], [{term(), node()}], [term()]}]
% }.
parse_state({Master, OverView, Detail}) ->
    MasterName =
    "<font size=2 color=red><i>Refreshing doesn't work on this page, yes, your F5 is not broken</i></font><hr>"
    "<font face=Tahoma size=5><b>Master Name:<font color=purple face=verdana>"
    ++atom_to_list(Master)++"</font></b>"
    "</font><hr>"
    "<table border=0.1>"
    "<tr>"
    "<th><font color=blue size=4>Group Name</font></th>"
    "<th><font color=green size=4>Number of Slaves</font><th>"
    "</tr>",
    append_html(?MONITORPAG,list_to_binary(MasterName)),
    write_detail(OverView, Detail),
    append_html(?MONITORPAG,list_to_binary("</table>")).

write_detail([], _) ->
    ok;
write_detail([{GroupName, SlaveNumber}|Rest], Detail) ->
    Link = 
    case lists:keyfind(GroupName, 1, Detail) of
        false ->
            "no_link";
        Find ->
            write_link_page(Find)
    end,
    TR =
    "<tr>"
    "<td bgcolor=lightblue size=4><a href=\""++Link++"\" title=\"Group Information\">"
    ++atom_to_list(GroupName)++"</td>"
    "<td bgcolor=lightgreen size=4>"++integer_to_list(SlaveNumber)++"</td>"
    "</tr>",
    append_html(?MONITORPAG,list_to_binary(TR)),
    write_detail(Rest, Detail).

write_link_page({GroupName, SlaveInfo, OngnQ, TaskQ}) ->            
    Link = ?MONITORDIR++atom_to_list(GroupName)++".html",
    write_htm_head(Link),

    %% slave info table
    Tab =
    "<font size=2 color=red><i>Refreshing doesn't work on this page, yes, your F5 is not broken</i></font><hr>"
    "<font color=purple face=verdana size=5><b>"
    ++atom_to_list(GroupName)++"</b></font>"
    "<table>"
    "<tr>"
    "<th><font color=brown>Slave Name</font></th>"
    "<th><font color=green>Slave State</font><th>"
    "</tr>",
    append_html(Link,list_to_binary(Tab)),
    write_slave_info(Link, SlaveInfo),
    append_html(Link,list_to_binary("</table>")),

    %% ongoing task table
    Tab1 =
    "<hr><table>"
    "<tr>"
    "<th><font color=brown>Ongoing Task</font></th>"
    "<th><font color=green>Slave Name</font><th>"
    "</tr>",
    append_html(Link,list_to_binary(Tab1)),
    write_ongnT_info(Link, OngnQ),
    append_html(Link,list_to_binary("</table>")),

    %% waitting task table
    Tab2 =
    "<p><font face=Tahoma size=4 color=red><b>"
    ++integer_to_list(length(TaskQ))++"</b></font>"
    "<font face=Tahoma size=4 color=grey> Tasks Remaining In Queue:</font><hr>"
    "<table>",
    append_html(Link,list_to_binary(Tab2)),
    write_tasks(Link,TaskQ),
    append_html(Link,list_to_binary("</table>")),
    write_htm_tail(Link),
    atom_to_list(GroupName)++".html".

% SlaveInfo = [{node(), idle|running}]
write_slave_info(_, []) ->
    ok;
write_slave_info(Link, [{Slave, State}|Rest]) ->
    TR =
    "<tr>"
    "<td bgcolor=lightblue>"++atom_to_list(Slave)++"</td>"
    "<td bgcolor=lightgreen>"++atom_to_list(State)++"</td>"
    "</tr>",
    append_html(Link,list_to_binary(TR)),
    write_slave_info(Link, Rest).

% OngnQ = [{term(), node()}]
write_ongnT_info(_, []) ->
    ok;
write_ongnT_info(Link, [{Task, Node}|Rest]) ->
    Formated = io_lib:format("~p",[Task]),
    TR =
    "<tr>"
    "<td bgcolor=lightblue rows=\"4\">"++Formated++"</td>"
    "<td bgcolor=lightgreen>"++atom_to_list(Node)++"</td>"
    "</tr>",
    append_html(Link,list_to_binary(TR)),
    write_ongnT_info(Link, Rest).

write_tasks(_,[]) ->
    ok;
write_tasks(Link,[Task|Rest]) ->
    Formated = io_lib:format("~p",[Task]),
    TR =
    "<tr>"
    "<td bgcolor=orange rows=\"2\"><font size=2>"++Formated++"</font></td>"
    "</tr>",
    append_html(Link,list_to_binary(TR)),
    write_tasks(Link, Rest).

append_html(File, Binary) ->
    file:write_file(File, Binary, [append]).

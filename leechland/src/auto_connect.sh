#!/usr/bin/expect 
 
set timeout 1 
set server [lindex $argv 0] 

spawn ssh $server
expect "(yes/no)?" 
send "yes\r" 
expect eof
exit

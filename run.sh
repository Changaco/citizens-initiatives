#!/bin/bash

domain="citizens-initiatives.changaco.net"

# Get a port number
old_pid="$(mkdir -p run; touch run/pid; cat run/pid)"
old_port="$(mkdir -p run; touch run/port; cat run/port)"
new_port=$(((old_port + 1) % 10 + 8840))

# Try to launch the app
logdir="$HOME/logs"
mkdir -p "$logdir"
logfile="$logdir/yesod-$new_port.log"
citizens-initiatives Production --port $new_port &>"$logfile" &
new_pid=$!
i=0
while true; do
    let i++
    sleep 1s
    out="$(curl -f http://$domain/ 2>/dev/null)"
    [ $? -eq 0 ] && break
    [ $i -gt 9 ] && cat "$logfile" && echo "$out" && exit 1
done

# Tell nginx to send request to the new port
sed -e "s/DOMAIN/$domain/g" -e "s/PORT/$new_port/g" -e "s|LOGDIR|$logdir|g" <config/nginx.conf.in >config/nginx.conf
nginx_pid="$(cat /run/nginx.pid)"
[ "$nginx_pid" != "" ] && kill -HUP $nginx_pid

# Kill the old process
[ "$old_pid" != "" ] && kill $old_pid
echo $new_pid >run/pid
echo $new_port >run/port

disown

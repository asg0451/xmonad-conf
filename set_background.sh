#!/usr/bin/bash
while true
do file=$(find ~/wallpaper/abstract/ | sort -R| head -n 1)
   feh --bg-scale "$file"
   sleep 10
done

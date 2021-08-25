#!/bin/sh
# Setting faster typing
xset r rate 200 75

# Converting caps lock to ctrl
setxkbmap -option "ctrl:nocaps"

# Startup picom
picom &

# Resotring previous wallpaper
# Not needed in the launcher script. Launching at exwm startup
# nitrogen --restore &

# Enable screen locking on suspend
xss-lock -- slock &

# Fire it up
# -mm - maximize window
# --debug-init - as init.el is loading if hit any errors drop into the debugger
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/Projects/super-emacs-econfig/desktop.el

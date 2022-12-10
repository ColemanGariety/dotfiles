# setxkbmap -option 'caps:escape' # for Kinesis Advantage2
setxkbmap -option 'altwin:ctrl_win,caps:escape' # for HHKB # ctrl:swapcaps
xrdb ~/.Xresources
xsetroot -xcf /usr/share/icons/Adwaita/cursors/left_ptr 32
xset r rate 200 50

# compton -o 0.25 -r 5 -l -3 -t -3 --backend xrender -I 1 -O 1 &
# feh --bg-fill /home/coleman/Images/joyful_and_peaceful_rest_meditation.jpg &
nitrogen --restore &
picom &
# gnome-panel &
xscreensaver -no-splash &
dunst &
conky &
# tint2 &
# pnmixer &
# ~/.bin/gitstatus-prompt-watcher &
# redshift -l 45.5155:122.6793 -t 6500K:3000K &
sxhkd &
thunar --daemon &
# exec bspwm -c ~/.config/bspwm/bspwmrc
exec karmen
# exec wind
# exec fvwm2
# exec twm

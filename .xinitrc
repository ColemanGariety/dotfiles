setxkbmap -option 'altwin:swap_alt_win,compose:ralt'
xrdb ~/.Xresources
# xsetroot -xcf /usr/share/icns/Adwaita/cursors/left_ptr 32
xset m 3/2 0
xset r rate 200 50

# compton -o 0.25 -r 5 -l -3 -t -3 --backend xrender -I 1 -O 1 &
# feh --bg-fill /home/coleman/Images/joyful_and_peaceful_rest_meditation.jpg &
nitrogen --restore &
picom -r 7 -o .33 -D 0 --no-fading-openclose --experimental-backends --vsync --backend glx --use-damage &
gnome-panel &
xscreensaver -no-splash &
dunst &
~/.bin/gitstatus-prompt-watcher &
# redshift -l 45.5155:122.6793 -t 6500K:3000K &
sxhkd &
# exec bspwm -c ~/.config/bspwm/bspwmrc
exec karmen
# exec wind

setxkbmap -option 'ctrl:ralt_rctrl,caps:escape,compose:rwin'
nitrogen --restore
xrdb ~/.Xresources
# xsetroot -xcf /usr/share/icons/Adwaita/cursors/left_ptr 32
xset m 3/2 0
xset r rate 200 30

# compton -o 0.25 -r 5 -l -3 -t -3 --backend xrender -I 1 -O 1 &
picom -r 7 -o .33 -D 0 --no-fading-openclose --experimental-backends --vsync --backend glx --use-damage &
dunst &
xscreensaver -no-splash &
emacs --daemon &
# conky &
redshift -l 45.5155:122.6793 -t 6500K:3000K &
sxhkd &
exec bspwm -c ~/.config/bspwm/bspwmrc
# exec karmen

setxkbmap -option 'altwin:swap_alt_win'
nitrogen --restore
xrdb ~/.Xresources
# xsetroot -xcf /usr/share/icns/Adwaita/cursors/left_ptr 32
xset m 3/2 0
xset r rate 200 50

# compton -o 0.25 -r 5 -l -3 -t -3 --backend xrender -I 1 -O 1 &
picom -r 7 -o .33 -D 0 --no-fading-openclose --experimental-backends --vsync --backend glx --use-damage &
dunst &
xscreensaver -no-splash &
emacs --daemon &
# redshift -l 45.5155:122.6793 -t 6500K:3000K &
sxhkd &
# exec bspwm -c ~/.config/bspwm/bspwmrc
exec karmen

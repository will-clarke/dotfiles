# adjust keyboard typematic delys and rate
xset r rate 270 30

# start xorg server at this dpi
xrandr --dpi 190

# mege and load config from .xresourse
xrdb -merge ~/.Xresources

# let QT and GTK autodetect retina sceen and autoadjust
export QT_AUTO_SCREEN_SCALE_FACTOR=1
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5

# xmodmap ~/.Xmodmap
# setxkbmap -option caps:escape

setxkbmap -option 'caps:ctrl_modifier'
xcape -e 'Caps_Lock=Escape' &

# finally start i3wm
exec i3


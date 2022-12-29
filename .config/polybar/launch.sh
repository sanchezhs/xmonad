#!/usr/bin/env sh

# More info : https://github.com/jaagr/polybar/wiki

# Install the following applications for polybar and icons in polybar if you are on ArcoLinuxD
# awesome-terminal-fonts
# Tip : There are other interesting fonts that provide icons like nerd-fonts-complete
# --log=error
# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

desktop=$(echo $DESKTOP_SESSION)
count=$(xrandr --query | grep " connected" | cut -d" " -f1 | wc -l)
screens=$(xrandr --listactivemonitors | grep -v "Monitors" | cut -d" " -f6)

if [[ $(xrandr --listactivemonitors | grep -v "Monitors" | cut -d" " -f4 | cut -d"+" -f2- | uniq | wc -l) == 1 ]]; then
  MONITOR=$(polybar --list-monitors | cut -d":" -f1) polybar --reload mainbar-xmonad -c ~/.config/polybar/config &
else
  primary=$(xrandr --query | grep primary | cut -d" " -f1)

  for m in $screens; do
    if [[ $primary == $m ]]; then
        MONITOR=$m polybar --reload mainbar-xmonad -c ~/.config/polybar/config &
    else
        MONITOR=$m polybar --reload secondbar-xmonad -c ~/.config/polybar/config &
    fi
  done
fi


#
#if [ $count = 1 ]; then
#  m=$(xrandr --query | grep " connected" | cut -d" " -f1)
#  MONITOR=$m polybar --reload mainbar-xmonad -c ~/.config/polybar/config &
#else
#  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
#    MONITOR=$m polybar --reload mainbar-xmonad -c ~/.config/polybar/config &
#  done
#fi





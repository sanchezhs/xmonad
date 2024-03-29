#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#Set your native resolution IF it does not exist in xrandr
#More info in the script
#run $HOME/.xmonad/scripts/set-screen-resolution-in-virtualbox.sh

#Find out your monitor name with xrandr or arandr (save and you get this line)
#xrandr --output VGA-1 --primary --mode 1360x768 --pos 0x0 --rotate normal
#xrandr --output DP2 --primary --mode 1920x1080 --rate 60.00 --output LVDS1 --off &
#xrandr --output LVDS1 --mode 1366x768 --output DP3 --mode 1920x1080 --right-of LVDS1
#xrandr --output HDMI2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off
xrandr --output HDMI-1 --primary --right-of HDMI-0

#(sleep 2; run $HOME/.config/polybar/launch.sh) & # starts polybar


#cursor active at boot
xsetroot -cursor_name left_ptr &

#start ArcoLinux Welcome App
run dex $HOME/.config/autostart/arcolinux-welcome-app.desktop

#Some ways to set your wallpaper besides variety or nitrogen
#feh --bg-fill /usr/share/backgrounds/archlinux/arch-wallpaper.jpg &
#feh --bg-fill /usr/share/backgrounds/arcolinux/arco-wallpaper.jpg &
nitrogen --restore /usr/share/backgrounds/3840x1080-Wallpaper-059.jpg &

#start the conky to learn the shortcuts
# (conky -c $HOME/.xmonad/scripts/system-overview) &

#starting utility applications at boot time
run nm-applet &
run pamac-tray &
run volumeicon &
numlockx on &
blueberry-tray &
#/home/samuel/.config/polybar/launch.sh
picom --config $HOME/.xmonad/scripts/picom.conf &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
# g413-led -fx breathing all ffffff 1fg413-led -fx breathing all ffffff 1f
#starting user applications at boot time
#nitrogen --restore &
#run caffeine &
#run vivaldi-stable &
#run firefox &
#run thunar &
#run spotify &
#run atom &

#run telegram-desktop &
#run discord &
#run dropbox &
#run insync start &
#run ckb-next -b &

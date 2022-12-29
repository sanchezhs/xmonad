Config { font            = "xft:Nerd:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Mononoki:pixelsize=10:antialias=true:hinting=true"
                           , "xft:Font Awesome 6 Free Solid:pixelsize=8"
                           , "xft:Font Awesome 6 Brands:pixelsize=8"
                           ]
       , bgColor      = "#2B2E37"
       , fgColor      = "#D8DEE9"
       -- Position TopSize and BottomSize take 3 arguments:
       --   an alignment parameter (L/R/C) for Left, Right or Center.
       --   an integer for the percentage width, so 100 would be 100%.
       --   an integer for the minimum pixel height for xmobar, so 24 would force a height of at least 24 pixels.
       --   NOTE: The height should be the same as the trayer (system tray) height.
       , position       = TopSize L 100 24
       , lowerOnStart = True
       , hideOnStart  = False
       , allDesktops  = True
       , persistent   = True
       , iconRoot     = ".xmonad/xpm/"  -- default: "."
       , commands = [
                        -- Cpu usage in percent
                     Run Cpu ["-t", "cpu: <fc=#AAC0F0><total></fc>%","-H","50","--high","red"] 20
                        -- Ram used number and percent
                    , Run Memory ["-t","Mem: <fc=#AAC0F0><usedratio></fc>%"] 10
                        -- Disk space free
                    , Run DiskU [("/", "ssd: <fc=#AAC0F0><free></fc> free")] [] 60
                        -- Time and date
                    , Run Date "%d %b %Y,  %H:%M " "date" 50
                        -- Script that dynamically adjusts xmobar padding depending on number of trayer icons.
                    , Run Com "/home/samuel/.xmonad/scripts/trayer-padding-icon.sh" [] "trayerpad" 20
                        -- Prints out the left side items such as workspaces, layout, etc.
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeStdinReader% }{  <fc=#5e81ac><action=`alacritty -e htop`>%cpu%</action></fc> <fc=#8fbcbb> | </fc> <fc=#5e81ac><action=`alacritty -e htop`>%memory%</action></fc> <fc=#8fbcbb> | </fc> <fc=#5e81ac>%disku%</fc> <fc=#8fbcbb> | </fc> <fc=#5e81ac><action=`emacsclient -c -a 'emacs' --eval '(doom/window-maximize-buffer(dt/year-calendar))'`>%date%</action></fc>%trayerpad%"
       }

Config { font            =   "Terminus 12"
       , additionalFonts = [ "Mononoki 10"
                           , "Font Awesome 6 Free Solid 8"
                           , "Font Awesome 6 Brands 8"
                           ]
       , bgColor = "#141319" -- "#2B2E37"
       , fgColor = "#929AAD"
       --, alpha = 100
       -- Position TopSize and BottomSize take 3 arguments:
       --   an alignment parameter (L/R/C) for Left, Right or Center.
       --   an integer for the percentage width, so 100 would be 100%.
       --   an integer for the minimum pixel height for xmobar, so 24 would force a height of at least 24 pixels.
       --   NOTE: The height should be the same as the trayer (system tray) height.
       , position       = TopSize L 100 28
       , lowerOnStart = True
       , hideOnStart  = False
       , allDesktops  = True
       , persistent   = True
       , iconRoot     = "/home/samuel/.xmonad/xpm/"  -- default: "."
       , commands = [
                     Run Date "%d %b %Y,  %H:%M " "date" 50
                    , Run Volume "default" "Master" [] 10
                    , Run Network "enp0s31f6" ["--template" , "↑<tx> kB/s ↓<rx> kB/s", "-L","0","-H","32",
                                          "--normal","gray","--high","gray"] 10
                    ,  Run UnsafeXPropertyLog "_XMONAD_LOG_0" -- Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %_XMONAD_LOG_0% } <fc=#5e81ac>%date%</fc> \
                   \{<fc=#5e81ac>%default:Master%</fc>\
                   \<fc=#5e81ac>%enp0s31f6%</fc> \
                   \%%"
       }
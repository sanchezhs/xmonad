Config { font            =   "Terminus 12"
       , additionalFonts = [ "Mononoki 10"
                           , "Font Awesome 6 Free Solid 8"
                           , "Font Awesome 6 Brands 8"
                           ]
       , bgColor = "#141319"-- "#2B2E37"
       , fgColor = "#929AAD"
       --, alpha   = 100
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
                    , Run UnsafeXPropertyLog "_XMONAD_LOG_1" -- Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %_XMONAD_LOG_1% }<fc=#5e81ac>%date%</fc>{"
       }

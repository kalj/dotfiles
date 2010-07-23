Config {
         font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
         -- font = "xft:Monospace-8"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 100
       , lowerOnStart = True
       , commands = [ Run Cpu ["-t","CPU: <total>","-L","3","-H","50","--normal","green","--high","red"] 10 
                    , Run Memory ["-t","Mem: <used> MiB"] 10
    		    , Run Date "%Y-%m-%d %R" "date" 10
                    , Run StdinReader
                    , Run Battery ["-t", "Bat: <left>", "-L","10","-H","50","--low","red","--normal","green"] 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %memory% | %battery% | %date% "
       }

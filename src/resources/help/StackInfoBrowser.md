## Stack Info Browser

### Introduction
The Stack Info browser presents the program's stack information for the current thread/process. The current thread/process is set by the Thread/Process Info browser.

Seer presents this information in three tabs:

* Frames
* Arguments
* Locals

### Frames
Frames is the most useful. It presents a stack frame (a traceback) for the selected thread.  

This information is shown for each Frame:
```
      Column       Description
      ----------   -----------------------------------------------------------------------
      Level        The frame level. 0 is the lowest level frame (ie: the executing frame).
                   1 is the caller of frame 0. 2 is the caller of 1. etc...
      Function     The function name for the level.
      File         The short name of the source file where Function is in.
      Line         The line number in the source file that is being executed.
      Fullname     The fullname of the source file.
      Address      The frame's address in memory.
```
Clicking on a Frame Level will cause Seer to make that frame the active frame. This will in turn cause the Editor Manager to bring up the source file for the frame (if possible).

Because the frame is set as the active frame, other Seer and gdb actions will default to that frame. For instance, entering a variable name in a Visualizer will use the variable in the active frame.

### Arguments
This tab shows the function argument names and their value for each level. For example, if level 0 is for main(), the arguments for level 0 will be the values for argc and argv.
```
      Column       Description
      ----------   ---------------------------------------------------------------------------
      Level        The frame level. 0 is the executing frame. 1 is the caller of frame 0. etc.
      Name         List of variables names at the frame level.
      Value        Value of the variables at the frame level.
```
Using the RMB will bring up a choice of Seer Visualizers and Loggers to further view the values. Note, you may need to set the level as active first in the Frames tab.

### Locals
This tab shows the local values in the selected frame. Local values means the variables of the execution line in that frame, plus any established variables before the current line.

Selecting a diffent frame in the Frames tab will show the locals for that frame.

```
      Column       Description
      ----------   -----------------------------------------------------------------------
      Name         The variable name.
      Arg          The function argument number, if any. May appear in the Arguements tab.
      Value        Value of the variable.
```
### Refresh
The refresh button refreshes the currently exposed tab.


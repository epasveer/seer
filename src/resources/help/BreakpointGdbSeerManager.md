## Breakpoint/GDB log Manager

### Introduction
This part of Seer shows Breakpoints, GDB log, and Seer log information. In detailed, the information is:

* Messages
* Breakpoints
* Watchpoints
* Catchpoints
* Printpoints
* Checkpoints
* GDB output
* Seer output
* Save and load breakpoints
* Manual GDB commands.

### Messages

Various gdb execution messages are listed in this tab. These occur as you debug the program. The message types are:

* Program startup and completion
* Breakpoint encouters
* Signal encounters
* General gdb errors

The message tab can be raised in various ways when a message is added to the tab:

* Any message
* Important messages (program startup, signal encounters)
* Never

### Breakpoints

Breakpoints sets up a stopping point of the program being debugged when a function and line number is reached.

### Watchpoints

Watchpoints catch the access and/or modification of a variable and will stop the program being debugged at that point.

### Catchpoints

Catchpoints catch the execution of one of these program actions just before they are called and will stop the program being debugged at that point.

* C++
    * throw
    * rethrow
    * catch

* Shared Library
    * Load
    * Unload

* Ada
    * assert
    * exception
    * handlers

There are other types of catchpoints but GDB/mi only supports the above list at the moment.

### Printpoints

A printpoint is a type of breakpoint that will print the value of a variable at a certain line of a function. It relies on gdb's ```dprintf``` feature.

### Checkpoints

A checkpoint is a simple form of time-travel debugging. You can create a checkpoint at any point where the program you're debugging
is stopped. This checkpoint is listed in the Checkpoints tab. You can continue to debug your program. At any time, you can return
back to the checkpoint (go back in time) and continue debugging again from that point.

You can create as many checkpoints as you want and switch between them.

A couple things to note:

* This is not supported on all platforms.
* Switching to a previous checkpoint does not undo any I/O. You can't unwrite data written to a file or unprint text sent to a printer.


### Modifying existing breakpoints.

Once a breakpoint (Breakpoint, Watchpoint, Catchpoint, Printpoint) is created, certain things about the breakpoint can be modified.
Not everything, though. For example, you can not change the function or line number for a breakpoint. In that case, you need to delete
the breakpoint and recreated it.

These things can be modified.

* Enable/disable state.
* Add or remove a condition. eg: Break if 'i == 10'.
* Add or remove an ignore count. eg: Ignore the first 5 occurences of the breakpoint.
* Add or remove a series of gdb commands to execute when the breakpoint is reached. Not available for Printpoints.


### GDB output

Any output from the GDB program is output to this logger. This is any regualar GDB output, including the result of any GDB command manually entered.

### Seer output

Any output from the Seer program is ouput to this logger. Mostly, this is the result of any GDB/mi command, whether the GDB/mi command is manually entered or entered by Seer. Normally this logger is disabled.

### Console output

Any output from the program being debugged is output to this logger. Basically, the program's stdout. The stdout can also be echoed
to the terminal that started Seer.

This logging tab can be started attached along side the other loggers. Or detached as a normal window. Or detached as a minimized
window. See Seer's config dialog or View->Console menu option. Be sure to save the config changes.

If the program being debugged is asking for stdin input, it can be entered in the "stding input text" field.

### Save and load breakpoints

There are two buttons to save or load the various types of breakpoints to/from a file. The breakpoint file can be specified in the Debug dialog or on the command line:
```
    --bl, --break-load <filename>
```
### Manual gdb commands

This input field allows GDB and GDB/mi commands to be manually entered. The results of the GDB commands will be logged to the GDB output. GDB/mi will be logged to the Seer output.

Seer maintains a history of N commands, as set in the Seer Config dialogs. This history is remembered for the next time Seer is used.

### Macro gdb commands

There are 10 macros you can create for custom commands. They are labelled "M1" through "M0". Think of your keyboard with numeric
keys 1 through 0. They have hotkeys mapped from Ctrl+Shift+1 through Ctrl+Shift+0. Clicking the button or using the hotkey will
execute the commands in the macro. Holding down the button will bring up an editor to edit the contents of the macro.

Typically the macro contains gdb commands. Output will go to the GDB output tab. The macro is executed using gdb's
"source" command, which can handle a lot off unique cases.

### References

Consult these gdb references

1. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Set-Breaks.html#Set-Breaks) Using Breakpoints.
2. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Set-Watchpoints.html#Set-Watchpoints) Using Watchpoints.
3. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Set-Catchpoints.html#Set-Catchpoints) Using Catchpoints.
4. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Dynamic-Printf.html#Dynamic-Printf) Using DPrintf for Printpoints.
5. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Checkpoint_002fRestart.html#Checkpoint_002fRestart) Using Checkpoints.


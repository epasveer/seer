## Breakpoint/GDB log Manager

### Introduction
This part of Seer shows Breakpoints, GDB log, and Seer log information. In detailed, the information is:

* Breakpoints
* Watchpoints
* Catchpoints
* Printpoints
* GDB output
* Seer output
* Save and load breakpoints
* Manual GDB commands.

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

There are other types of catchpoints but GDB/mi only supports the above list at the moment.

### Printpoints

A printpoint is a type of breakpoint that will print the value of a variable at a certain line of a function. It relies on gdb's ```dprintf``` feature.

### GDB output

Any output from the GDB program is output to this logger. This is any regualar GDB output, including the result of any GDB command manually entered.

### Seer output

Any output from the Seer program is ouput to this logger. Mostly, this is the result of any GDB/mi command, whether the GDB/mi command is manually entered or entered by Seer. Normally this logger is disabled.

### Save and load breakpoints

There are two buttons to save or load the various types of breakpoints to/from a file. The breakpoint file can be specified in the Debug dialog or on the command line:
```
    --bl, --break-load <filename>
```
### Manual gdb commands

This input field allows GDB and GDB/mi commands to be manually entered. The results of the GDB commands will be logged to the GDB output. GDB/mi will be logged to the Seer output.

Seer maintains a history of N commands, as set in the Seer Config dialogs. This history is remembered for the next time Seer is used.

### References

Consult these gdb references

1. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb/Set-Breaks.html#Set-Breaks) Using Breakpoints.  
2. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb/Set-Watchpoints.html#Set-Watchpoints) Using Watchpoints.  
3. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb/Set-Catchpoints.html#Set-Catchpoints) Using Catchpoints.  
4. [Link](https://sourceware.org/gdb/onlinedocs/gdb/Dynamic-Printf.html#Dynamic-Printf) Using DPrintf.  


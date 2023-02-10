## Seer debug modes

### Introduction
Seer supports most debugging modes that gdb provides. These are:

* Starting and running a program
* Attaching to a local process on the system
* Connecting to a gdbserver
* Loading a corefile leftover from a program that crashed

### What can you do?
Select the mode and enter the appropriate arguments and settings. For all modes,
you can specify 'pre' and 'post' native gdb commands.

* 'pre' - gdb commands to execute before the executable is loaded (or attach, or connect, or loading of corefile).
* 'post' - gdb commands to execute afterwards

Click OK and Seer will starting the debugging session.

### Sessions
A session file can be saved. The session file will save:

* Name of executable
* Name of symbol file
* Working directory
* All arguments and settings for the selected mode
* All 'pre' and 'post' commands

The session file can then be loaded the next time Seer is used. The session file has a simple Json layout and can be hand edited.


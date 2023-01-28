## Corefile debug mode

### Introduction

This mode allows Seer to debug corefiles. Corefiles are files that are the state of a process that exits unexepectedly
via a signal (SIGTERM, SIGSEGV, etc...)

### Requirements
In this mode, Seer needs:

* The executable
* Optional symbol file if the executable doesn't have debug information
* The corefile.

### What can you do?
In this mode, Seer will load the executable and the corefile. It will try to show the reason for the corefile (which signal).
Look for this in the "GDB output" tab.

Seer will also show the source and line number where the program crashed.

You won't be able to do any stepping. You can move through the stack and show values of variables and memory, though.

### References

Check your system's configuration to see how to enable corefile generation. Some distros have it disabled by default.
Also, corefiles may not be located in the directory where the program ran. They may be located in some system directory (eg:
/var/local/dumps). Check your distro.



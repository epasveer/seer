## Run debug mode

### Introduction
This mode allows Seer to debug a program by starting the program itself. This is the most typical
way of using Seer.

### Requirements
In this mode, Seer needs:

* The executable
* Optional symbol file if the executable doesn't have debug information
* Optional working directory to run the program from
* Arguments to run the program with
* Various Seer/gdb options to start the program by

### What can you do?
If 'start' is selected, the program is started and breaks in 'main'.

If 'run' is selected, the program is started and executes without any breakpoints unless they are
specified in the optional breakpoint file.

From this point, you can debug the process as normal (stepping and setting breakpoints, etc...)

### References


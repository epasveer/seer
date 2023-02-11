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
There are a couple gdb settings that you can choose from when running the executable.
The main one, though, is the initial breakpoint, which there are three ways:

* No initial breakpoint. The program is started and executes without any breakpoints unless they are specified in the optional breakpoint file.
* Break in 'main'. The program is started and breaks in the program's main function.
* Break in 'function'. The program is started and breaks in a named function or at an address.

From this point, you can debug the process as normal (stepping and setting breakpoints, etc...)


## Attach debug mode

### Introduction

This mode allows Seer to debug a locally running process by attaching to the PID.

### Requirements
In this mode, Seer needs:

* The executable
* Optional symbol file if the executable doesn't have debug information
* The PID of the process.

### What can you do?
In this mode, Seer will attach to the process and immediately interrupt it with a SIGINT.
Seer will show the source and line number where the program was interrupted. The program's stack and threads are shown.

From this point, you can debug the process as normal (stepping and setting breakpoints, etc...)

### References




## RR debug mode

### Introduction

This mode allows Seer to debug a local or remote RR replay session.

RR allows time-travel debugging. In that, you can step forward or backwards in the execution of program
being debugged.

gdb has its own notion of time-travel debugging. However, it differs from RR's method. With gdb, the
record of instructions is recorded as you are debugging the program.  With RR, the record of instructions
are made once when you run the program with 'rr record'. Seer/gdb can then connect to the recordings
via the 'rr replay -s' command.

Further information on RR is listed below in the references.


### Requirements
In this mode, Seer needs:

* Optional symbol file if the executable doesn't have debug information
* How to look for the RR replay session (machine and port)
* Optional gdb commands to execute before and after Seer connects to the RR replay session

### What can you do?
In this mode, Seer will connect to the RR replay session via its host:port. The program will be stopped befor 'main'.
At this point you can set breakpoints and tell Seer to 'continue' or any typical debugging command.

Seer will show a 'Direction' arrow on the toolbar. This will set the debugging direction to forward or reverse. And
affects all commands like: 'step', 'next', 'finish', etc.

A typical debugging sequence would look like:

```
    # Use RR to record all instructions for a program.
    $ rr record -n hellosegv one two three

    # Use RR to start a replay session on the previously create instructions. Pick a free port.
    $ rr replay -s 50505 -k
    Launch gdb with
    gdb '-l' '10000' '-ex' 'set sysroot /' '-ex' 'target extended-remote 127.0.0.1:50505' /nas/erniep/Development/seer/tests/hellosegv/hellosegv

    # In a separate terminal, use Seer to connect to that replay session port.
    $ seergdb --rr localhost:50505
```

Now use Seer as you normally would. Use the Direction arrow in the main toolbar to change debugging directions.

### References

Check references for RR and gdb.

* [RR website](https://rr-project.org)
* [RR github](https://github.com/rr-debugger/rr/wiki/Usage)
* [gdb's record mode](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Process-Record-and-Replay.html#index-record-mode)


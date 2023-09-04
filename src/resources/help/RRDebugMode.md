## RR debug mode

### Introduction

This mode allows Seer to debug a RR replay session.

RR allows time-travel debugging. In that, you can step forward or backwards in the execution of program
being debugged.

gdb has its own notion of time-travel debugging. However, it differs from RR's method. With gdb, the
record of instructions is recorded as you are debugging the program.  With RR, the record of instructions
are made once when you run the program with 'rr record'. Seer can then load the recording (aka: trace-directory)
by running the 'rr replay -s' command.

Further information on RR is listed below in the references.


### Requirements
In this mode, Seer needs:

* Optional symbol file if the recording doesn't have debug information
* A path to the directory containing the RR recording (aka: trace-directory)
* Optional gdb commands to execute before and after Seer connects to the RR replay session

See the RR config settings set the path to the RR program.
```
   Settings -> Configure -> RR
```

### What can you do?
In this mode, Seer will load the RR replay session by running 'rr replay'. The program will be stopped befor 'main'.
At this point you can set breakpoints and tell Seer to 'continue' or any typical debugging command.

Seer will show a 'Direction' arrow on the toolbar. This will set the debugging direction to forward or reverse. And
affects all commands like: 'step', 'next', 'finish', etc.

A typical debugging sequence would look like:

```
    # Use RR to record all instructions for a program.
    # The recording will be saved to 'hellosegv.rr'
    $ rr record -n --output-trace-dir=hellosegv.rr hellosegv one two three

    # Use Seer to debug this recording. Can be done as many times as you
    # want without creating a new recording.
    $ seergdb --rr hellosegv.rr
```

Now use Seer as you normally would. Use the Direction arrow in the main toolbar to change debugging directions.

### References

Check references for RR and gdb.

* [RR website](https://rr-project.org)
* [RR github](https://github.com/rr-debugger/rr/wiki/Usage)
* [gdb's record mode](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Process-Record-and-Replay.html#index-record-mode)


## Tracepoints

### Introduction

A tracepoint records data when it is reached and lets the program continue. Each hit saves a
snapshot (gdb calls it a trace frame) that can be looked at later. Compare this to a breakpoint,
which stops the program so it can be looked at now.

### Requirements

Tracing needs a target that supports it. Gdb's native target doesn't. Normally, use a
```gdbserver``` and Seer's ```Connect``` mode. It can be on the same machine.

```
gdbserver localhost:1234 myprogram
```

### Creating tracepoints

Add a tracepoint from the Tracepoints tab or by right-clicking on a line in the source. Besides
a location, a tracepoint can have:

* Condition. Only record if the condition is true.
* Pass Count. Stop tracing after this many hits.
* Actions. What to record, one action per line. Anything not collected shows as ```<unavailable>```.

Some actions:

```
collect $args, $locals
collect v[0]@8
collect $regs
```

A fast tracepoint is quicker but the program must be using gdb's in-process agent library.

### Tracing

* ```Start``` begins tracing (tstart). Snapshots from an earlier trace are discarded.
* Continue the program. A breakpoint can be used to stop it.
* ```Stop``` ends tracing (tstop). Tracing also ends when a pass count is reached or the buffer is full.

Start and Stop while the program is paused.

### Looking at snapshots

The ```TracepointOutput``` tab shows the tracepoint that is selected in the ```Tracepoints``` tab. The
```Show``` button goes there. Another tracepoint can be picked from the list on the left. Its snapshots
are listed next to it. The buttons are along the top, to the left of the help button.

Click on a snapshot, or use the ```|<  <  >  >|``` buttons, to look at it (tfind). These only move
between the snapshots of this tracepoint. The collected data is listed and Seer's stack, variable,
and register browsers show the snapshot too. The program itself doesn't move. Use ```Go``` to select
any snapshot by its number.

Seer finds which snapshots belong to the tracepoint by visiting them. With more than 100, it waits
for the ```Load all``` button.

```Live``` returns to the program. Seer does this automatically when the program is stepped or continued.

```Save...``` writes the snapshots to a file (tsave). Use gdb's ```target tfile``` command to read it.

### References

Consult gdb's documentation on [tracepoints](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Tracepoints.html).

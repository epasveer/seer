# Debugging pipower with simavr

Note that when building in this directory, various state timers are redefined to have shorter periods. The code built for simulation should not be deployed onto an actual device.

## Manual debugging

1. Run `make` in the `sim` directory. This will build a version of `pipower` with debugging symbols enabled.

2. Start `simavr`:

        simavr -m attiny85  -f 1000000 pipower.elf  -t -g

3. Connect to `simavr` with `avr-gdb`:

        $ avr-gdb -se pipower.elf
        (gdb) target remote :1234
        (gdb) load
        (gdb) b loop
        Breakpoint 1 at 0xaa: file ../pipower.c, line 115.
        (gdb) c
        Continuing.

        Breakpoint 1, loop () at ../pipower.c:115
        115         now = millis();

## Gathering traces

1. Run `make clean all TRACE=1` in the `sim` directory. This will include `simavr.c`, which sets metadata describing trace data we wish to collect with `simavr`.

2. Use `avr-gdb` to run `simulate.gdb`:

        avr-gdb -x simulate.gdb

  (You could of course just start a manual debugging session, as in the earlier example.)

This will generate `gtkwave_trace.vcd`, which you can view in `gtkwave` by running:

    gtkwave gtkwave_trace.vcd

You can view an existing trace by running `gtkwave` against the included save file:

    gtkwave pipower.gtkw


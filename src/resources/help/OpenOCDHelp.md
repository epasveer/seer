## OpenOCD debug mode

### Introduction

This mode lets Seer debug a target over JTAG/SWD via [OpenOCD](https://openocd.org), instead of a
local process or a gdbserver. Seer starts (or connects to) `openocd`, then attaches gdb to it as a
remote target. This is the mode used for bare-metal and Linux-kernel debugging on embedded boards
(e.g. Raspberry Pi 4 / BCM2711) using a JTAG adapter.

### Requirements

In this mode, Seer needs:

* Path to the `openocd` executable, e.g. `/usr/local/bin/openocd`
* OpenOCD options/arguments (board/target `.cfg` file, adapter speed, transport, etc.), e.g.:
  ```
  -f board/rpi4b.cfg
  ```
  or, with SMP enabled (see "Multi-core (SMP) targets" below):
  ```
  -c "set USE_SMP 1" -f board/rpi4b.cfg
  ```
* Optional executable/symbol file if not already loaded, e.g. a `vmlinux` with debug info for
  kernel debugging
* Optional source directory, e.g. the kernel source tree the `vmlinux` was built from
* Optional load address, if the symbol file needs one
* gdb executable capable of talking to the target's architecture, e.g. `gdb-multiarch` for an
  aarch64 target debugged from an x86_64 host
* OpenOCD's gdb port (default `3333`) and telnet port (default `4444`)

Example full command line Seer ends up running for a Raspberry Pi 4:

```
openocd -f board/rpi4b.cfg -c "gdb_port 3333" -c "telnet_port 4444"
```

### What can you do?

Once connected, you debug as normal: set breakpoints, step, inspect memory/registers. Two things
are specific to this mode:

* **Hardware breakpoints only.** Kernel/module `.text` on most Linux targets is mapped read-only,
  so software breakpoints (which patch the instruction stream) are unreliable there. Always use
  `hbreak`/`thbreak`, never `break`/`tbreak`, when debugging kernel code through OpenOCD. Example:
  ```
  (gdb) hbreak load_module
  Hardware assisted breakpoint 1 at 0xffffffc0080d4ed0: file kernel/module.c, line 3521.
  ```
* **Debug on Init.** For debugging a *loadable kernel module* from the moment it's inserted
  (`insmod`), use the "Debug on Init" dialog:

  | Field | Example value |
  |---|---|
  | Module name | `helloworld` |
  | Serial device | `/dev/ttyUSB0` |
  | Module symbol file (`.ko` with debug info) | `.../helloworld_owrt/helloworld.ko` |
  | Kernel source directory | `.../linux-5.15.132/` |
  | Command to load the module | `insmod helloworld.ko` (auto-filled from the module name; edit it if you need `taskset`, see below) |

  Seer arms a breakpoint inside `load_module()`, sends the load command over the target's serial
  console, waits for the stop, reads the module's real (randomized) section addresses from the
  kernel's `mod->sect_attrs`, loads the module's symbols at those addresses with `add-symbol-file`,
  and sets a breakpoint at the module's init function (`helloworld_init` in this example). See
  `src/resources/mi-python/MIDebugOnInit.py`.

### Multi-core (SMP) targets

On a multi-core target (e.g. a 4-core Cortex-A72 board like the BCM2711 on a Raspberry Pi 4), a
hardware breakpoint set by gdb is inserted **only on the single OpenOCD target that's currently
selected** unless the board's OpenOCD config groups the cores together with `target smp`. Most
stock board `.cfg` files (e.g. `target/bcm2711.cfg`) gate this behind a `USE_SMP` Tcl variable that
**defaults to off**:

```tcl
if { [info exists USE_SMP] } {
    set _USE_SMP $USE_SMP
} else {
    set _USE_SMP 0        ;# <- off unless you set it yourself
}
...
if {$_USE_SMP} {
    eval $_smp_command    ;# only now does "target smp cpu0 cpu1 cpu2 cpu3" run
}
```

If it's off and the code you're trying to catch (e.g. `load_module()` during `insmod`, or a
syscall/sysfs handler inside your module) happens to run on a different core than the one gdb is
attached to, the breakpoint is silently never hit — this looks exactly like a flaky/unreliable
breakpoint, but it isn't; the code just ran on a core that never had the breakpoint installed, and
the target simply goes back to idle once the code finishes.

**How to confirm this is what's happening:** connect to OpenOCD's telnet console (default port
`4444`, e.g. `telnet localhost 4444`) while the breakpoint is armed, and list breakpoints per core
(`bp` always reports on whichever target is currently selected, so select each core in turn):

```
> targets bcm2711.cpu0
> bp
Hardware breakpoint(IVA): addr=0xffffffc0080d6a70, len=0x4, num=1
> targets bcm2711.cpu1
> bp
> targets bcm2711.cpu2
> bp
> targets bcm2711.cpu3
> bp
```

Here only `cpu0` lists the breakpoint's address — `cpu1`/`cpu2`/`cpu3` print nothing at all. That's
the confirmation: the breakpoint exists on exactly one of the four cores, so it only fires when the
Linux scheduler happens to run the target code on that specific core.

Two ways to deal with this, with different trade-offs:

* **Pin the caller with `taskset` (recommended default).** If the code you're breaking on runs
  synchronously inside a userspace process/syscall you control, run that process with
  `taskset -c 0 <command>` to hard-pin it to the same core the breakpoint is armed on (core 0,
  OpenOCD's default selected target — `taskset` sets a *hard* CPU affinity, so the scheduler can't
  move the process even across a blocking syscall). This needs no board-specific setup and has no
  crash risk, but only works for code reachable synchronously from a process you can pin — not
  interrupt handlers, workqueues, kthreads, or a bug that only reproduces with genuine multi-core
  parallelism. Examples, all pinned to core 0 to match a breakpoint armed on `cpu0`:
  ```bash
  # Loading the module (this is what Debug on Init's "Command" field sends over serial):
  taskset -c 0 insmod helloworld.ko

  # Unloading it - breakpoint on helloworld_exit():
  taskset -c 0 rmmod helloworld

  # A sysfs file the module exposes - breakpoint on fibonacci_show()/fibonacci_store():
  taskset -c 0 cat /sys/kernel/helloworld/fibonacci
  taskset -c 0 bash -c 'echo 10 > /sys/kernel/helloworld/fibonacci'
  ```
  Note the last example: `echo N > file` redirection is performed by the *current shell*, not by
  `echo` itself, so `taskset -c 0 echo N > file` would only pin the (irrelevant) `echo` builtin.
  Wrap it in `bash -c '...'` so the process actually doing the `open()`/`write()` is the one that's
  pinned.
* **Enable `target smp` in the board's OpenOCD config** (e.g. set `USE_SMP 1` before the board
  `.cfg` is sourced):
  ```
  openocd -c "set USE_SMP 1" -f board/rpi4b.cfg
  ```
  This correctly fans a hardware breakpoint out to every core in the group (confirmed in OpenOCD's
  own source, `src/target/breakpoints.c`, `breakpoint_add()`), so it's hit no matter which core runs
  the code — no `taskset` needed anywhere. The trade-off: OpenOCD cross-halts *every* core in the
  group whenever any one of them stops (a manual halt or a breakpoint hit), via the board's
  Cross-Trigger Interface (CTI). Freezing every core for the duration of a debug session can trip
  the target board's **hardware watchdog** — a silicon-level timer chip, unrelated to the Linux
  kernel. This is a real, observed failure mode, not a theoretical one: on a Raspberry Pi
  4/OpenWrt test target, enabling SMP caused the board to hard-reset a few seconds after any halt,
  because OpenWrt's `procd` runs a `watchdogd` process that pets the BCM2835 hardware watchdog —
  and that process is itself frozen along with every other core while the debugger has the target
  halted:
  ```
  $ dmesg | grep -i watchdog
  [    0.470921] bcm2835-wdt bcm2835-wdt: Broadcom BCM2835 watchdog timer
  $ ps | grep watchdog
     65 root         0 SW   [watchdogd]
  ```
  Kernel cmdline flags like `nowatchdog`, `nosoftlockup`, or `rcupdate.rcu_cpu_stall_suppress=1`
  do **not** fix this — those only affect Linux's own software lockup/stall detectors, which may not
  even be compiled into the kernel (confirmed here: `dmesg` reported `nowatchdog`/`nosoftlockup` as
  *"Unknown kernel command line parameters"*, i.e. `CONFIG_LOCKUP_DETECTOR` wasn't built). The reset
  comes from the separate hardware watchdog chip counting down regardless of what the Linux kernel
  is configured to do.

  If you use `target smp`, disable (or extend the timeout of) the target's hardware watchdog
  *before* starting the debug session. How to do this is entirely board/OS-specific:
  ```bash
  # OpenWrt (procd):
  kill $(pidof watchdogd)

  # If the watchdog driver doesn't allow a plain close to disarm it (CONFIG_WATCHDOG_NOWAYOUT),
  # send the magic-close character first:
  python3 -c "
  import os
  fd = os.open('/dev/watchdog', os.O_WRONLY)
  os.write(fd, b'V')
  os.close(fd)
  "

  # systemd-based target:
  systemctl stop watchdog
  ```
  Some boards have no hardware watchdog at all, in which case none of this applies. Seer does not
  attempt to detect or disable a target's watchdog for you — this is left to your board's setup
  documentation/scripts, since watchdog presence, register layout, and the daemon managing it are
  entirely target-specific.

### Debugging interrupts, workqueues, and kthreads

`taskset` only pins a *userspace process* — it works for anything your module does synchronously
inside a syscall/VFS op triggered by a process you control (`insmod`, `rmmod`, a `cat`/`echo`
against a sysfs file, an ioctl from your own test program, etc.), including a module that's already
loaded and running (found via `/sys/module/<name>/sections/...` and loaded manually with
`add-symbol-file`/`hbreak`, without using Debug on Init at all) — the core that ran the module's
`init` doesn't matter once it's loaded; only the core that will run the code you're about to break
on matters. But `taskset` has nothing to pin for interrupt handlers, workqueue callbacks, or a
module's own kernel threads, since there's no userspace process to point it at. Same underlying
idea (force the code onto core 0, the one the breakpoint is armed on), different mechanism per
subsystem:

* **Interrupt handlers.** Pin the IRQ itself, not a process, via `/proc/irq/<N>/smp_affinity`:
  ```bash
  cat /proc/interrupts                    # find <N> for your driver, by name
  echo 1 > /proc/irq/<N>/smp_affinity     # bitmask 1 = CPU0 only
  ```
  Not all interrupts allow this — some are hardware-pinned per core (timers, IPIs) and reject a
  changed affinity. Ordinary peripheral IRQs (GPIO, UART, network, etc.) normally accept it.

* **Workqueues.** Depends on how the module queues work:
  * `schedule_work()`/`queue_work()` on a normal (non-`WQ_UNBOUND`) workqueue: by default the work
    item runs on the *same core that queued it*. If you control/pin whatever triggers the
    `schedule_work()` call (a process via `taskset`, or an IRQ via `smp_affinity` above), the work
    callback follows it onto the same core — no extra step needed.
  * A module-private workqueue created with `WQ_SYSFS`: it exposes
    `/sys/devices/virtual/workqueue/<name>/cpumask` — write a mask to restrict which cores it's
    allowed to use.
  * `WQ_UNBOUND` with no `WQ_SYSFS` cpumask: not pinnable from userspace at all; see `target smp`
    below.

* **The module's own kthreads** (`kthread_run()`/`kthread_create()`). These have a normal PID, so
  pin them exactly like any process, just with `-p` instead of a command to launch:
  ```bash
  ps | grep <thread name>
  taskset -p 0x1 <pid>          # pin to CPU0
  ```

* **Nothing above applies** (an unpinnable `WQ_UNBOUND` workqueue, a hardware-pinned interrupt, or a
  bug that only reproduces with genuine multi-core parallelism, where forcing everything onto one
  core would hide it). This is the case `target smp` exists for — see "Multi-core (SMP) targets"
  above, including the hardware-watchdog trade-off. There is no way to get breakpoint coverage on
  every core without also getting the whole-group cross-halt that comes with it; picking per-context
  pinning above versus `target smp` here is a real trade-off, not a Seer limitation.

### References

* [OpenOCD User's Guide](https://openocd.org/doc/html/index.html)
* [OpenOCD SMP / multi-core debugging](https://openocd.org/doc/html/Architecture-and-Core-Commands.html)
* Your board's OpenOCD target `.cfg` file, for its `USE_SMP` (or equivalent) setting
* Your board's watchdog hardware and the OS/init system that pets it (if you plan to use
  `target smp`)

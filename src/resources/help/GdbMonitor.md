## GDB Monitor

### Introduction

The GDB monitor is used when gdb is communicating with some gdb "server", like the gdbserver program. The monitor provides extra
commands that are directed to the "server" and not to the executable the "server" is running.

For example, when using gdbserver as the "server", you can send the "monitor exit" command. It will tell gdbserver to exit.

There are other types of "servers" other than gdbserver. For example, valgrind's vgdb server has its own set of commands. And they
are different to the commands that gdbserver understands. OpenOCD is another "server" that is used when debugging Embedded MCUs. It,
again, has its own set off commands.

The rocgdb debugger is based on gdb. It has additions to gdb's monitor command.
```
(gdb) monitor set rocm
```

One thing in common is they all seem to accept the "help" command. Which will print the commands and options that "server" supports.

```
(gdb) monitor help
```

### The View.

The GDB monitor view contains 4 main sections.

* A large text area that contains the text recieved from the "server".
* 10 customizable macros.
* A input field to easyly enter a command for that "server".
* Clear, save, and print buttons.

All responses from the "server" are printed to the bottom of the large text area. Any command that is sent to the "server" (using a macro or
entering it from the input field) is first printed to the text area.

Macros are created by holding the macro button down and selecting "Edit Macro". Enter the "server" command you wish the macro to
perform then click "Ok". Note, you don't need to enter "monitor help". Just "help" is expected. Set up macros for your commonly used
commands.

The input field can be used for other monitor commands. Again, just type "help" and not "monitor help".

The large text area can be cleared, saved to a file, or printed.

### Resources

https://sourceware.org/gdb/current/onlinedocs/gdb.html/Server.html  
https://openocd.org/doc/html/General-Commands.html  


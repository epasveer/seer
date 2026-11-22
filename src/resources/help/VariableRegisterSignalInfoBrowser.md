## Variable/Register/Signal Info Browser

### Introduction

The Variable/Register/Signal Info browser is a simple, but quick, method of viewing the values of the program's variables.

Seer presents this information in four tabs:

* Logger
* Tracker
* Registers
* Signals

### Logger
The logger is a simple method for printing the value of a variable. There is an entry field to manually enter the name of the variable. The variable must be part of the active stack frame, as selected by the Stack Info browser.

This information is shown for each variable.
```
      Column            Description
      ---------------   ----------------------------------------------
      Timestamp         When the variable was logged.
      Name              The name of the variable.
      Value             The value of the variable.
```

The easiest method for logging a variable is to double-click the variable text in the code editor. When doing this, a key modifier (shift and/or ctrl) can be used to prepend a '*' and/or '&' to the variable. This will better handle pointers and references.
```
      Double-Click
      Modifier      Description                           Example
      ------------  ------------------------------------  -----------------------
      None          No modifier is prepended to variable  'argc'
      Shift Key     Prepend '&' to variable               '&argc'
      Ctrl Key      Prepend '*' to variable               '*argc'
      Ctrl+Shift    Prepend '*&' to variable              '*&argc'
```

A format suffix can be added to the end of the variable name to print its value in a different format. For structures, classes, and arrays, each member is printed in the chosen format.
```
      Suffix   Description                           Example
      -------  ------------------------------------  -----------------------
      None     Default format (see below)            'argc'
      :n       Natural format (gdb's default)        'argc:n'
      :x       Hexadecimal                           'argc:x'
      :o       Octal                                 'argc:o'
      :d       Decimal                               'argc:d'
      :t       Binary                                'argc:t'
      :f       Float                                 'argc:f'
```

The format button next to the entry field sets the default format. It is used for any variable entered without a format suffix, including variables added by double-clicking in the code editor. It starts as 'Natural' and is remembered between sessions. A suffix always overrides the default. Use ':n' to log a variable in the natural format when the default is set to something else.

Some types stay readable in every format:
```
      Type                       Printed as
      -------------------------  ------------------------------------------------
      Pointers                   Always a hex address. 'char*' also shows its text.
      char arrays                Always a string.
      std::string, etc.          Always a string (handled by gdb's pretty printers).
      float, double              Their raw IEEE bits. '1.5' with ':x' is '0x3fc00000'.
                                 With ':f', they print normally.
      unsigned with ':d'         Unsigned decimal, so large values aren't negative.
```

The ':f' suffix works like gdb's 'print/f'. It does not convert an integer to a float. Instead, the integer's bits are read as a float of the same size. This is the reverse of ':x' on a float, and is useful for integers that hold raw float bits. An ordinary integer will print as a strange value.
```
      Variable                                   With ':f'
      -----------------------------------------  ----------------
      int bits = 0x3fc00000                      1.5
      uint64_t dbits = 0xc002000000000000        -2.25
      int i = 42                                 5.88545355e-44
      char c = 'a'                               97
```

There are buttons to clear some or all entries in the logger.

### Tracker
The tracker is a method to watch several variables at a time. Manually enter a list of variable names. Their values will be updated in the Logger display at each stopping point (after a 'step' or 'next' or a breakpoint) is reached.

The variable must be part of the active stack frame, as selected by the Stack Info browser.

The same format suffixes as the Logger (':x', ':o', ':d', ':t', ':f', ':n') can be added to a variable name. The suffix stays with the variable, so it is printed in that format at each stopping point.

The Tracker also has a format button that sets the default format for variables tracked without a suffix. Changing it updates those variables right away. It starts as 'Natural' and is remembered between sessions. Use ':n' to keep a variable in the natural format when the default is set to something else.

There are buttons to clear some or all variables in the tracker.


### Registers
Registers is a view of the program's register values. The register values are updated at each stopping point (after a 'step' or 'next' or a breakpoint) is reached.

This information is shown for each register.
```
      Column            Description
      ---------------   ----------------------------------------------
      Name              The name of the register.
      Value             The value of the register.
```

The register value can be printed in multiple formats.
```
      Format             Description
      ---------------   ------------------------------------------------------------
      Natural           Print using a format that's best for the register's purpose.
      Hex               Print in hex.
      Octal             Print in octal.
      Binary            Print in binary.
      Decimal           Print in decimal.
      Raw               Print in the raw form.
```

Register values can be modified. Double-click on a Value column for a register will allow the value to be edited and changed.  Using a RMB on a register will bring up a menu to change the register value as well. This second method can better handle resisters that have 'union' like properties (eg: xmm0 with xmm0.v8_bfloat16, xmm0.v4_float, etc...) where the register name needs to be modified slightly to include the 'union' field name.

Register profiles can be created. A profile selects which registers to actually show. Multiple profiles are allowed. You can create
new ones and modify existing ones. There is one profile that is special. It's called 'allregisters'. It means show all
registers. It can't be modified or deleted.

Switching between register profiles is easily done using the "Profile" list selector.

### Signals
Signals is a view of the program's signal settings and how gdb handles them. The handling can be modified here.

This information is shown for each signal.
```
      Column            Description
      ---------------   ----------------------------------------------
      Name              The name of the signal.
      Stop              GDB should stop your program when this signal happens.
                        This implies the print keyword as well.
      Print             GDB should print a message when this signal happens.
      Pass              GDB should allow your program to see this signal; your program can handle the signal,
                        or else it may terminate if the signal is fatal and not handled. pass and noignore are synonyms.
      Description       The description of the signal.
```

### References

Consult these gdb references

1. [Link](https://sourceware.org/gdb/onlinedocs/gdb/Registers.html) Viewing/setting register values.
2. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Signals.html) Viewing/setting signal values.


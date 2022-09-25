## Variable/Register Info Browser

### Introduction

The Variable/Register Info browser is a simple, but quick, method of viewing the values of the program's variables.

Seer presents this information in three tabs:

* Logger
* Tracker
* Registers

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

There are buttons to clear some or all entries in the logger.

### Tracker
The tracker is a method to watch several variables at a time. Manually enter a list of variable names. Their values will be updated in the Logger display at each stopping point (after a 'step' or 'next' or a breakpoint) is reached.

The variable must be part of the active stack frame, as selected by the Stack Info browser.

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

### References

Consult these gdb references

1. [Link](https://sourceware.org/gdb/onlinedocs/gdb/Registers.html) Viewing/setting register values.




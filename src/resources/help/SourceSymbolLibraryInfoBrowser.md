## Source/Symbol/Library Info Browser

### Introduction

This browser allows the program's symbol table to be seached and browsed.

Seer presents this information in three tabs:

* Source
* Functions
* Types
* Statics
* Libraries
* Ada exceptions
* Skips

### Source

This browser lists the source files used by the program being debugged. They are categorized into three folders bases on setting in ```Config->Source```.

```
      Folder           Description
      ----------       ---------------------------------------
      Source files     Source files your program has.
      Header files     Header files your program has.
      Misc files       System related header and source files.
```

Double-clicking on a filename will load the source file in the Code Manager.

Source files can be searched for using an Unix style wildcard. This will limit the number of source files listed to those that match.

### Functions

This browser lists all the functions that match a Regex wildcard the program being debugged uses.

This information is shown for each Function:
```
      Column       Description
      ----------   -----------------------------------------------------------------------
      Function     The function name.
      File         The short name of the source file where Function is declared.
      Line         The line number in the source file where the Function is declared on.
      Fullname     The fullname of the source file.
      Type         The function prototype.
      Description
```
Double-clicking on an entry will load the source file in the Code Manager.

### Types

This browser lists all the class/struct types that match a Regex wildcard the program being debugged uses.

This information is shown for each Type:
```
      Column       Description
      ----------   -----------------------------------------------------------------------
      Type         The class/struct name.
      File         The short name of the source file where Type is declared.
      Line         The line number in the source file where the Type is declared on.
      Fullname     The fullname of the source file.
```
Double-clicking on an entry will load the source file in the Code Manager.

### Statics

This browser lists all the static variables that match a Regex wildcard the program being debugged uses.

This information is shown for each Static:
```
      Column       Description
      ----------   --------------------------------------------------------------------
      Variable     The static name.
      File         The short name of the source file where Static is declared.
      Line         The line number in the source file where the Static is declared on.
      Fullname     The fullname of the source file.
      Type         The static prototype.
      Description
```
Double-clicking on an entry will load the source file in the Code Manager.

### Libraries

This browser lists all the shared libraries that match a Regex wildcard the program being debugged uses.

This information is shown for each Library:
```
      Column            Description
      ----------        -------------------------------------------------
      Id                Library name.
      Target-Name       Library name.
      Host-Name
      Symbols Loaded    Are symbols loaded for this library? 1 == yes.
      Thread Group      Which thread group is this entry for.
      Ranges            Memory range where the shared library is loaded.
```
### Ada exceptions

This browser lists all the Ada exceptions that match a Regex wildcard the program being debugged uses.
The program must be an Ada program.

This information is shown for each exception:
```
      Column            Description
      ----------        -------------------------------------------------
      Name              The name of the exception.
      Address           The address of the exception.
```
There is a button to quickly create a catchpoint for a selected exception. Once created, Seer will
stop the program when the exception is raised.

### Skip commands

This browser manages the gdb skip commands you might have set up. A skip will bypass entering a function
when the 'step' command is used. Skips can be described in various ways - filename, function name, and with
glob wildcarding or regex. See the 'gdb skip' reference below.

This information is shown for each exception:
```
      Column            Description
      ----------        -------------------------------------------------
      Number            The skip's interal number.
      Enable            Is the skip enabled?
      Glob              Is the skip a file glob wildcard?
      File              The name of the file. Can be <none> if the skip is a function.
      RE                Is the skip a function regex?
      Function          The name of the function. Can be <none> if the skip is a file.
```
Skips can be: added, deleted, enabled, or disabled. As well, they can be saved to Seer's settings or loaded.

### References

Consult these references

1. [Link](https://en.wikipedia.org/wiki/Regular_expression) Regular expressions.
2. [Link](https://en.wikipedia.org/wiki/Glob_(programming)) Unix wildcards.
3. [Link](https://sourceware.org/gdb/current/onlinedocs/gdb.html/Skipping-Over-Functions-and-Files.html) GDB Skip command.


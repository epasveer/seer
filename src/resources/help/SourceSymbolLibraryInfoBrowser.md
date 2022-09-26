## Source/Symbol/Library Info Browser

### Introduction

This browser allows the program's symbol table to be seached and browsed.

Seer presents this information in three tabs:

* Source
* Functions
* Types
* Statics
* Libraries

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

This browser list all the functions that match a Regex wildcard the program being debugged uses.

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

This browser list all the class/struct types that match a Regex wildcard the program being debugged uses.

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

This browser list all the static variables that match a Regex wildcard the program being debugged uses.

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

This browser list all the shared libraries that match a Regex wildcard the program being debugged uses.

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
### References

Consult these references

1. [Link](https://en.wikipedia.org/wiki/Regular_expression) Regular expressions.
2. [Link](https://en.wikipedia.org/wiki/Glob_(programming)) Unix wildcards.


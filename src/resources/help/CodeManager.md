## Code Manager

### Introduction
This is the main part of Seer that shows source files as they are opened. Opening a source file can happen in three ways:

* The program reaching a stopping point (via a 'step', 'next', or breakpoint).
* Clicking on a frame in the Stack Info browser.
* Clicking on a filename in the Source browser (Source/Symbol/Library browser).

As well as the source files, Seer can show the code's assembly as a tab. See:
```
    View->Assembly
```
Or
```
    Config->Assembly
```

When in a source file, using the RMB will bring up a menu to do any of these:

* Create a breakpoint
* Create a printpoint
* Enable/disable a breakpoint or printpoint
* Execute the program to a certain line in the source file.

Selecting a variable (highlighting) will allow that variable to be added to one of these via the RMB:

* Variable Logger
* Variable Tracked
* Memory Visualizer
* Array Visualizer
* Struct Visualizer

### Keys

The easiest method for logging a variable to the Variable Logger is to double-click the variable text in the source. When doing this, a key modifier (shift and/or ctrl) can be used to prepend a '*' and/or '&' to the variable. This will better handle pointers and references.
```
      Double-Click
      Modifier      Description                           Example
      ------------  ------------------------------------  -----------------------
      None          No modifier is prepended to variable  'argc'
      Shift Key     Prepend '&' to variable               '&argc'
      Ctrl Key      Prepend '*' to variable               '*argc'
      Ctrl+Shift    Prepend '*&' to variable              '*&argc'
```

Using ^F will bring up a search bar in the source window.

Using ^O will bring up a file bar to help locate the file for the source (in case it isn't in the directory that GDB thinks it is).

### Buttons

There are these buttons:

* File open.  Open a file. Can be any file, as long as it is plain text.
* Close files. Bring up a dialog to easily select source files to close.
* Search. Open the search bar.

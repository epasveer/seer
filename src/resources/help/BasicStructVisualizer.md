## Basic Struct Visualizer

### Introduction

The Struct Visualizer shows the contents of C/C++ structs and classes in a tree view. It is read-only and doesn't follow pointers.
Use the regular Struct Visualizer for that.

For each variable of the struct, Seer shows these two columns:

* Variable name
* Value

*Variable name* is the name of the variable, at that point in the stuct hiearchy.  
*Value* is the value of the variable, presented in the best way possible by gdb.  

This version of the Struct Visualizer expands to all levels when the variable is first entered. After that, levels can be collapsed or expanded again.

Also, variables that are pointers are never followed.

Use the new implementation for that :^)

### Operation

The Struct Visualizer has these main components

* Variable entry field
* Refresh
* Auto mode
* Variable Tree

### Variable entry field

This entry field allows you to enter the name of a variable. This variable can be for a struct or a class. It could also be a simple datatype like an *int* or a *string* but that would be pointless as simple datatypes have no nesting structure.

Enter the variable name and hit return. All levels of the struct will be shown. If it has any subvalues, a '+' icon will appear beside it so that it can later be collapsed.

Note, if the variable name is not valid, a message will be printed in the Value field.

### Refresh

This will refresh the tree with any variables that have changed values since the last time.

### Auto mode

This mode will refresh the tree each time Seer reaches a stopping point (when you 'step' or 'next' or reach a 'breakpoint').

### Variable tree

As mentioned, the variable tree shows the variable names and values of the struct.

A '+' icon is shown beside each level. This will expand or collapse the level.

### Modifying values.

This implementation has no feature to modify values.

### Invoking other Visualizers.

Using the RMB on a variable will bring up a menu to launch another Visualizer for that variable.


## Struct Visualizer

### Introduction

The Struct Visualizer shows the contents of C/C++ structs and classes in a tree view.

For each variable of the struct, Seer shows these three columns:

* Variable name
* Value
* Datatype

*Variable name* is the name of the variable, at that point in the stuct hiearchy.  
*Value* is the value of the variable, presented in the best way possible by gdb.  
*Datatype* is the variables datatype (basic type, class, or struct). If the variable is a pointer, a '*' is shown at the end of the datatype.

A variable of the struct that has a value of '{...}' means it has subvalues (ie: is nested) and can be expanded. There will be a visual icon (like a '+') to show the tree can be expanded at that level.

gdb has the notion of virtual levels for structs and classes.

* public
* protected
* private

These indicate where the variables reside in scope withing the struct or class. All struct variables will have a scope of 'public'. Classes will have the appropriate scope as they are defined in the class header file. These virtual levels are easy to see in the tree as they have no value or type.

### Operation

The Struct Visualizer has these main components

* Variable entry field
* Expand tree
* Collapse tree
* Recursive mode
* Refresh
* Auto mode
* Variable Tree

### Variable entry field

This entry field allows you to enter the name of a variable. This variable can be for a struct or a class. It could also be a simple datatype like an *int* or a *string* but that would be pointless as simple datatypes have no nesting structure.

Enter the variable name and hit return. The first level of the struct will be shown. If it has any subvalues, a '+' icon will appear and you can expand it to look at the subvalues.

Note, if the variable name is not valid, an error box is display.

### Expand tree

The big '+' icon that appears in the top bar will expand the currently selected level in the tree. If no level is selected, it will expand the top level of the tree.

### Collapse tree

The big '-' icon that appears in the top bar will collapse the currently selected level in the tree. If no level is selected, it will collapse the top level of the tree.

### Recursive mode

When expanding, the recursive mode determins how many levels to expand. For example, a struct with multiple levels, if the recursive mode is 'off', it will only expand one level each time.

If the recursive mode is 'on', it will expand the sublevels indefinetly.

**NOTE** The recursive mode can be fooled by some structs and cause an infinite loop. To this end, the recursive mode will always stop if the variable is a pointer ('*'). Also, while the expanding is happening, turning off the recursive mode will stop the recursion imediately.

### Refresh

This will refresh the tree with any variables that have changed values since the last time.

### Auto mode

This mode will refresh the tree each time Seer reaches a stopping point (when you 'step' or 'next' or reach a 'breakpoint').

### Variable tree

As mentioned, the variable tree shows the variable names, values, and types of the struct.

A '+' icon is shown beside each level. This will expand or collapse the level in the same way as the big '+' or '-' in the top bar. When expanding, it follows the same recursive mode.

### Modifying values.

The Struct Visualizer allows variables in the tree to have their value changed. This is for simple datatypes only (int, floats, etc.).

For a variable in the tree, double click the 'Value' column for the variable. It will then allow you to enter a new value (after you change it and hit return).

An error box is shown if the variable can't be changed.

### Invoking other Visualizers.

Using the RMB on a variable will bring up a menu to launch another Visualizer for that variable.


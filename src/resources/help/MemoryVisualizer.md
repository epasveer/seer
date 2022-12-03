## Memory Visualizer

### Introduction

The Memory Visualizer shows the contents of a region of memory in different formats, including strings and assembly.

* Hex
* Octal
* Binary
* Decimal
* Ascii
* Ebcdic
* Assembly

The memory displayed in 3 views.

* A memory dump in the chosen Display format. Included in this view is the memory displayed in the Character format. A byte can be selected in this view to highlight it.
* A table showing the highlighted byte in different numeric formats (int, float, double, etc...).
* A separate tab showing the memory as disassembly.

A CRC16 checksum of the memory region is also displayed. Useful for quickly determining if different memory regions have the same contents.

### Operation

There are various things to control how the memory is display.

* Starting address entry field
* Number of bytes entry field
* Display format
* Character format
* Column width
* Refresh
* Auto Refresh

### Starting address entry field

The 'Starting Address' input field takes the name of a variable that is in the current Stack Frame (selected by the Stack Frame browser). This variable should result in an address. For example, take this code:
```
    int var = 10;
```
To visualize the memory for 'var', the variable should be '&var', not 'var'. As 'var' doesn't resolve to an address.

Note, the variable name can be an address. Enter the address like:
```
    0xdeadbeaf
```
It can also be one of gdb' special variables. Like the program counter. Enter it like:
```
    $pc
```


### Number of bytes entry field

The default number of bytes to display is 256. Fewer or more than this number can be entered.

### Display format.

The memory dump can be displayed in these formats:

* Hex
* Octal
* Binary
* Decimal

### Character format

The memory dump can be displayed in these character formats:

* Ascii
* Ebcdic

### Column width

This specifies the column width of the memory dump.

### Refresh

This will refresh the memory dump since the last time.

### Auto mode

This mode will refresh the memory dump each time Seer reaches a stopping point (when you 'step' or 'next' or reach a 'breakpoint').


## Image Visualizer

### Introduction

The Image Visualizer shows the contents of a region of memory as a image.

### Operation

There are various things to control how the memory is display.

* Starting address entry field
* Number of bytes entry field
* Image width (in pixels)
* Image height (in pixels)
* Image format
* Refresh
* Auto Refresh

A check is done to ensure the amount of memory given to the Memory Visualizer will cover the image size.

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

### Image format.

Two supported formats are available:

* RGBA8888
* RGB888

### Refresh

This will refresh the memory dump since the last time.

### Auto mode

This mode will refresh the memory dump each time Seer reaches a stopping point (when you 'step' or 'next' or reach a 'breakpoint').

### Image interaction

Avaible Quick keys while in the Image Visualizer:
```
    '+' zoom in
    '-' zoom out
    <ESC> reset to default zoom level.
    ^P  print image.
```


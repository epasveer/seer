## Skips

### Introduction

Skips are gdb's way of ignoring non-important arguments that are functions when stepping into a function to debug.

For example, consider this code:
```
    101     int func()
    102     {
    103         foo(boring());
    104         bar(boring());
    105     }
```
` `  
Suppose you wish to step into the functions ```foo``` and ```bar```, but you are not interested in stepping through ```boring```.
If you run ```step``` at line 103, you’ll enter ```boring()```, but if you run ```next```, you’ll step over both ```foo``` and ```boring```!

One solution is to ```step``` into ```boring``` and use the ```finish``` command to immediately exit it. But this can become tedious if ```boring``` is called from many places.

A more flexible solution is to tell gdb to execute ```boring``` with out stepping into it. The ```skip``` command does this.

### Skip types

There are 4 types of skips.

#### 'file' skips

This skip takes a single source file. All functions descriped in the file will be skipped when stepping.

#### 'file glob-pattern' skips

Functions in files matching file-glob-pattern will be skipped over when stepping.

#### 'function' skips

This skip takes a single function specification (see the Location-Specifications link below).
This function will be skipped when stepping.

#### 'function regex' skips

Functions whose name matches regexp will be skipped over when stepping.

This form is useful for complex function names. For example, there is generally no need to step into C++ std::string constructors or destructors.
Plus with C++ templates it can be hard to write out the full name of the function, and often it doesn’t matter what the template arguments are.

Specifying the function to be skipped as a regular expression makes this easier.
```
    ^std::(allocator|basic_string)<.*>::~?\1 *\(
```
` `  
If you want to skip every templated C++ constructor and destructor in the std namespace you can do:
```
    ^std::([a-zA-z0-9_]+)<.*>::~?\1 *\(
```
` `  
### References

Here is gdb's reference for the ```skip``` command.

https://sourceware.org/gdb/current/onlinedocs/gdb.html/Skipping-Over-Functions-and-Files.html  
https://sourceware.org/gdb/current/onlinedocs/gdb.html/Location-Specifications.html#Location-Specifications  

Here's a good article from "MaskRay" where he describes skips in an easy to understand way.

https://maskray.me/blog/2024-12-30-skipping-boring-functions-in-debuggers  

` `  

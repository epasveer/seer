## Printpoints

### Introduction

Printpoints are a type of gdb breakpoint that simply prints a custom message when the breakpoint
is reached. After the message is printed, the program is continued automatically.

So, basically, a way to add print statements without adding them to your code.

### Printpoint types

There are 3 types of printpoints.


### 'gdb' printpoints

The ```gdb``` printpoint is the most common. When used, it uses gdb's ```printf``` command, which
provides C style formatting.

Consider this example of code:
```
    12
    13      std::cout << " C++ loop" << std::endl;
    14      for (n=1; n<=count; n++) {
    15          nfact = nfact * n;
    16          std::cout << std::setw(12)  << n << std::setw(14) << nfact << std::endl;
    17      }
    18
```
A printpoint can be added to line 15 to print the value of ```n``` and ```count```. Create a
printpoint by RMB clicking on line 15 of source, or click 'Add a new printpoint' in the Printpoints
tab. Fill in the name of the source file and the line number.

Now fill in the printpoint details:
```
Format    : "N=%d  COUNT=%d\n"
Arguments : n count
Type      : gdb
Function  : <blank>
Channel   : <blank>
```

When you run your program, the print statements will appear in Seer's Gdb tab.

### 'call' printpoints

The ```call``` printpoint type allows you to use an alternate ```print``` function instead of gdb's
```printf``` command.

There are a couple restrictions for this ```print``` function.

- Must have a signature that takes VA_ARGS.
- Needs to be part of your program. ie: linked in or part of some .so, like glibc.

There are 2 ```call``` methods. One without a "channel" and one with a "channel". What is a "channel"?
It's like the first argument to ```fprintf()```, or ```dprintf()```

***

Here is an example of ```call``` without a "channel". It will use ```printf()``` from glibc.

We'll use the same code example and will add the printpoint on the same line number.
```
Format    : "N=%d  COUNT=%d\n"
Arguments : n count
Type      : call
Function  : printf
Channel   : <blank>
```

When you run your program, the print statements will appear in Seer's Console tab.

***

Here is an example of ```call``` ***with*** a "channel". It will use ```dprintf()``` from glibc,
which takes a file descriptor as its first argument. We'll be using FD of 1, which is stdout.

We'll use the same code example and will add the printpoint on the same line number.
```
Format    : "N=%d  COUNT=%d\n"
Arguments : n count
Type      : call
Function  : dprintf
Channel   : 1
```

When you run your program, the print statements will appear in Seer's Console tab.


### 'agent' printpoints

If your program is started with ```gdbserver```, a printpoint type of ```agent``` will tell the
gdbserver to print the message.

First, start the program using ```gdbserver```.
```
$ gdbserver :1234 hellodprintf
```
Then start Seer and connect to the ```gdbserver```.

We'll use the same code example and will add the printpoint on the same line number.
```
Format    : "N=%d  COUNT=%d\n"
Arguments : n count
Type      : agent
Function  : <blank>
Channel   : <blank>
```
The print statements will appear from the ```gdbserver``` process.

### Warnings

Printpoints, actually ```dprintf```, are not very error friendly. Errors in your parameters are
not checked until the breakpoint is reached. This can cause gdb to behave poorly or can, most
likely, confuse Seer. You'll end up restarting your debugging session.

The ```call``` method seems to work only with C/C++ code. It doesn't work with Fortran. Not sure
about any other language. The error message is very vague and appears in the Gdb tab as "parsing error".
Seer ends up in a confused state.

### References

Printpoints use gdb's ```dprintf``` command. Not to be confused with the C language's ```dprintf()```
function.

https://sourceware.org/gdb/current/onlinedocs/gdb.html/Output.html  
https://sourceware.org/gdb/current/onlinedocs/gdb.html/Dynamic-Printf.html  

Here's a good article from Andreas Heck where he creates a custom ```print``` function that writes
to a file, instead of stdout. This works with a ```call``` printpoint without a channel.

https://abstractexpr.com/2024/03/03/dynamic-printf-debugging-with-gdb/  


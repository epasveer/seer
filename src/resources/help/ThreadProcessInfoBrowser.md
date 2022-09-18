## Thread/Process Info Browser

### Introduction

The Thread/Process Info browser presents the program's thread and process information. In gdb speak, these are threads and inferiors.

Seer presents this information in three tabs:

* Frames
* Ids
* Groups

### Frames
Frames is the most useful. It presents a list of Thread Ids the program is currently using.  A Thread Id can be:

* A system thread (aka, posix thread)
* A system process (aka, another inferior because of a fork or exec)


This information is shown for each Thread Id:
<br>

|Column   |Description                                        |
|---------|---------------------------------------------------|
|Thread Id|An id given to the thread/process by gdb           |
|State    |The running state of the id. 'running' or 'stopped'|
|Target Id|Details of the id. Thread or process info          |
|Function |The function name the thread id is in              |
|File     |The filename the thread id is in                   |
|Line     |The line number in the filename                    |
|Arguments|The arguments passed to the function               |
|Core     |Which cpu core the thread id is running on         |

Clicking on a Thread Id will cause Seer to make that Thread Id the active thread. This will in turn cause the Stack Info Browser to refer to that Thread Id.

### Ids
Ids is a simplified list ofThread Ids, with no other information. Clicking on a Thread Id will cause Seer to make that Thread Id the active thread. This will in turn cause the Stack Info Browser to refer to that Thread Id.

### Groups
Groups is a list of Thread Groups. Basically, inferior processes. Most programs are just one inferior, even if they use threads. Program's that use fork and exec will have multiple inferiors.
This information is shown for each Thread Id:
<br>

|Column         |Description                                   |
|---------------|----------------------------------------------|
|Thread Group Id|An id given to the thread/process group by gdb|
|Type           |The Thread Group type. Usually 'process' type |
|Pid            |The process id                                |
|Executable     |The name and path of the process executable   |
|Cores          |List of cpu cores used by the process         |


### References

Consult these gdb references

1. [Link](https://sourceware.org/gdb/onlinedocs/gdb/Threads.html#thread-ID-lists) Listing thread information.
1. [Link](https://sourceware.org/gdb/onlinedocs/gdb/All_002dStop-Mode.html#All_002dStop-Mode) GDB all-stop mode.
1. [Link](https://sourceware.org/gdb/onlinedocs/gdb/Forks.html) GDB follow-fork mode.

<br>

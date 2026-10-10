## ParallelStacks Visualizer

### Introduction

The ParallelStacks Visualizer shows all the thread's stack frames as a node graph. This visualizer is dynamic.
In that thread nodes can be interacted with to tell Seer to switch to different threads and frames within
that thread. Likewise, if Seer's regular Frame and Thread views are used to change the current Frame or Thread,
the ParallelStacks Visualizer will get updated.

This Visualizer is taken from a feature in VSCode.

### Refresh

This will refresh the image since the last time.

### Auto mode

This mode will refresh the image each time Seer reaches a stopping point (when you 'step' or 'next' or reach a 'breakpoint'). Note,
this can be costly.

### Method View

The view mode combobox selects between 'Stack' and 'Method'. 'Stack' shows every thread's stack. 'Method' pivots the graph
on a single function — the one in
the current thread's current frame. That function is shown alone in the highlighted (heavier-bordered) box in the middle,
merging every thread that calls it, however it got there. Threads that never call it are left out.

* Above the pivot are its callees — what each of those threads is doing inside it.
* Below the pivot are its callers — the paths that led to it, down to each thread's entry point.

The pivot box's header shows how many threads call the function. If the function is recursive, the graph pivots on
its innermost call.

Like the Visual Studio feature, Method View follows the debugger: selecting a different thread or stack frame (in Seer's
thread or stack frame browsers, or in the graph's own thread popup) re-pivots the graph on that frame's function.

### Search

The search field highlights every frame, in every node, whose function name matches the typed regular expression
(case-insensitive). Matching nodes are also colored in the minimap. If a node's middle frames are hidden by the stack size
setting, its '[...]' row is highlighted when any of those hidden frames match.

Typing jumps to the first matching node. 'Enter' (or 'Ctrl+G') moves to the next matching node and 'Shift+Enter'
(or 'Ctrl+Shift+G') to the previous one. The label beside the field shows which match you're on; hover over it for the
total count of matching frames. The search is reapplied whenever the graph is refreshed.

### ParallelStacks interaction

Available Quick keys while in the ParallelStacks Visualizer:
```
    '+'             Zoom in.
    '-'             Zoom out.
    MouseScroll     Zoom in and out.
    ESC             Reset to default zoom level.
    Ctrl+F          Focus the search field.
    Ctrl+G          Jump to the next search match.
    Ctrl+Shift+G    Jump to the previous search match.
    Shift+LMB       Grab a stack node so it
                    can be moved.
    Hover           Over a stack node to pop-up
                    its list of threads.
    LMB+Click       On one of the threads in the
                    pop-up to select that thread.
```

### Resources

[Seer Issue](https://github.com/epasveer/seer/issues/312)  
[GDB python script](https://github.com/bravikov/parallel-stacks)  


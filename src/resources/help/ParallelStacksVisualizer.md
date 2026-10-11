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

The search field highlights every frame, in every node, whose function name matches the typed text (case-insensitive).
With 'Regex' checked, the text is a regular expression. Unchecked, it's matched as plain text, so characters like '(', '*'
or '[' need no escaping. Matching nodes are also colored in the minimap. If a node's middle frames are hidden by the stack size
setting, its '[...]' row is highlighted when any of those hidden frames match.

Typing jumps to the first matching node. 'Enter' (or 'Ctrl+G') moves to the next matching node and 'Shift+Enter'
(or 'Ctrl+Shift+G') to the previous one. The label beside the field shows which match you're on; hover over it for the
total count of matching frames. The search is reapplied whenever the graph is refreshed.

### Filter

The filter button (the funnel) opens a list of every library, function and thread found in the threads' stacks, arranged
as a tree: library, then the functions in that library, then the threads that call each function. Frames with no shared
library (the program itself) are listed under '(program)'. The text field at the top narrows the list.

Checking items hides every thread that doesn't pass through them. A thread is shown if any of its frames is in a checked
library or a checked function, or if the thread itself is checked. Checking a library or function checks everything under it.
The graph updates as you check items, and the filter button stays pressed while a filter is active. Hover over it to see
how many threads are shown.

'Clear Filters' unchecks everything and shows all threads again.

The filter applies to both the 'Stack' and 'Method' views, and is kept when the graph is refreshed. Libraries and functions
are remembered by name and threads by id, so they apply to whatever stacks the next refresh brings.

### External Code

Frames without debug info (library or system code, like libc's thread start-up or a futex wait) are normally of little
interest. Each run of consecutive frames like that is merged into a single '[External Code]' frame. Threads whose stacks
differ only inside external code then share a node instead of each getting their own.

Search sees through an '[External Code]' frame: it's highlighted when any of the functions it stands in for match.
If the current frame is inside external code, its '[External Code]' frame is the one shown in bold. In Method View, the
pivot function is never merged, even when it has no debug info.

To see every frame, check 'Show External Code' in the settings dialog (see Settings, below).

### Settings

The settings button opens a dialog that controls how the graph is drawn. The settings are remembered between sessions,
and the graph is redrawn when the dialog is closed with 'OK'.

* **Show Minimap** — A small overview of the whole graph, in the corner of the view. The rectangle in it marks the part of
  the graph that's visible. Click or drag in it to jump around the graph, and Shift+drag to move the minimap itself.
  * 'Auto' shows it only when the graph doesn't fit in the view (when there are scrollbars).
  * 'Always' shows it all the time.
  * 'Never' hides it.
* **Function Name Length** — With 'Full' checked, function names are shown at their full length. Unchecked, names longer
  than the given number of characters are shortened by replacing their middle characters with ' ... '. Useful for long C++
  template names. The default is 64.
* **Stack Frame Size** — With 'All' checked, every frame of a node is shown. Unchecked, a node with more frames than the
  given number shows only that many: half from the top of its stack and half from the bottom. A single '[...]' row
  stands in for the frames left out of the middle. The default is 20.
* **Show External Code** — Unchecked (the default), each run of frames without debug info is merged into a single
  '[External Code]' frame (see External Code, above). Checked, every frame is shown.

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


## ParallelStacks Visualizer

### Introduction

The ParallelStacks Visualizer shows all the thread's stack frames as a node graph. Currently this is a static image. Future plans
are to make it dynamic in that thread nodes can be interacted with to tell Seer to switch to different threads and a frames within
that thread.

This Visualizer is taken from a feature of VSCode.

### Refresh

This will refresh the image since the last time.

### Auto mode

This mode will refresh the image each time Seer reaches a stopping point (when you 'step' or 'next' or reach a 'breakpoint'). Note,
this can be costly.

### ParallelStacks interaction

Available Quick keys while in the ParallelStacks Visualizer:
```
    '+'   zoom in
    '-'   zoom out
    ESC   reset to default zoom level.
```

### Resources

[Seer Issue](https://github.com/epasveer/seer/issues/312)  
[GDB python script](https://github.com/bravikov/parallel-stacks)  


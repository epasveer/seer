## Array Visualizer

### Introduction

The Array Visualizer shows the contents of one or two variables, that are arrays, in a graph/chart view. When providing two arrays, they can be treated as two independant arrays (assigning both to the Y axis) or one array can be the X points and the other array can be the Y points.

There are different views of the plot (line, spline, scatter) and the array values can be shown as points and/or labels.

The Array Visualizer is made up of 3 main parts:

* Input parameters
* Values table
* Plot area

The plot area is a typical graph that has its 'Y' axis on the left side and the 'X' axis across the bottom.
```
      ^
      |
      |
    Y |
      |
      |
      |
      +--------------------------------->
                     X

```


### Input parameters

This part of the Visualizer specifies the one or two arrays to view. If one array is to be viewed, leave the second entry blank. Otherwise, enter the array details for both arrays. Here are the details for an array.

* Axis. Specify what axis the array is for. Can be 'Y' or 'X'. 'Y' is the default.
* Variable name. The name name should resolve to an address.
* Number of elements. The total length of the array.
* Array offset. How many elements to initially skip. Default 0 (start at the begining of the array).
* Array stride. How the elements are accessed in the array. Default 1 (use every element). 2 would access every second element.
* Array data type.
* Refresh.

By having array offset and array stride, it's possible to handle the case of a single array containing X/Y points as alternating X and Y values.

### Values table

This part of the Visualizer shows the arrays values as one or two columns. The number of rows in the column is the number of elements in the array.

Note, the array offset and array stride is taken into account.

Each column in the table will have a title, which reflects the variable name and the array offset and stride:
```
    &array:0:1
```
### Plot area

This area shows the plot for the one or two arrays. The plot can be modified with:

* Chart title. Add a title to the top of the plot.
* Plot mode
    * Line. Simple line plot.
    * Spline. Points are plotted using splines.
    * Scatter. Points are plotted using scatter points.

* Annotation
    * Points. Visually show the points.
    * Labels. Annotate the value at each point.

### Operation

The plot area can be interactive in these way:

* '+', '-', mouse scroll button. Zoom in or out of the plot.
* LMB drag and hightlight. Zoom in on the selected region.
* Shift+LMB. Drag plot around.
* 'esc'. Reset plot area.


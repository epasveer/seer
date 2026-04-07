## Matrix Visualizer

### Introduction

The Matrix Visualizer shows the contents of an array as a matrix, a number of rows and an number of columns.

The Matrix Visualizer is made up of 3 main parts:

* Input parameters
* Values table
* Statistic information

### Input parameters

This part of the Visualizer specifies the array to view. Here are the details for an array.

* Variable name of the array. The name name should resolve to an address.
* Number of rows and columns. The total length of the array is determined from this.
* Array offset. How many elements to initially skip. Default 0 (start at the begining of the array).
* Array stride. How the elements are accessed in the array. Default 1 (use every element). 2 would access every second element.
* Array data type.
* Refresh.

By having array offset and array stride, it's possible to handle oddly constructed arrays.

### Values table

This part of the Visualizer shows the array's values as a matrix. The number of rows and number of columns reflect the values
entered in the input parameters.

Note, the array offset and array stride is taken into account.

### Statistic information

These statistical values are calculated from the array values.

* Count of values
* Count of number of rows
* Count of number of columns
* Minimum value of values
* Maximum value of values
* Sum of values
* Average value
* Median value
* RMS value



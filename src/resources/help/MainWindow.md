## Seer Main Window


### Main Tool Bar

The main tool bar has buttons for Seers's common functions. Most of these buttons also have hotkeys
assigned to them. These can be configured with "Settings->Configure->Keys".

The buttons are arranged in groups.

* Starting/Restarting the debug session.

    * Debug - Start a new debugging session.
    * Restart - Restart the current debugging session. Stop in main().
    * Terminate - Terminate the current debugging session.

* Stepping.

    * Continue - Continue the program until it encounters the next breakpoint or it ends.
    * Next - Execute the next line. Step over if it's a function.
    * Step - Exectue the next line. Step into it it's a function.
    * Finish - Finish the current function. Honor any breakpoints.

* Instruction recording. Use gdb's instruction and playback feature.

    * Record - Start the record mode.
    * Direction - Set playback mode. Forward or reverse. The direction affects the Stepping functions.

* Program interruption. Interrupt the running program or send it a specific signal.

* Visualizers

    * Memory - Visualize a region of memory in a 'hex viewer'.
    * Array - Visualize a region of memory as arrays. A table and 2D plot.
    * Matrix - Visualize a region of memory as a 2D table of values.
    * Struct - Visualize a region of memory as a nested C/C++ structure.
    * Basic Struct - Similiar as 'Struct' but doesn't follow pointers.
    * Image - Visualize a region of memory as a RGB or RGBA image.
    * Parallel Stacks - View and interact with threads. (BETA)


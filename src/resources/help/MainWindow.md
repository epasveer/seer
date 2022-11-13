## Seer Main Window


### Main Tool Bar

The main tool bar has buttons for Seers's common functions. Most of these buttons also have hotkeys
assigned to them. These can be configured with "Settings->Configure->Keys".

The buttons are arranged in groups.

* Restarting the debug session.

    * Run - Restart the program. Only stop at any previously set breakpoints.
    * Start - Restart the program. Stop in main().

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
    * Struct - Visualize a region of memory as a nested C/C++ structure.
    * Image - Visualize a region of memory as a RGB or RGBA image.


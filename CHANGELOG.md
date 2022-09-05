
# Seer Change Log

## [1.10beta] - 2022-XX-XX
* New Struct Visualizer.
    - Built apon gdb's -var-obj framework.
    - Can recursively show the contents of a struct/class.
    - Show datatype of each member. (Value or pointer)
    - Can modify values of simple datatypes.

## [1.9] - 2022-08-30
* Rename 'seer' binary to 'seergdb'. The name conflicts with another opensource project.
* Prepare for better Debian (and other distros) packaging and releasing.
* Add history (up/down arrows) to all line input fields.
    - Variable logger, tracker browsers.
    - Source, function, types, statics, library browsers.
    - Struct, memory, array visualizers.
    - Code and assembly editors.
    - Console.
* Add StructVisualizer to source/assembly editors and stack arguments/locals browsers.
* Add a RMB to launch Memory/Array/Struct viewer from a running Struct viewer.
* Change 'detachtab' to use RMB context menu instead of a double-click.
* Allow filenames with spaces when setting breakpoints.

## [1.8] - 2022-08-08

* Add StructVisualizer
    - New visualizer to view the contents of a C/C++ struct or a C++ class in a tree.
    - Click on the "StructVisualizer" in the menubar or "View -> Struct Visualizer".
    - Enter the name of the variable in the text section.
    - examples:
        "*this"
        "mystruct"
        "mystruct.member"
    - The variable text section has a history. So you can use the UP and DOWN arrows
      to select previous entries.
    - Manually refresh the view or click on "Auto" to refresh the view at each
      stopping point (after a step/next/breakpoint).

* Add Dark theme option for Seer's code editor. Set in Seer config page.
    - Works for C/C++ source tabs and the assembly tab.

* Register browser.
    - Can now edit register values.
    - Either by double-clicking the register value or by RMB context menu
      to bring up a dialog.
    - Add format selector for register values.
    - Improve detection of changed register values from previous stopping point.

* Assembly support.
    - Add assembly config page for certain assembly settings.
        - assembly style
        - C++ symbol demangling.
        - Keep assembly tab on top.
        - Show assembly tab on Seer startup.
    - Launch MemoryVisualizer and ArrayVisualizer from the Assembly tab on highlighter
      text.

* Debug options for launching program.
    - No break on program start.
    - Break in "main".
    - Break in function.
    - Break at address.
    - Randomize program's start address.

* ArrayVisualizer and MemoryVisualizer now allow an address as the variable name.

* A menu to experiment setting Seer with different Qt gui styles.
    - See "View -> Styles"

* Bugs fixes.
    - Fixed double refresh bug in code editor.
    - Fixed bug when displaying assembly for the first time. It wasn't showing
      the current line.
    - Fixed background color problem in MemoryVisualizer for disassembly.

## [1.7] - 2022-07-04

* Add an assembly tab, along side the source tabs in the Code Manager.
    - Shows the program's assembly.
    - Enable with "View -> Assembly View"
    - Can set and show breakpoints.
    - Highlight the current instruction.
    - Step by instruction (Nexti and Stepi).
    - Double-clicking on entries in the "Breakpoints" tab and the "Stack frames" tab will show the assembly for those addresses.
    - ^F to bring up search bar in Assembly tab.
* Fix some minor bugs.


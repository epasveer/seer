
# Seer Change Log

## [2.6beta] - 2025-XX-XX
* Starting version 2.6 development cycle.
* Fixed regression when adding commands to a watchpoint.
* Create console once per Seer session. Instead of constant create/destroy.
* Fixed regression when ignoring files to be opened in the
  EditorManager.
* Fixed regression when order of message tabs not being preserved between
  sessions.
* Connect mode to a gdb server now supports 'remote' and 'extended-remote'.
* Connect mode's 'pre' commands are executed before the 'target connect'.
* Optionally add a timestamp to the Seer and Gdb log widgets.
* Add 'gdbserver debug' checkbox to Connect launch tab. For showing
  gdb and gdbserver communication debug information in gdb tab.
* Manage gdb skip commands via a new Skip Browser.
* Revamp the "run/start" buttons on the menu bar to be a predominate
  "terminate" that switches to a "restart".
* Fixed bug when adding variable to tracker. Sometimes would not refresh value.
* Raise Logger or Tracker tab when new variable is added.

## [2.5] - 2024-12-24
* Console now supports a subset of ANSI color codes.
* Console can be started in these modes:
    - Detached
    - Detached and minimized
    - Attached in Seer's tab view (with gdb logs and seer logs).
* Improved handling of \n \t and other escaped characters in gdb log window.
* Show breakpoint info as a tooltip if the breakpoint icon is clicked with
  LMB and held down.
* Show stack as a hex dump, with options to view as short, int, long, ascii, ...
* The "go to address" in the Assembly view now works if address it outside
  current assembly view.
* Visualizers can take a gdb expression for input fields for
  length/size of something. The visualizers are:
    - ArrayVisualizer
    - ImageVisualizer
    - MemoryVisualizer
* Fixed regression when setting/saving the editor font setting.
* Fixed bug when the Basic Struct visualizer display simple variable
  types (non-structs).
* Source all files in ~/.config/seergdb/scripts/ on startup.
* Fixed regression handling ignoring of system header files.
* Fixed regression when adding commands to a breakpoint.
* Refactored/improved Printpoints. Added 'help' to the
  Printpoint create dialog.
* Fixed regression when display variable value when hovering over
  the text in the editor window.

## [2.4] - 2024-03-18
* Changed main icon to a more license friendly one.
  All icons are now GPLv3, CC3.0, or CC4.0
* Fixed string compares for breakpoint conditions (#184)
* Added '--bs' command line option to specify a breakpoint at a source.cpp:lineno
* Fixed long tooltips text by restricting them to 100 characters. (#189)
  The text in the various viewing dialogs is still the full length.
  The 100 limit probably needs to be configurable.
* Added register profiles to show only interesting/relevant registers.
* Added UTF-8,16,32 support in the Memory Visualizer.
* Added an internal "dark" and "light" theme via the View->Style menu.
* Added option to open the current source file in an external editor.
* Fixed saving of RR parameters in config dialog.
* Sped up some visualization views.

## [2.3] - 2023-11-19
* In the margins of the source windows, allow CTRL+DoubleClick to do a quick RunToLine or RunToAddress.
* Add --gdb-program and --gdb-arguments to command line to override settings from Seer's config.
* Fixed a rare bug with blank lines from gdb causing a segv in GdbMonitor.
* Fixed bug specifying path to "rr" debugger.
* Add option to reload source file if it changes.
* Tighten up the layout by removing some needless whitespace.
* Move gdb messages to a tab in the breakpoints window.
    - Options to raise on every message, never, or important messages.
* Add C++ level (11 or 17) depending on qt5 or qt6 compile.
* Fixed up column resizing for the Variable logger (#173)
* Use monspace font, where it makes sense (#175)
* Allow copy to clipboard (#176)
* Fixed hovering a variable name and value (#179)
* Add nested struct viewing in "locals", "arguments", "logger", and "tracker" tabs. (#180)

## [2.2] - 2023-09-07
* Fixed infinite loop when starting with RR mode.
* Reworked RR mode to directly run 'rr replay'.
  No need to start a 'rr server' and connect to it.
  https://github.com/epasveer/seer/wiki/RR-and-Seer
* Fixed Source handling in source browser. Qt5 mode was broken.
  Qt6 works.  Directory paths need a tailing "/*"

## [2.1] - 2023-08-23
* Add Execution dialog to record breakpoint events. This solves the problem
  of too many break/error dialogs to "swat away" as the program is debugged.
* Add a method to add/change gdb commands to execute when a breakpoint (breakpoint, watchpoint, catchpoint) is reached.
* Add a method to add/change a breakpoint's condition command.
* Add a method to add/change a breakpoint's ignore count.
* Add a method to add/change a breakpoint's command list.
* Debug dialog for "attach" mode now detects executable name and path from /proc/<pid>/exe.
* Commandline for "attach" mode now detects executable name and path from /proc/<pid>/exe.

## [2.0] - 2023-03-06
* Seer is Qt6 based. Still compiles with Qt5.
* See Seer's Wiki page for compile instructions.
     https://github.com/epasveer/seer/wiki

## [1.17] - 2023-04-23
* Add support for the RR debugger.
  https://github.com/epasveer/seer/wiki/RR-and-Seer
* Add a dialog when a breakpoint is reached.

## [1.16] - 2023-04-07
* Add pid to main window and console title bars.
* The Pending flag is automatically supplied to the breakpoint function in
  the Debug dialog for the Run mode. Some apps use deferred loading of code
  with dlopen().
* Fixed bug when restoring from project file with 'start' mode.
* ADA: Improve visualizers for Ada arrays. "(system.address) 0x7fffffffcf10"
* ADA: Add Ada task browser.
* ADA: Add Ada specific catchpoints.
* ADA: Add Ada exception brower.
* Add a Shortcut (CTRL-I) to interrupt the running program.

## [1.15] - 2023-03-04
* Revamp Debug dialog. Move debug modes into "tabs".
* Add "help" icon to each debug mode in Debug dialog.
* Add 'pre' and 'post' gdb commands that can be executed
  just before and just after the program is loaded into gdb.
* Add the concept of 'project' files that can be created and loaded.
  Project files contain all settings available in the Debug dialog.
* Assembly view can now be shown in 2 modes. See "Settings->Configure->Assembly"
     - Function. The view shows all assembly in the function 
       the $PC is in.
     - Length. The view shows N bytes of assembly after the $PC. Used
       for code that is pure assembly or has no debug information.
* Minor fixes when debugging corefiles.
* Fixed breakpoints with conditional statements.
* Fixed up icons and Debian "copyright" file for Debian Intent-To-Package.
* Double-clicking on a breakpoint in the source window will delete it.
  Previously it would disable it.
* Assembly Tab - show only Nexti and Stepi buttons.
  Source tabs - show only Next and Step buttons.
* Hide certains buttons (Run, Start, ...) depending on Debug mode (run, attach,
  connect, etc...)
* Save state of tabs (tab order and current tab). So next session will get them.

## [1.14] - 2023-01-02
* Happy New Year!
* Add 'tabsize' property to Source Editors to properly display source files
  containing tab characters. Default is 4. Can be changed by the Editor config
  page.
* Fixed '\t' character returned by some flavors of gdb that display the disassembly.
* Remove whitespace from C/C++ lines in the Assembly tab.
* Allow assembly code to have its own font format. See Config->Editor->AssemblyText.
* Add Nexti and Stepi toolbar buttons if Assembly tab is shown.
* Add PC, SP, FLAGS status bar to Assembly tab.
* Re-added the original struct visualizers as "Basic Struct Visualizer". The advanced
  version is called "Struct Visualizer".
* Moved selection of visualizers into a sub menu called "Visualizers" on the menu bar.
* Added alternate file to load symbols from (instead of the debugee executable.

## [1.13] - 2022-12-02

* Improve specifying an alternate directory if the source has moved. Fewer mouse clicks.
* Add new Image Visualizer.
    - View RGB888 and RGBA8888 images.
* Assembly Tab can show source lines along with assembly.
* Add CRC16 checksum to Memory Visualizer.
* Add help for the main window's tab bar.

## [1.12] - 2022-10-22

* Added Gdb's Reverse Debugging mode.
    - Turn on/off gdb instruction recording.
    - Set play-back direction.
* Added Gdb's Non-Stop mode.
    - https://sourceware.org/gdb/onlinedocs/gdb/Non_002dStop-Mode.html
    - A new --non-stop-mode <yes|no>  command line flag
    - A new entry in the Debug dialog.
    - A new entry in Seer's config dialog under the gdb section.
* Added support for the older QT5.12 library.
    - Expect rendering problems with the builtin help system.
    - Urge to move to QT5.15 library.
* Main execute buttons.
    - Add '--all threads' for main execute buttons.
        - run/start/next/step/finish/continue/interrupt.
    - Individual threads can be managed by the Thread Manager.
* Thread Manager changes.
    - Thread Frame browser.
        - Allow multiple threads to be select. Selecting 1 thread will select stack frame.
        - Add next/step/finish/continue/interrupt for selected thread(s).
    - Thread Id browser.
        - Allow multiple threads to be select. Selecting 1 thread will select stack frame.
        - Add next/step/finish/continue/interrupt for selected thread(s).
    - Thread Group browser.
        - Allow multiple thread groups to be select.
        - Add continue/interrupt for selected thread group(s).
        - Add run/start for selected thread group(s). Disabled for now.
* Assembly view changes.
    - Showing Address, Offset, and Opcodes columns are configuable from the Config Dialog.
    - ^F in the Assembly editor can do quick changes.
* Changing the Qt Style can be saved using Settings->Save Configuration.
* Added valgrind support.
    - https://github.com/epasveer/seer/wiki/Valgrind-and-Seer.

## [1.11] - 2022-09-26

* Thread/Process manager.
    - Add a fork/vfork mode. Follow:
        - Parent process.
        - Child process.
        - Both (switchable via thread manager)
        - (see gdb's follow-fork-mode and detach-on-fork modes)
    - Add thread execution mode. Execute:
        - Selected thread only.
        - All threads
        - (see gdb's scheduler-locking mode)
    - Add multi-process execution mode.
        - Execute selected process.
        - Execute all processes.
        - (see gdb's schedule-multiple mode)
* Add Help to Thread/Process browser.
* Add Help to Stack Frame browser.
* Add Help to Variable/Register browser.
* Add Help to Source/Symbol/Library browser.
* Add Help to Struct Visualizer.
* Add Help to Memory Visualizer.
* Add Help to Array Visualizer.
* Add Help to Code Manager.
* Add Help to Breakpoint/Gdb log Manager.
* Add RMB menu to progress indicator to select indicator type.

## [1.10] - 2022-09-22

* New Struct Visualizer.
    - Built apon gdb's -var-obj framework.
    - Can recursively show the contents of a struct/class.
    - Show datatype of each member. (Value or pointer)
    - Can modify values of simple datatypes.
* Improve Source Browsing.
    - See new options in 'Source' config page.
    - Ignore opening files from a list of directories. Like system directories that don't exist.
    - Better sorting of program's files into "Source", "Header", and "Misc" folders in the
      SourceBrowser by having a list of filename patterns for each folder.
* The list of C/C++ file suffixes for syntax highlighting is configurable.
* Changed to CreativeCommons and GPLv3 icons.
    - Friendlier license to use icons.
    - Breeze Icon set for most things. (GPLv3)
    - Icons from these sites that require CreativeCommon license. (Simple attribution).
        icon-icons.com
        thenounproject.com

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


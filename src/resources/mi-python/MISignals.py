# SPDX-FileCopyrightText: 2025 Ernie Pasveer <epasveer@att.net>
#
# SPDX-License-Identifier: MIT

import re

#
# Python MI command to manage gdb's "signal" command.
#
#   https://www.sourceware.org/gdb/current/onlinedocs/gdb.html/Signals.html
#

class MISignal(gdb.MICommand):
    """
    Run the 'signal' command.

    -signal-list-values    List all signals, including the attributes for each signal (name, stop, print, pass, description).


    (gdb) info signals
    Signal          Stop    Print   Pass to program         Description

    SIGHUP          Yes     Yes     Yes                     Hangup
    SIGINT          Yes     Yes     No                      Interrupt
    SIGQUIT         Yes     Yes     Yes                     Quit
    ...
    EXC_BREAKPOINT  Yes     Yes     Yes                     Breakpoint
    SIGLIBRT        No      No      Yes                     librt internal signal

    Use the "handle" command to change these tables.
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MISignal, self).__init__(name)

    def invoke(self, argv):

        if self._mode == "list-values":

            command = ""
            result  = ""

            signalentries = []

            if (len(argv) == 0):
                command = "info signals"
            else:
                command = "handle " + " ".join(argv)

            result = gdb.execute (command, from_tty=True, to_string=True)

            lines = result.split("\n")
            for line in lines:
                if (line == ""):
                    continue

                columns = re.search(r"^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)$", line)
                if columns:
                    if (columns.group(1) == "Signal"):
                        continue
                    if (columns.group(1) == "Use"):
                        continue
                    signalmeta = {}
                    signalmeta["name"]          = columns.group(1)
                    signalmeta["stop"]          = columns.group(2)
                    signalmeta["print"]         = columns.group(3)
                    signalmeta["pass"]          = columns.group(4)
                    signalmeta["description"]   = columns.group(5)

                    signalentries.append(signalmeta)

            return { "signal-values": signalentries}

        elif self._mode == "list-names":

            signalnames = []

            result = gdb.execute ("info signals", to_string=True)

            lines = result.split("\n")
            for line in lines:
                if (line == ""):
                    continue

                columns = re.search(r"^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)$", line)
                if columns:
                    if (columns.group(1) == "Signal"):
                        continue
                    if (columns.group(1) == "Use"):
                        continue

                    signalnames.append(columns.group(1))

            return { "signal-names": signalnames}

        elif self._mode == "stop":

            gdb.execute ("handle " + " ".join(argv) + " stop", from_tty=True, to_string=True)
            return None

        elif self._mode == "nostop":

            gdb.execute ("handle " + " ".join(argv) + " nostop", from_tty=True, to_string=True)
            return None

        elif self._mode == "print":

            gdb.execute ("handle " + " ".join(argv) + " print", from_tty=True, to_string=True)
            return None

        elif self._mode == "noprint":

            gdb.execute ("handle " + " ".join(argv) + " noprint", from_tty=True, to_string=True)
            return None

        elif self._mode == "pass":

            gdb.execute ("handle " + " ".join(argv) + " pass", from_tty=True, to_string=True)
            return None

        elif self._mode == "nopass":

            gdb.execute ("handle " + " ".join(argv) + " nopass", from_tty=True, to_string=True)
            return None

        else:
            raise gdb.GdbError("signal: Invalid parameter: %s" % self._mode)

MISignal("-signal-list-names",  "list-names")
MISignal("-signal-list-values", "list-values")
MISignal("-signal-stop",        "stop")
MISignal("-signal-nostop",      "nostop")
MISignal("-signal-print",       "print")
MISignal("-signal-noprint",     "noprint")
MISignal("-signal-pass",        "pass")
MISignal("-signal-nopass",      "nopass")


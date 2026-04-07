# SPDX-FileCopyrightText: 2025 Ernie Pasveer <epasveer@att.net>
#
# SPDX-License-Identifier: MIT

import re

#
# Python MI command to manage gdb's Objective-C commands.
#
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/Objective_002dC.html#Objective_002dC
#

class MIObjectiveC(gdb.MICommand):
    """
    Run the ObjectiveC command.

    -objc-evaluate-expression     Evaluate an Objective-C object via 'print-object' and '_NSPrintForDebugger'.

    See: https://sourceware.org/gdb/current/onlinedocs/gdb.html/Objective_002dC.html#Objective_002dC
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MIObjectiveC, self).__init__(name)

    def invoke(self, argv):
        if self._mode == "evaluate-expression":

            result = gdb.execute ("print-object " + " ".join(argv), to_string=True)

            return { "value": result.rstrip('\n')}

        else:
            raise gdb.GdbError("objectivec: Invalid parameter: %s" % self._mode)

MIObjectiveC("-objc-evaluate-expression", "evaluate-expression")


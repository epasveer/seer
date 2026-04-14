# SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
#
# SPDX-License-Identifier: MIT

import re

#
# Python MI command to support gdb's "start" commands.
#

class MIStart(gdb.MICommand):
    """
    Run the 'start' or 'starti' command.

    -exec-start              Start the program, break in main. See gdb's 'start' command.
    -exec-starti             Start the program, break at first instruction. See gdb's 'starti' command.

    See:
        https://sourceware.org/gdb/current/onlinedocs/gdb.html/Starting.html
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MIStart, self).__init__(name)

    def invoke(self, argv):
        if self._mode == "start":
            gdb.execute ("start" + " ".join(argv), to_string=True)
            return None
        elif self._mode == "starti":
            gdb.execute ("starti" + " ".join(argv), to_string=True)
            return None
        else:
            raise gdb.GdbError("start: Invalid parameter: %s" % self._mode)

MIStart("-exec-start",   "start")
MIStart("-exec-starti",  "starti")


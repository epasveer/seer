# SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
#
# SPDX-License-Identifier: MIT

#
# Python MI command to support gdb's "monitor" command.
#

class MIMonitor(gdb.MICommand):
    """
    Send a command to gdb's monitor.

    -monitor-exec            Run a command in gdb's monitor.
                             Return the resulting text, if any.

    See:
        https://sourceware.org/gdb/current/onlinedocs/gdb.html/Server.html
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MIMonitor, self).__init__(name)

    def invoke(self, argv):

        if self._mode == "monitor-exec":

            result = gdb.execute ("monitor " + " ".join(argv), to_string=True)

            return {"monitor-output": result}

        else:
            raise gdb.GdbError("monitor: Invalid parameter: %s" % self._mode)

MIMonitor("-monitor-exec", "monitor-exec")


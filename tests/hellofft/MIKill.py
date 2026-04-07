import re

#
# Python MI command to support gdb's "kill" commands.
#
#   https://sourceware.org/bugzilla/show_bug.cgi?id=31090
#   https://stackoverflow.com/questions/9669025/gdb-exit-program-without-exiting-gdb


class MIKill(gdb.MICommand):
    """
    Run the 'kill' command.

    -exec-kill               Kill the child process in which your program is running under GDB.
    -exec-kill-inferiors     Kill the inferior or inferiors identified by GDB inferior number(s) infno.

    See:
        https://sourceware.org/gdb/current/onlinedocs/gdb.html/Kill-Process.html
        https://sourceware.org/gdb/current/onlinedocs/gdb.html/Inferiors-Connections-and-Programs.html#Inferiors-Connections-and-Programs
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MIKill, self).__init__(name)

    def invoke(self, argv):
        if self._mode == "kill":
            gdb.execute ("kill" + " ".join(argv), to_string=True)
            return None
        elif self._mode == "killinferiors":
            gdb.execute ("kill inferiors" + " ".join(argv), to_string=True)
            return None
        else:
            raise gdb.GdbError("kill: Invalid parameter: %s" % self._mode)

MIKill("-exec-kill",             "kill")
MIKill("-exec-kill-inferiors",   "killinferiors")


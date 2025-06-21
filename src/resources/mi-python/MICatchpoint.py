#
# Python MI command to manage gdb's "catchpoint" command..
#
# Gdb has most catchpoint MI commands, but are missing these:
#
#   -catch-signal
#   -catch-fork
#   -catch-vfork
#   -catch-syscall
#   -catch-exec
#

class MICatchpoint(gdb.MICommand):
    """
    Run the 'catchpoint' command.

    -catch-signal [signal... | ‘all’]                                       The delivery of a signal.
    -catch-fork                                                             A call to fork.
    -catch-vfork                                                            A call to vfork.
    -catch-syscall [name | number | group:groupname | g:groupname] ...      A call to or return from a system call, a.k.a. syscall.
    -catch-exec                                                             A call to exec.

    See: https://sourceware.org/gdb/current/onlinedocs/gdb.html/Set-Catchpoints.html#Set-Catchpoints
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MICatchpoint, self).__init__(name)

    def invoke(self, argv):
        if self._mode == "signal":
            gdb.execute ("catch signal " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "fork":
            gdb.execute ("catch fork " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "vfork":
            gdb.execute ("catch vfork " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "syscall":
            gdb.execute ("catch syscall " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "exec":
            gdb.execute ("catch exec " + " ".join(argv), to_string=True)
            return None
        else:
            raise gdb.GdbError("catchpoint: Invalid parameter: %s" % self._mode)

MICatchpoint("-catch-signal",  "signal")
MICatchpoint("-catch-fork",    "fork")
MICatchpoint("-catch-vfork",   "vfork")
MICatchpoint("-catch-syscall", "syscall")
MICatchpoint("-catch-exec",    "exec")


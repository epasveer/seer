#
# Gdb's own custon Python MI command.
#
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/GDB_002fMI-Commands-In-Python.html#GDB_002fMI-Commands-In-Python
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/Basic-Python.html#Basic-Python
#

class MIEcho(gdb.MICommand):
    """Echo arguments passed to the command."""

    def __init__(self, name, mode):
        self._mode = mode
        super(MIEcho, self).__init__(name)

    def invoke(self, argv):
        if self._mode == 'dict':
            return { 'dict': { 'argv' : argv } }
        elif self._mode == 'list':
            return { 'list': argv }
        else:
            return { 'string': ", ".join(argv) }


MIEcho("-echo-dict", "dict")
MIEcho("-echo-list", "list")
MIEcho("-echo-string", "string")


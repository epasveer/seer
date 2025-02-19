
class MISkip(gdb.MICommand):
    """Run the 'skip' command."""

    def __init__(self, name, mode):
        self._mode = mode
        super(MISkip, self).__init__(name)

    def invoke(self, argv):
        if self._mode == 'dict':
            return { 'dict': { 'argv' : argv } }
        elif self._mode == 'list':
            return { 'list': gdb.execute ("info skip", to_string=True) }
        else:
            return { 'string': ", ".join(argv) }


MISkip("-skip-list", "list")


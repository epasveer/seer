import gdb
import itertools
import re
import time

class QStringPrinter:

    def __init__(self, val):
        self.val = val

    def to_string(self):
        ret = ""

        # The QString object may not be initialized yet. In this case 'size' is a bogus value
        # or in case of Qt5, 'd' is an invalid pointer and the following lines might throw memory
        # access error. Hence the try/catch.
        try:
            size = self.val['d']['size']
            if size == 0:
                return ret

            dataAsCharPointer = self.val['d']['ptr'].cast(gdb.lookup_type("char").pointer())

            ret = dataAsCharPointer.string(encoding = 'UTF-16', length = size * 2)

        except Exception as e:
            # swallow the exception and return empty string
            # pass
            return str(e)
        return ret

    def display_hint (self):
        return 'string'


def QStringPrinter_func(val):
    if str(val.type) == 'QString':
        return QStringPrinter(val)

gdb.pretty_printers.append(QStringPrinter_func)



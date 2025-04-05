import re

#
# Python MI command to manage gdb's "skip" command..
#
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/Skipping-Over-Functions-and-Files.html
#

class MISkip(gdb.MICommand):
    """
    Run the 'skip' command.

    -skip-list               List all skips, including the id for each skip.
    -skip-delete             Delete a list of skip id's.
    -skip-enable             Enable a list of skip id's.
    -skip-disable            Disable a list of skip id's.
    -skip-create             Functions described in manual syntax will be skipped over when stepping.
    -skip-create-file        Functions in file will be skipped over when stepping.
    -skip-create-gfile       Functions in files matching file-glob-pattern will be skipped over when stepping.
    -skip-create-function    Functions named by linespec or the function containing the line named by linespec will be skipped over when stepping.
    -skip-create-rfunction   Functions whose name matches regexp will be skipped over when stepping.

    See: https://sourceware.org/gdb/current/onlinedocs/gdb.html/Skipping-Over-Functions-and-Files.html
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MISkip, self).__init__(name)

    def invoke(self, argv):
        if self._mode == "list":

            skipentries = []

            result = gdb.execute ("info skip " + " ".join(argv), to_string=True)

            lines = result.split("\n")
            for line in lines:
                if (line == ""):
                    continue

                columns = re.search(r"^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)$", line)
                if columns:
                    if (columns.group(1) == "Num"):
                        continue
                    if (columns.group(1) == "Not"):
                        continue
                    skipmeta = {}
                    skipmeta["number"]   = columns.group(1)
                    skipmeta["enable"]   = columns.group(2)
                    skipmeta["glob"]     = columns.group(3)
                    skipmeta["file"]     = columns.group(4)
                    skipmeta["re"]       = columns.group(5)
                    skipmeta["function"] = columns.group(6)

                    skipentries.append(skipmeta)

            return { "skips": skipentries}

        elif self._mode == "delete":
            gdb.execute ("skip delete " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "enable":
            gdb.execute ("skip enable " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "disable":
            gdb.execute ("skip disable " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "create":
            gdb.execute ("skip \"" + " ".join(argv) + "\"", to_string=True)
            return None
        elif self._mode == "createfile":
            gdb.execute ("skip -file \"" + " ".join(argv) + "\"", to_string=True)
            return None
        elif self._mode == "creategfile":
            gdb.execute ("skip -gfile \"" + " ".join(argv) + "\"", to_string=True)
            return None
        elif self._mode == "createfunction":
            gdb.execute ("skip -function \"" + " ".join(argv) + "\"", to_string=True)
            return None
        elif self._mode == "createrfunction":
            gdb.execute ("skip -rfunction \"" + " ".join(argv) + "\"", to_string=True)
            return None
        else:
            raise gdb.GdbError("skips: Invalid parameter: %s" % self._mode)

MISkip("-skip-list",             "list")
MISkip("-skip-delete",           "delete")
MISkip("-skip-enable",           "enable")
MISkip("-skip-disable",          "disable")
MISkip("-skip-create",           "create")
MISkip("-skip-create-file",      "createfile")
MISkip("-skip-create-gfile",     "creategfile")
MISkip("-skip-create-function",  "createfunction")
MISkip("-skip-create-rfunction", "createrfunction")


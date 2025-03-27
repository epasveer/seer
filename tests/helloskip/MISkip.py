import re

class MISkip(gdb.MICommand):
    """
    Run the "skip" command.

    https://sourceware.org/gdb/current/onlinedocs/gdb.html/Skipping-Over-Functions-and-Files.html

    ^done,stack=[frame={level="0",addr="0x00000000004008e5",func="foo",file="helloskip.cpp",fullname="/nas/erniep/Development/seer/tests/helloskip/helloskip.cpp",line="8",arch="i386:x86-64"},
                 frame={level="1",addr="0x0000000000400994",func="main",file="helloskip.cpp",fullname="/nas/erniep/Development/seer/tests/helloskip/helloskip.cpp",line="17",arch="i386:x86-64"}]

    //          +--------1---------2---------3---------4-----------------------------------------------------------------
    //          1--------0---------0---------0---------0-----------------------------------------------------------------
    ^done,list="Num   Enb Glob File                 RE Function
                1     y      n <none>                n std::vector<int, std::allocator<int> >::back()
                2     y      n <none>                n std::unique_ptr<int, std::default_delete<int> >::operator*() const"

    ^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)$

    def extract_second_column_to_end(text):
    match = re.search(r"^[^\\s]+\\s+(.*)$", text)
    if match:
        return match.group(1)
    return None

    Here is the output!

    ^done,skips=[
                  {number="1",enable="n",glob="n",file="<none>",re="n",function="std::vector<int, std::allocator<int> >::back()"},
                  {number="2",enable="n",glob="n",file="<none>",re="n",function="std::unique_ptr<int, std::default_delete<int> >::operator*() const"}
                ]

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
            return gdb.execute ("skip delete " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "enablex":
            result = gdb.execute ("skip enable " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "disable":
            gdb.execute ("skip disable " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "createfile":
            gdb.execute ("skip -file " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "creategfile":
            gdb.execute ("skip -gfile " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "createfunction":
            gdb.execute ("skip -function " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "createrfunction":
            gdb.execute ("skip -ffunction " + " ".join(argv), to_string=True)
            return None
        else:
            raise gdb.GdbError("skips: Invalid parameter: %s" % self._mode)

MISkip("-skip-list",             "list")
MISkip("-skip-delete",           "delete")
MISkip("-skip-enable",           "enable")
MISkip("-skip-disable",          "disable")
MISkip("-skip-create-file",      "createfile")
MISkip("-skip-create-gfile",     "creategfile")
MISkip("-skip-create-function",  "createfunction")
MISkip("-skip-create-rfunction", "createrfunction")


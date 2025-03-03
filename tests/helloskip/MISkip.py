
class MISkip(gdb.MICommand):
    """
    Run the "skip" command.

    https://sourceware.org/gdb/current/onlinedocs/gdb.html/Skipping-Over-Functions-and-Files.html

    ^done,stack=[frame={level="0",addr="0x00000000004008e5",func="foo",file="helloskip.cpp",fullname="/nas/erniep/Development/seer/tests/helloskip/helloskip.cpp",line="8",arch="i386:x86-64"},
                 frame={level="1",addr="0x0000000000400994",func="main",file="helloskip.cpp",fullname="/nas/erniep/Development/seer/tests/helloskip/helloskip.cpp",line="17",arch="i386:x86-64"}]

    ^done,list="Num   Enb Glob File                 RE Function
                1     y      n <none>                n std::vector<int, std::allocator<int> >::back()
                2     y      n <none>                n std::unique_ptr<int, std::default_delete<int> >::operator*() const"
    """


    def __init__(self, name, mode):
        self._mode = mode
        super(MISkip, self).__init__(name)

    def invoke(self, argv):
        print(self._mode)
        print(argv)
        if self._mode == "list":
            return { "skiplist": gdb.execute ("info skip " + " ".join(argv), to_string=True) }
        elif self._mode == "delete":
            return { "skipdelete": gdb.execute ("skip delete " + " ".join(argv), to_string=True) }
        elif self._mode == "enable":
            return { "skipenable": gdb.execute ("skip enable " + " ".join(argv), to_string=True) }
        elif self._mode == "disable":
            return { "skipedisable": gdb.execute ("skip disable " + " ".join(argv), to_string=True) }
        elif self._mode == "createfile":
            return { "skipcreated": gdb.execute ("skip -file " + " ".join(argv), to_string=True) }
        elif self._mode == "creategfile":
            return { "skipcreated": gdb.execute ("skip -gfile " + " ".join(argv), to_string=True) }
        elif self._mode == "createfunction":
            return { "skipcreated": gdb.execute ("skip -function " + " ".join(argv), to_string=True) }
        elif self._mode == "createrfunction":
            return { "skipcreated": gdb.execute ("skip -ffunction " + " ".join(argv), to_string=True) }
        else:
            raise gdb.GdbError("Invalid parameter: %s" % self._mode)

MISkip("-skip-list",             "list")
MISkip("-skip-delete",           "delete")
MISkip("-skip-enable",           "enable")
MISkip("-skip-disable",          "disable")
MISkip("-skip-create-file",      "createfile")
MISkip("-skip-create-gfile",     "creategfile")
MISkip("-skip-create-function",  "createfunction")
MISkip("-skip-create-rfunction", "createrfunction")


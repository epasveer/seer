import re

#
# Python MI command to manage gdb's "checkpoint" command..
#
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/Checkpoint_002fRestart.html#Checkpoint_002fRestart
#

class MICheckpoint(gdb.MICommand):
    """
    Run the 'checkpoint' command.

    -checkpoint-list         List all checkpoints, including the id for each checkpoint.
    -checkpoint-create       Create a checkpoint at the current debugging position.
    -checkpoint-select       Select a checkpoint to make active as described by ID.
    -checkpoint-delete       Delete a checkpoint described by ID.

    See: https://sourceware.org/gdb/current/onlinedocs/gdb.html/Checkpoint_002fRestart.html#Checkpoint_002fRestart
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MICheckpoint, self).__init__(name)

    def invoke(self, argv):
        if self._mode == "list":

            checkpointentries = []

            result = gdb.execute ("info checkpoints " + " ".join(argv), to_string=True)
            print(result)

            lines = result.split("\n")
            for line in lines:
                if (line == ""):
                    continue

                columns = re.search(r"^([ *])\ (\d+)\ (.*?)\,\ (.*?)\,\ (.*?)$", line)
                if columns:
                    checkpointmeta = {}
                    checkpointmeta["id"]        = columns.group(2)
                    checkpointmeta["state"]     = columns.group(1)
                    checkpointmeta["process"]   = columns.group(3)
                    checkpointmeta["file"]      = columns.group(4)
                    checkpointmeta["line"]      = columns.group(5)

                    checkpointentries.append(checkpointmeta)

                else:
                    columns = re.search(r"^([ *])\ (\d+)\ (.*)$", line)
                    if columns:
                        checkpointmeta = {}
                        checkpointmeta["id"]        = columns.group(2)
                        checkpointmeta["state"]     = columns.group(1)
                        checkpointmeta["process"]   = columns.group(3)
                        checkpointmeta["file"]      = ""
                        checkpointmeta["line"]      = ""

                        checkpointentries.append(checkpointmeta)

            return { "checkpoints": checkpointentries}

        elif self._mode == "create":
            gdb.execute ("checkpoint " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "select":
            gdb.execute ("select " + " ".join(argv), to_string=True)
            return None
        elif self._mode == "delete":
            gdb.execute ("delete checkpoint " + " ".join(argv), to_string=True)
            return None
        else:
            raise gdb.GdbError("checkpoints: Invalid parameter: %s" % self._mode)

MICheckpoint("-checkpoint-list",    "list")
MICheckpoint("-checkpoint-create",  "create")
MICheckpoint("-checkpoint-select",  "select")
MICheckpoint("-checkpoint-delete",  "delete")


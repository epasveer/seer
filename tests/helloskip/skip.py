#!/usr/bin/python3

# Play around with parsing the results of a 'skip list' command.

import re

result  = "1    y      n <none>                n std::vector<int, std::allocator<int> >::back()"
columns = re.search(r"^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(.*)$", result)

if columns:
    """
    print(columns.group(1))
    print(columns.group(2))
    print(columns.group(3))
    print(columns.group(4))
    print(columns.group(5))
    print(columns.group(6))
    """

    skipmeta = []
    skipmeta.append("number="   + "\"" + columns.group(1) + "\"")
    skipmeta.append("enable="   + "\"" + columns.group(2) + "\"")
    skipmeta.append("glob="     + "\"" + columns.group(3) + "\"")
    skipmeta.append("file="     + "\"" + columns.group(4) + "\"")
    skipmeta.append("re="       + "\"" + columns.group(5) + "\"")
    skipmeta.append("function=" + "\"" + columns.group(6) + "\"")

    skipentry = "{" + ",".join(skipmeta) + "}"

    skipentries = []
    skipentries.append(skipentry)

    print("[" + ",".join(skipentries) + "]")

else:
    print("No match")


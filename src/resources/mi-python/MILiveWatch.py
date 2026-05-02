# SPDX-FileCopyrightText: 2026 Nguyen Minh Quang <nguyenminhquangcn1@gmail.com>
#
# SPDX-License-Identifier: MIT

# Custom GDB Command: -var-create-live-watch var_1 @ "i"
# OUTPUT: ^done,name="var_1",numchild="0",value="556",type="int",has_more="0",custom_value="TrackerID"

# Custom GDB Command: -var-update-live-watch --all-values var_1
# OUTPUT: ^done,name="var_1",numchild="0",value="557",type="int",has_more="0",custom_value="TrackerID"

# Array of structs: [{"name": ..., "expr": ..., "id": ...}, ...]
import gdb
_live_watches = []

class VarCreateLiveWatch(gdb.MICommand):
	"""Create a live watch variable and store its expression for later updates.
	Arguments:
		-var-create-live-watch <name> <frame> <expr> <ID>
		name:	The variable alias.
		frame: 	The stack frame to evaluate the expression in: *, @, 0x<address>
		expr: 	The variable name.
		ID: 	The unique identifier for the watch, represented as a number in tracker
	Eg: -var-create-live-watch var_i @ "i" 1
	"""
	def __init__(self):
		super().__init__("-var-create-live-watch")

	def invoke(self, argv):
		# 1. Check arguments
		if len(argv) < 4:
			raise gdb.GdbError(f"-var-create-live-watch: expected <name> <frame> <expr> <ID>, got {argv}")

		name 	= argv[0]
		frame 	= argv[1]
		expr 	= argv[2]
		id 		= argv[3]

		# 2. Append struct to _live_watches buffer
		_live_watches.append({"name": name, "expr": expr, "id": id})
		# 3. Create gdb alias
		cmd = f'interpreter-exec mi "-var-create {name} {frame} {expr}"'
		output = gdb.execute(cmd, to_string=True)
		return

class VarUpdateLiveWatch(gdb.MICommand):
	"""Re-evaluate a live watch variable created with -var-create-live-watch.
	Read the output and reassemble to update the variable in tracker.
	Arguments:
		-var-update-live-watch
	"""
	def __init__(self):
		super().__init__("-var-update-live-watch")

	def invoke(self, argv):
		# interpreter-exec mi output goes to GDB's MI stdout and is NOT captured
		# by gdb.execute(..., to_string=True), so use gdb.parse_and_eval() directly.
		results = []
		for w in _live_watches:
			try:
				val = gdb.parse_and_eval(w["expr"])
				results.append({"id": w["id"], "value": str(val)})
			except gdb.error:
				pass
		return {"live_watch_results": results}



class VarDeleteLiveWatch(gdb.MICommand):
	"""Delete a live watch variable created with -var-create-live-watch.
	Arguments:
		-var-delete-live-watch <ID>
		ID: The unique identifier for the watch.
	"""
	def __init__(self):
		super().__init__("-var-delete-live-watch")

	def invoke(self, argv):
		id = argv[-1]

		# 1. Find entry by ID
		entry = next((w for w in _live_watches if w["id"] == id), None)
		if entry is None:
			return

		_live_watches.remove(entry)
		return

VarCreateLiveWatch()
VarUpdateLiveWatch()
VarDeleteLiveWatch()
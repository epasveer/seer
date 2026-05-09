# SPDX-FileCopyrightText: 2026 Nguyen Minh Quang <nguyenminhquangcn1@gmail.com>
#
# SPDX-License-Identifier: MIT

import gdb
_live_watches = []

class VarCreateLiveWatch(gdb.MICommand):
	"""Create a live watch variable and store its expression for later updates.
	Arguments:
		-var-create-live-watch <name> <ID>
		name:	The variable name.
		ID: 	The unique identifier for the watch, represented as a number in tracker
	Eg: -var-create-live-watch var_i 1
	"""
	def __init__(self):
		super().__init__("-var-create-live-watch")

	def invoke(self, argv):
		# 1. Check arguments
		if len(argv) < 2:
			raise gdb.GdbError(f"-var-create-live-watch: expected <name> <ID>, got {argv}")

		expr 	= argv[0]
		id 		= argv[1]

		# 2. Append struct to _live_watches buffer
		_live_watches.append({"expr": expr, "id": id})
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
		for w in _live_watches:
			try:
				cmd = f'interpreter-exec mi "{w["id"]}-data-evaluate-expression {w["expr"]}"'
				gdb.execute(cmd, to_string=True)
			except gdb.error:
				pass
		return

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
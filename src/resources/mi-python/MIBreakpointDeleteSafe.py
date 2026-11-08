# SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
#
# SPDX-License-Identifier: MIT

#
# Python MI command to delete one or more breakpoints safely, even while the
# target is currently running - the same problem '-break-insert-safe' solves
# for insertion, gdb's normal '-break-delete' has for deletion: it refuses to
# touch a live remote target while it's running.
#
# See MIBreakpointInsertSafe.py for the full reasoning behind every piece of
# this - the design here is identical, just swapping the create-a-breakpoint
# step for a delete-these-breakpoints step:
#
#   - gdb.execute("interrupt") doesn't block until the target actually stops,
#     and gdb's event loop can't process the pending remote reply while this
#     command is still running - so wait via a gdb.events.stop callback
#     rather than a synchronous loop.
#   - Do the delete/resume work via gdb.post_event() rather than directly
#     inside the stop-event callback, or a second call arriving while the
#     first's resume is still nested inside that callback hangs gdb outright.
#   - If the target was actually already stopped by the time 'interrupt'
#     reached gdb, gdb.events.stop never fires (nothing transitions) - a
#     timer breaks that wait after a few seconds, falling back to the
#     'already stopped' behavior (delete without resuming).
#   - Resume with 'continue &', not 'continue' - the plain form blocks this
#     call, and therefore all of gdb's command processing, until gdb next has
#     a *stopped to report, which on a hot/recursive location can take a very
#     long time (or effectively forever) even though gdb itself isn't hung.
#
#   -break-delete-safe NUMBER [NUMBER ...]
#
# See:
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/GDB_002fMI-Breakpoint-Commands.html
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/Events-In-Python.html
#

import threading

# How long to wait for gdb.events.stop before assuming the interrupt was a
# no-op (target was already stopped) rather than genuinely still pending.
# Generous enough to cover a slow/real remote link's round trip.
_STOP_EVENT_TIMEOUT_SECS = 3.0


class MIBreakpointDeleteSafe(gdb.MICommand):
    """
    Delete one or more breakpoints, stopping the target first if it's
    running, and resuming it afterward - but only if this command is the
    one that stopped it. If the target was already stopped (e.g. the user
    is removing a breakpoint while paused at another one, or the
    running-check below raced with the target actually stopping), it is
    left stopped.

    -break-delete-safe NUMBER [NUMBER ...]
    """

    def __init__(self, name):
        super(MIBreakpointDeleteSafe, self).__init__(name)

    def invoke(self, argv):
        if not argv:
            raise gdb.GdbError("break-delete-safe: At least one breakpoint number is required.")

        numbers = " ".join(argv)

        thread = gdb.selected_thread()
        was_running = thread is not None and thread.is_running()

        if not was_running:
            gdb.execute("delete " + numbers, to_string=True)
            return None

        # 'state' carries the outcome across on_stop/on_timeout, whichever
        # fires first - each disconnects/cancels the other's path so only
        # one of them ever actually acts.
        state = {"settled": False, "timer": None}

        def settle(resume):
            if state["settled"]:
                return
            state["settled"] = True
            if state["timer"] is not None:
                state["timer"].cancel()
            try:
                gdb.execute("delete " + numbers, to_string=True)
            finally:
                if resume:
                    # We're the one who stopped it, so we're the one who resumes it.
                    gdb.execute("continue &", to_string=True)

        def on_stop(event):
            gdb.events.stop.disconnect(on_stop)
            gdb.post_event(lambda: settle(True))

        def on_timeout():
            # No stop event arrived - see the file header for why. Delete
            # without resuming, the same as the 'already stopped' case above.
            try:
                gdb.events.stop.disconnect(on_stop)
            except ValueError:
                pass
            settle(False)

        gdb.events.stop.connect(on_stop)
        gdb.execute("interrupt", to_string=True)

        state["timer"] = threading.Timer(_STOP_EVENT_TIMEOUT_SECS, lambda: gdb.post_event(on_timeout))
        state["timer"].daemon = True
        state["timer"].start()

        return None


MIBreakpointDeleteSafe("-break-delete-safe")

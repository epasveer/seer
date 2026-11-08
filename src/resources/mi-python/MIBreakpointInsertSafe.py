# SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
#
# SPDX-License-Identifier: MIT

#
# Python MI command to insert a breakpoint safely, even while the target is
# currently running - e.g. in Seer's 'connect' mode, where gdb's normal
# '-break-insert' refuses to touch a live remote target while it's running.
#
# This runs entirely inside gdb, using gdb.events.stop rather than a
# blocking wait: gdb.execute("interrupt") does not block until the target
# actually stops (it behaves like gdb/mi's asynchronous '-exec-interrupt' -
# it only confirms the request was accepted), and gdb's own event loop
# cannot make progress on the pending remote reply while a Python command is
# still running inside invoke() - so a synchronous wait loop here would just
# hang gdb until it timed out. Registering a one-shot gdb.events.stop
# handler and returning immediately avoids that: gdb's event loop delivers
# the callback once it has genuinely processed the target's stop, and only
# then is it safe to insert the breakpoint and resume.
#
# The actual insert/resume work is deferred one more step, via
# gdb.post_event(), rather than being run directly inside the stop-event
# callback. Running gdb.execute("break ...")/gdb.execute("continue")
# directly inside the callback works for a single insert, but a *second*
# '-break-insert-safe' arriving while the first one's resume is still
# nested inside that callback reliably hangs gdb outright (confirmed by
# testing - it stops responding even to a raw SIGINT). Deferring via
# post_event() avoids nesting the resume inside the stop-event dispatch and
# fixes that hang; multiple inserts fired back-to-back are simply processed
# one after another, since gdb won't even look at the next queued command
# until the current stop-event/callback processing has settled.
#
# There's one more failure mode this has to guard against: if the target
# was actually already stopped by the time '-exec-interrupt' reaches gdb
# (e.g. it stopped on its own between the is_running() check above and the
# interrupt being sent, or over a slow/real remote link the two can simply
# race), the interrupt is a no-op - and confirmed by testing, gdb does NOT
# fire gdb.events.stop for that no-op, since nothing actually transitions.
# The one-shot handler above would then wait forever for a stop that will
# never come, silently wedging that gdb session (not a gdb hang - gdb stays
# fully responsive to everything else - just this command's own state
# stuck, so the breakpoint never gets inserted and the target, if it really
# had been running, never gets resumed). A short timer breaks that: if no
# stop event shows up within a couple of seconds, treat it exactly like the
# 'already stopped' case up front - insert without resuming, rather than
# risk wrongly resuming a target the user meant to leave stopped.
#
# Location/option parsing: this accepts the same flags Seer's own
# '-break-insert' callers already build (see SeerBreakpointCreateDialog,
# SeerEditorWidgetSourceAreas, SeerEditorWidgetAssemblyAreas), which use
# gdb/mi's own '-break-insert' flag spelling (long options, e.g.
# '--source FILE --line N'). Those aren't valid input to the plain 'break'
# CLI command or to gdb.Breakpoint()'s location spec, which both expect the
# short-option spelling ('-source FILE -line N') instead - so invoke()
# below translates before creating the breakpoint.
#
#   -break-insert-safe [-t] [-h] [-f] [-d] [-c CONDITION] [-i IGNORE-COUNT]
#                       [-p THREAD-ID] [--source FILE] [--function NAME]
#                       [--label LABEL] [--line LINE] [*ADDRESS]
#
# See:
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/GDB_002fMI-Breakpoint-Commands.html
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/Events-In-Python.html
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/Breakpoints-In-Python.html
#

import threading

# How long to wait for gdb.events.stop before assuming the interrupt was a
# no-op (target was already stopped) rather than genuinely still pending.
# Generous enough to cover a slow/real remote link's round trip.
_STOP_EVENT_TIMEOUT_SECS = 3.0


def _requote(token):
    # A token that contains whitespace (e.g. a source path) needs
    # requoting before being handed back to gdb, or it will be seen as more
    # than one argument.
    if any(c.isspace() for c in token):
        return '"' + token.replace('"', '\\"') + '"'
    return token


def _parse_breakpoint_args(argv):
    """
    Translate a gdb/mi '-break-insert'-style argv (long options) into a
    gdb.Breakpoint() location spec (short options) plus the handful of
    settings gdb.Breakpoint() doesn't take as constructor/location
    arguments (condition, ignore count, thread, disabled).
    """

    options = {
        "temporary":    False,
        "hardware":     False,
        "pending":      False,
        "disabled":     False,
        "condition":    None,
        "ignore_count": None,
        "thread":       None,
    }

    location_parts = []

    i = 0
    while i < len(argv):
        arg = argv[i]

        if arg == "-t":
            options["temporary"] = True
        elif arg == "-h":
            options["hardware"] = True
        elif arg == "-f":
            options["pending"] = True
        elif arg == "-d":
            options["disabled"] = True
        elif arg == "-c" and i + 1 < len(argv):
            i += 1
            options["condition"] = argv[i]
        elif arg == "-i" and i + 1 < len(argv):
            i += 1
            options["ignore_count"] = int(argv[i])
        elif arg == "-p" and i + 1 < len(argv):
            i += 1
            options["thread"] = int(argv[i])
        elif arg in ("--source", "--function", "--label", "--line") and i + 1 < len(argv):
            i += 1
            location_parts.append("-%s %s" % (arg[2:], _requote(argv[i])))
        else:
            # A bare linespec/address (e.g. '*0xADDR', 'file.c:10', 'func')
            # not using the '--flag value' form. Pass it through as-is.
            location_parts.append(_requote(arg))

        i += 1

    if not location_parts:
        raise gdb.GdbError("break-insert-safe: No breakpoint location given.")

    return " ".join(location_parts), options


def _create_breakpoint(spec, options):

    bp_type = gdb.BP_HARDWARE_BREAKPOINT if options["hardware"] else gdb.BP_BREAKPOINT

    previous_pending = None
    if options["pending"]:
        previous_pending = gdb.parameter("breakpoint pending")
        gdb.execute("set breakpoint pending on", to_string=True)

    try:
        bp = gdb.Breakpoint(spec, type=bp_type, temporary=options["temporary"])
    finally:
        if options["pending"]:
            if previous_pending is True:
                gdb.execute("set breakpoint pending on", to_string=True)
            elif previous_pending is False:
                gdb.execute("set breakpoint pending off", to_string=True)
            else:
                gdb.execute("set breakpoint pending auto", to_string=True)

    if options["condition"] is not None:
        bp.condition = options["condition"]
    if options["ignore_count"] is not None:
        bp.ignore_count = options["ignore_count"]
    if options["thread"] is not None:
        bp.thread = options["thread"]
    if options["disabled"]:
        bp.enabled = False

    return bp


class MIBreakpointInsertSafe(gdb.MICommand):
    """
    Insert a breakpoint, stopping the target first if it's running, and
    resuming it afterward - but only if this command is the one that
    stopped it. If the target was already stopped (e.g. the user is adding
    a breakpoint while paused at another one, or the running-check above
    raced with the target actually stopping), it is left stopped.

    -break-insert-safe [-t] [-h] [-f] [-d] [-c CONDITION] [-i IGNORE-COUNT]
                        [-p THREAD-ID] [--source FILE] [--function NAME]
                        [--label LABEL] [--line LINE] [*ADDRESS]
    """

    def __init__(self, name):
        super(MIBreakpointInsertSafe, self).__init__(name)

    def invoke(self, argv):
        spec, options = _parse_breakpoint_args(argv)

        thread = gdb.selected_thread()
        was_running = thread is not None and thread.is_running()

        if not was_running:
            _create_breakpoint(spec, options)
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
                _create_breakpoint(spec, options)
            finally:
                if resume:
                    # We're the one who stopped it, so we're the one who resumes it.
                    # The trailing '&' matters: plain 'continue' blocks this call -
                    # and therefore gdb's entire command processing - until gdb next
                    # has a *stopped to report. On a location gdb has to step over on
                    # its way out (breakpoint sitting at the resume PC) that can mean
                    # thousands of internal remote round trips before anything is
                    # reported back, and confirmed by testing, all of gdb (not just
                    # this command) is unresponsive for the whole stretch - which is
                    # indistinguishable from a hang. '&' backgrounds the resume so it
                    # returns immediately, leaving gdb free to process the next
                    # command no matter how long the target takes to stop again.
                    gdb.execute("continue &", to_string=True)

        def on_stop(event):
            gdb.events.stop.disconnect(on_stop)
            gdb.post_event(lambda: settle(True))

        def on_timeout():
            # No stop event arrived - see the file header for why. Insert
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


MIBreakpointInsertSafe("-break-insert-safe")

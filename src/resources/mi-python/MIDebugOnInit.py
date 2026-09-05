# SPDX-License-Identifier: MIT
#
# Python MI command that drives gdb through "debug a kernel module from insmod".
#
#   -debug-on-init MODULE SERIAL SYMFILE SRCDIR CMD
#
#       MODULE   kernel module name, as shown by lsmod
#       SERIAL   target serial device to write CMD to (e.g. /dev/ttyUSB0)
#       SYMFILE  the module's .ko, built with debug info
#       SRCDIR   directory to add to gdb's source search path
#       CMD      shell command that loads the module on the target (insmod ...)
#
# The whole sequence runs inside gdb from one command: gdb.execute("continue")
# from a Python command blocks until the inferior stops, so the steps run in
# order with no worker thread and no MI-async races to orchestrate around - that
# is the entire point of doing this in Python instead of from Seer's C++ side.
# (We can't 'set mi-async off' - it's rejected whenever a live inferior exists,
# which for a remote/OpenOCD target is the whole session.)
#
# On success gdb is left stopped at the module's init function with the
# module's symbols loaded at their real addresses.

import gdb
import os
import re
import signal
import threading
import time

TIMEOUT_S      = 30.0    # how long to wait for a breakpoint to be hit
SERIAL_DELAY_S = 0.7     # wait after 'continue' before writing CMD, so the target is running
SKIP_SECTIONS  = {".symtab", ".strtab", ".shstrtab"}   # not placeable by add-symbol-file
LOGFILE        = os.environ.get("SEER_DEBUG_ON_INIT_LOG", "/tmp/seer-debug-on-init.log")


def _log(msg):
    # Persist progress to a file so it can be inspected even while gdb is blocked in 'continue'
    # (the MI channel is silent then). Thread-safe: one open()/write()/close() per line.
    line = "%s  %s\n" % (time.strftime("%H:%M:%S"), msg)
    try:
        with open(LOGFILE, "a") as f:
            f.write(line)
    except Exception:
        pass


class _Timeout:
    """
    Fires SIGINT into gdb after 'seconds' unless cancelled first. A blocking
    'continue' cannot be interrupted by anything else, so this simulates the
    user pressing Ctrl-C: gdb stops the target, 'continue' returns, and the
    caller notices it did not land on the expected breakpoint.
    """

    def __init__(self, seconds):
        self._lock  = threading.Lock()
        self._done  = False
        self._fired = False
        self._timer = threading.Timer(seconds, self._fire)
        self._timer.daemon = True

    def start(self):
        self._timer.start()

    def _fire(self):
        with self._lock:
            if self._done:
                return
            self._fired = True
        _log("timeout: sending SIGINT to gdb to break the blocking 'continue'")
        # Retry a few times: one SIGINT may be swallowed while gdb/OpenOCD is mid-transaction.
        for _ in range(5):
            try:
                os.kill(os.getpid(), signal.SIGINT)
            except Exception:
                pass
            time.sleep(0.5)
            with self._lock:
                if self._done:
                    return

    def cancel(self):
        with self._lock:
            self._done = True
        self._timer.cancel()

    @property
    def fired(self):
        return self._fired


def _diag(msg):
    # Progress marker: gdb console log (main thread only) + the persistent file.
    _log(msg)
    try:
        gdb.write("@debug-on-init: %s\n" % msg)
        gdb.flush()
    except Exception:
        pass


def _autobool_str(v):
    # gdb.parameter() gives an auto-boolean back as True / False / None.
    if v is None:
        return "auto"
    return "on" if v else "off"


def _target_running():
    inf = gdb.selected_inferior()
    if not inf or not inf.threads():
        return False
    return any(t.is_running() for t in inf.threads())


def _really_stopped():
    if _target_running():
        return False
    try:
        gdb.selected_frame()
        return True
    except gdb.error:
        return False




def _module_is_loaded(name):
    try:
        # The kernel's "modules" list head is `static` (kernel/module/main.c), so
        # lookup_global_symbol() alone returns None here - same fallback used for
        # load_module() below.
        sym = (gdb.lookup_global_symbol("modules")
               or gdb.lookup_static_symbol("modules"))
        modules = sym.value()
        mtype   = gdb.lookup_type("struct module")
        offset  = mtype['list'].bitpos // 8
        voidp   = gdb.lookup_type("void").pointer()
    except Exception:
        return False

    cur  = modules['next']
    head = modules.address
    guard = 0
    while cur != head and guard < 4096:
        guard += 1
        try:
            mod = (cur.cast(voidp) - offset).cast(mtype.pointer()).dereference()
            if mod['name'].string() == name:
                return True
            cur = cur['next']
        except gdb.error:
            break
    return False


def _section_name(attr):
    for path in (('battr', 'attr', 'name'), ('attr', 'name'), ('name',)):
        try:
            v = attr
            for f in path:
                v = v[f]
            return v.string()
        except (gdb.error, KeyError):
            continue
    return None


def _read_sections():
    mod  = gdb.parse_and_eval("mod")
    sect = mod['sect_attrs']
    n    = int(sect['nsections'])
    arr  = sect['attrs']

    out = {}
    for i in range(n):
        a    = arr[i]
        name = _section_name(a)
        if not name:
            continue
        try:
            out[name] = int(a['address'])
        except (gdb.error, KeyError):
            continue
    return out


def _find_return_line(path):
    call = re.compile(r'do_init_module\s*\(\s*mod\s*\)')
    ret  = re.compile(r'^\s*return\s+do_init_module\s*\(\s*mod\s*\)')
    fallback = None
    with open(path, 'r', errors='replace') as f:
        for i, line in enumerate(f, 1):
            if not call.search(line):
                continue
            if ret.search(line):
                return i
            if fallback is None:
                fallback = i
    return fallback


def _continue_blocking(timeout_s):
    """Run 'continue' and wait for the stop. Returns True if it timed out."""
    t = _Timeout(timeout_s)
    t.start()
    try:
        gdb.execute("continue")
    except KeyboardInterrupt:
        pass
    except gdb.error:
        pass
    finally:
        # gdb.execute("continue") from a command blocks until the stop; if this build somehow
        # returns early, wait the target out here rather than reading it while it runs. The
        # _Timeout still fires SIGINT to break a genuine hang.
        end = time.time() + timeout_s
        while _target_running() and time.time() < end:
            time.sleep(0.05)
        t.cancel()
    return t.fired


class MIDebugOnInit(gdb.MICommand):
    """
    -debug-on-init MODULE SERIAL SYMFILE SRCDIR CMD

    Drive gdb through debugging a kernel module from its insmod:
      - break where load_module() is about to call do_init_module(),
      - load CMD onto the target over SERIAL,
      - read the module's section load addresses from mod->sect_attrs,
      - add-symbol-file SYMFILE at those addresses and 'directory SRCDIR',
      - set a breakpoint at the module's init function and run to it.

    The target must already be stopped (Seer sends a SIGINT first).
    """

    def __init__(self):
        super(MIDebugOnInit, self).__init__("-debug-on-init")

    def invoke(self, argv):
        # Always emit one '@debug-on-init-complete <ok|error> <detail>' console line so Seer can
        # clear its "in progress" state and tell the user what happened, whether we return a
        # result or raise.
        try:
            result = self._run(argv)
            gdb.write("@debug-on-init-complete ok %s\n" % result.get("warnings", ""))
            gdb.flush()
            return result
        except gdb.GdbError as e:
            gdb.write("@debug-on-init-complete error %s\n" % str(e))
            gdb.flush()
            raise
        except Exception as e:
            gdb.write("@debug-on-init-complete error %s\n" % str(e))
            gdb.flush()
            raise gdb.GdbError("-debug-on-init: %s" % e)

    def _run(self, argv):
        if len(argv) < 5:
            raise gdb.GdbError("-debug-on-init: expects MODULE SERIAL SYMFILE SRCDIR CMD")

        module  = argv[0]
        serial  = argv[1]
        symfile = argv[2]
        srcdir  = argv[3]
        command = " ".join(argv[4:])

        try:
            open(LOGFILE, "w").close()   # fresh log per run
        except Exception:
            pass
        _log("start: module=%s serial=%s cmd=%r" % (module, serial, command))

        inf = gdb.selected_inferior()
        if not inf or not inf.threads():
            raise gdb.GdbError("-debug-on-init: no running inferior")

        # The target must be stopped. Seer SIGINTs it before sending this command, so normally we're
        # already stopped here; interrupt as a fallback. A Python MI command can't wait for a stop
        # itself (gdb's event loop doesn't run inside invoke()), so if it's still running after
        # this, bail and let the user retry.
        if not _really_stopped():
            try:
                gdb.execute("interrupt")
            except gdb.error:
                pass
            for _ in range(50):
                if _really_stopped():
                    break
                time.sleep(0.1)
        if not _really_stopped():
            raise gdb.GdbError("-debug-on-init: target is still running - stop it first, then retry")

        # NOTE: do NOT 'set breakpoint always-inserted on' here. On this OpenOCD SMP target it makes
        # gdb insert the breakpoint once and never refresh it, and OpenOCD then loses it - the
        # breakpoint is silently never hit even though the code runs through it. The default
        # ('auto': remove on stop, freshly re-insert on each resume) is what actually works.

        saved_bps   = [(bp, bp.enabled) for bp in (gdb.breakpoints() or [])]
        created_bps = []
        warnings    = []

        def restore_settings():
            pass

        def restore_user_bps():
            for bp, enabled in saved_bps:
                try:
                    bp.enabled = enabled
                except Exception:
                    pass

        def delete_created_bps():
            for bp in created_bps:
                try:
                    bp.delete()
                except Exception:
                    pass
            created_bps.clear()

        def bail(msg):
            # Leave the target stopped where it is - the user can see the failure point and
            # continue manually.
            delete_created_bps()
            restore_user_bps()
            restore_settings()
            raise gdb.GdbError(msg)

        try:
            if _module_is_loaded(module):
                bail("module '%s' is already loaded - unload it first before debug on init" % module)

            for bp, _ in saved_bps:
                try:
                    bp.enabled = False
                except Exception:
                    pass

            # --- Resolve load_module()'s source file -------------------------------
            lm_file = None
            try:
                sals = gdb.decode_line("load_module")[1]
                if sals and sals[0].symtab is not None:
                    lm_file = sals[0].symtab.fullname()
            except gdb.error:
                pass
            if lm_file is None:
                sym = (gdb.lookup_global_symbol("load_module")
                       or gdb.lookup_static_symbol("load_module"))
                if sym is not None and sym.symtab is not None:
                    lm_file = sym.symtab.fullname()
            if lm_file is None:
                bail("could not resolve load_module() - are the kernel's debug symbols loaded?")

            line = _find_return_line(lm_file)
            if not line:
                bail("could not find 'return do_init_module(mod)' in %s" % lm_file)

            # --- Break at that line, then load the module over serial --------------
            # Permanent hardware breakpoint ('hbreak', not 'thbreak') - that is what hits reliably
            # when done by hand on this target; a temporary HW breakpoint gets missed. We delete it
            # ourselves once it's hit. (Project rule: hardware breakpoints only.)
            spec = "%s:%d" % (lm_file, line)
            gdb.execute("hbreak " + spec)
            created_bps.append(gdb.breakpoints()[-1])
            _diag("armed hw breakpoint at %s" % spec)

            data = (command + "\n").encode()

            def _send():
                time.sleep(SERIAL_DELAY_S)
                try:
                    fd = os.open(serial, os.O_WRONLY | os.O_NONBLOCK | os.O_NOCTTY)
                except OSError as e:
                    _log("serial: cannot open %s: %s" % (serial, e))
                    return
                try:
                    buf = data
                    deadline = time.time() + 5.0
                    while buf and time.time() < deadline:
                        try:
                            buf = buf[os.write(fd, buf):]
                        except BlockingIOError:
                            time.sleep(0.05)
                    _log("serial: wrote %r to %s (%d bytes unsent)" % (data, serial, len(buf)))
                finally:
                    os.close(fd)

            threading.Thread(target=_send, daemon=True).start()

            try:
                _log("breakpoints before continue:\n" + gdb.execute("info breakpoints", to_string=True))
            except Exception:
                pass

            _diag("continuing, waiting for %s" % spec)
            timed_out = _continue_blocking(TIMEOUT_S)

            where = "?"
            try:
                where = gdb.selected_frame().name() or "?"
            except Exception:
                pass
            _diag("stopped in %s (timed_out=%s)" % (where, timed_out))

            # Forensics: did the breakpoints ever get inserted / hit?
            for cmd in ("info breakpoints", "p/x $pc", "bt 3"):
                try:
                    _log("%s ->\n%s" % (cmd, gdb.execute(cmd, to_string=True)))
                except Exception as e:
                    _log("%s -> <%s>" % (cmd, e))

            if where != "load_module":
                if timed_out:
                    bail("timed out after %ds - the breakpoint at %s was never hit even though the "
                         "module loaded (stopped in %s). The target is dropping this breakpoint. Try "
                         "pinning the loader to the debugged core: 'taskset -c 0 %s'."
                         % (int(TIMEOUT_S), spec, where, command))
                bail("stopped in %s, not load_module(), while waiting for the module load" % where)

            # temporary breakpoints delete themselves on the hit; delete whatever's left.
            for b in list(created_bps):
                try:
                    if b.is_valid():
                        b.delete()
                except Exception:
                    pass
            created_bps.clear()

            # --- Read the module's section load addresses -------------------------
            try:
                sections = _read_sections()
            except gdb.error as e:
                sections = {}
                warnings.append("sect_attrs: %s" % e)

            if ".text" not in sections:
                bail("could not read mod->sect_attrs section addresses")
            _diag("read %d sections (.text = 0x%x)" % (len(sections), sections[".text"]))

            # --- Load the module's symbols at those addresses --------------------
            parts = ["add-symbol-file", symfile, "0x%x" % sections[".text"]]
            for name, addr in sorted(sections.items()):
                if name == ".text" or name in SKIP_SECTIONS:
                    continue
                parts += ["-s", name, "0x%x" % addr]
            try:
                gdb.execute(" ".join(parts), to_string=True)
            except gdb.error as e:
                warnings.append("add-symbol-file: %s" % e)

            try:
                gdb.execute("directory " + srcdir, to_string=True)
            except gdb.error as e:
                warnings.append("directory: %s" % e)

            # --- Breakpoint at the module's init function -----------------------
            init_specs = ["init_module"]
            if ".init.text" in sections:
                init_specs.append("*0x%x" % sections[".init.text"])

            init_bp = None
            for spec in init_specs:
                try:
                    gdb.execute("hbreak " + spec)      # permanent hardware breakpoint
                    init_bp = gdb.breakpoints()[-1]
                    break
                except gdb.error:
                    continue
            if init_bp is None:
                warnings.append("could not set a breakpoint at the module's init function")
            else:
                _diag("init breakpoint set (%s)" % init_bp.location)

            # --- Restore the user's breakpoints and run to the init breakpoint --
            restore_user_bps()

            init_timed_out = False
            if init_bp is not None:
                _diag("continuing to module init")
                init_timed_out = _continue_blocking(TIMEOUT_S)
                try:
                    _log("after init continue:\n" + gdb.execute("info breakpoints", to_string=True))
                except Exception:
                    pass
                # Delete our init breakpoint - the user takes over from here.
                try:
                    if init_bp.is_valid():
                        init_bp.delete()
                except Exception:
                    pass

            if init_timed_out:
                warnings.append("module init breakpoint was not hit within %ds" % int(TIMEOUT_S))

            result = {
                "result":     "done",
                "module":     module,
                "sourcefile": lm_file,
                "line":       str(line),
                "sections":   str(len(sections)),
            }
            if warnings:
                result["warnings"] = "; ".join(warnings)
            return result

        except gdb.GdbError:
            raise
        except Exception as e:
            try:
                delete_created_bps()
                restore_user_bps()
                restore_settings()
            except Exception:
                pass
            raise gdb.GdbError("-debug-on-init: %s" % e)


MIDebugOnInit()

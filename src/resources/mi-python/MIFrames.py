# SPDX-FileCopyrightText: 2025 Ernie Pasveer <epasveer@att.net>
#
# SPDX-License-Identifier: MIT

import gdb

#
# Python MI command to add a -frames commands.
#

class MIFrames(gdb.MICommand):
    """
    Add a command to list all frames for all threads.

    Usage: -frames-list [OPTIONS]

    Options:
      --max-frames N          Maximum frames per thread
      --thread-id ID          Only show specific thread
      --include-args          Include function arguments
      --include-locals        Include local variables
      --simple-values         Only simple types
      --no-values             Don't include variable values
      --frame-filter PATTERN  Only frames matching pattern
    """

    def __init__(self, name, mode):
        self._mode = mode
        super(MIFrames, self).__init__(name);

    def invoke(self, argv):
        if self._mode == "list":

            # Parse options
            options = {
                'max_frames': None,
                'thread_id': None,
                'include_args': False,
                'include_locals': False,
                'simple_values': False,
                'no_values': False,
                'frame_filter': None
            }

            i = 0
            while i < len(argv):
                if argv[i] == "--max-frames" and i + 1 < len(argv):
                    options['max_frames'] = int(argv[i + 1])
                    i += 2
                elif argv[i] == "--thread-id" and i + 1 < len(argv):
                    options['thread_id'] = int(argv[i + 1])
                    i += 2
                elif argv[i] == "--include-args":
                    options['include_args'] = True
                    i += 1
                elif argv[i] == "--include-locals":
                    options['include_locals'] = True
                    i += 1
                elif argv[i] == "--simple-values":
                    options['simple_values'] = True
                    i += 1
                elif argv[i] == "--no-values":
                    options['no_values'] = True
                    i += 1
                elif argv[i] == "--frame-filter" and i + 1 < len(argv):
                    options['frame_filter'] = argv[i + 1]
                    i += 2
                else:
                    raise gdb.GdbError(f"Unknown option: {argv[i]}")

            # Save current thread and frame
            current_thread = gdb.selected_thread()
            current_frame_level = gdb.selected_frame().level() if current_thread else None

            try:
                result = {
                    'frames': [],
                    'current_thread_id': current_thread.num if current_thread else None,
                    'current_frame_level': current_frame_level,
                    'total_threads': 0,
                    'total_frames': 0
                }

                # Get all threads
                all_threads = gdb.selected_inferior().threads()

                # Filter by thread ID if specified
                if options['thread_id']:
                    all_threads = [t for t in all_threads if t.num == options['thread_id']]
                    if not all_threads:
                        raise gdb.GdbError(f"Thread {options['thread_id']} not found")

                for thread in all_threads:
                    thread_data = self._process_thread(thread, current_thread, options)
                    if thread_data['frames']:  # Only include if we got frames
                        result['frames'].append(thread_data)
                        result['total_frames'] += len(thread_data['frames'])

                result['total_threads'] = len(result['frames'])

                return result

            finally:
                # Restore original thread and frame
                if current_thread:
                    current_thread.switch()
                    if current_frame_level is not None:
                        try:
                            frame = gdb.newest_frame()
                            for _ in range(current_frame_level):
                                frame = frame.older()
                            frame.select()
                        except:
                            pass
        else:
            raise gdb.GdbError("frames: Invalid parameter: %s" % self._mode)

    def _process_thread(self, thread, current_thread, options):
        """Process a single thread."""
        thread_data = {
            'thread-id': thread.num,
            'target-id': str(thread.ptid),
            'name': thread.name or '',
            'state': 'running' if thread.is_running() else 'stopped',
            'current': thread == current_thread,
            'frames': []
        }

        if thread.is_running():
            thread_data['state_detail'] = 'Thread is running, no frames available'
            return thread_data

        try:
            # Switch to thread
            thread.switch()

            # Walk the stack
            frame = gdb.newest_frame()
            frame_count = 0

            while frame is not None:
                # Check max frames limit
                if options['max_frames'] and frame_count >= options['max_frames']:
                    thread_data['frames_truncated'] = True
                    break

                # Get frame info
                frame_info = self._extract_frame_info(frame, options)

                # Apply frame filter
                if options['frame_filter']:
                    func_name = frame_info.get('func', '')
                    if options['frame_filter'] not in func_name:
                        frame = frame.older()
                        continue

                thread_data['frames'].append(frame_info)
                frame = frame.older()
                frame_count += 1

        except Exception as e:
            thread_data['error'] = str(e)

        return thread_data

    def _extract_frame_info(self, frame, options):
        """Extract detailed frame information."""
        info = {
            'level': frame.level(),
            'addr': hex(frame.pc()),
            'func': frame.name() or '??',
        }

        # Architecture info
        try:
            info['arch'] = frame.architecture().name()
        except:
            pass

        # Source location
        try:
            sal = frame.find_sal()
            if sal.symtab:
                info['file'] = sal.symtab.filename
                info['fullname'] = sal.symtab.fullname()
                info['line'] = sal.line
            else:
                info['from'] = gdb.solib_name(frame.pc()) or 'unknown'
        except:
            pass

        # Frame type
        try:
            frame_type = frame.type()
            type_names = {
                gdb.NORMAL_FRAME: 'normal',
                gdb.DUMMY_FRAME: 'dummy',
                gdb.INLINE_FRAME: 'inline',
                gdb.TAILCALL_FRAME: 'tailcall',
                gdb.SIGTRAMP_FRAME: 'sigtramp',
                gdb.ARCH_FRAME: 'arch',
                gdb.SENTINEL_FRAME: 'sentinel'
            }
            info['type'] = type_names.get(frame_type, 'unknown')
        except:
            pass

        # Variables
        if options['include_args'] or options['include_locals']:
            try:
                block = frame.block()
                if block:
                    if options['include_args']:
                        info['args'] = self._get_block_variables(
                            block, frame, True, options
                        )
                    if options['include_locals']:
                        info['locals'] = self._get_block_variables(
                            block, frame, False, options
                        )
            except:
                pass

        return info

    def _get_block_variables(self, block, frame, get_args, options):
        """Get variables from a block."""
        variables = []

        while block:
            for symbol in block:
                if get_args and not symbol.is_argument:
                    continue
                if not get_args and (symbol.is_argument or not symbol.is_variable):
                    continue

                var_info = {'name': symbol.name}

                try:
                    value = symbol.value(frame)
                    var_type = value.type

                    # Type filtering
                    if options['simple_values'] and not self._is_simple_type(var_type):
                        continue

                    var_info['type'] = str(var_type)

                    # Value
                    if not options['no_values']:
                        try:
                            var_info['value'] = str(value)
                            # Add address for pointers
                            if var_type.code == gdb.TYPE_CODE_PTR:
                                var_info['address'] = hex(int(value))
                        except:
                            var_info['value'] = '<unavailable>'

                except Exception as e:
                    var_info['error'] = str(e)

                variables.append(var_info)

            if block.function:
                break
            block = block.superblock

        return variables

    def _is_simple_type(self, gdb_type):
        """Check if type is scalar."""
        return gdb_type.code in [
            gdb.TYPE_CODE_INT, gdb.TYPE_CODE_FLT,
            gdb.TYPE_CODE_BOOL, gdb.TYPE_CODE_CHAR,
            gdb.TYPE_CODE_ENUM, gdb.TYPE_CODE_PTR
        ]

# Register
MIFrames("-frames-list", "list")


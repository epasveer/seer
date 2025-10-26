# File: lsmod.py

import gdb

class MIlsmod(gdb.MICommand):
    def __init__(self):
        self._mode = "default"  # Placeholder mode
        super(MIlsmod, self).__init__("-lsmod")  # MI command name

    def invoke(self, argv):
        try:
            # Get filter string from argv (if provided)
            filter_str = argv[0] if argv else None

            # Try to access 'modules' symbol
            modules_symbol = gdb.lookup_global_symbol("modules")
            if not modules_symbol:
                try:
                    modules = gdb.parse_and_eval("modules")
                    if not modules:
                        return {
                            "result": "error",
                            "message": "Symbol 'modules' not found via parse_and_eval. Ensure kernel debugging symbols are loaded."
                        }
                except gdb.error:
                    return {
                        "result": "error",
                        "message": "Symbol 'modules' not found. Run 'symbol-file vmlinux' and ensure CONFIG_DEBUG_INFO=y, CONFIG_MODULES=y."
                    }
            else:
                modules = modules_symbol.value()

            # Diagnostic: Print address of modules
            gdb.write(f"Debug: Address of 'modules' = {modules.address}\n")

            # Get the type of 'struct module'
            try:
                module_type = gdb.lookup_type("struct module")
            except gdb.error:
                return {
                    "result": "error",
                    "message": "Type 'struct module' not found. Ensure kernel debugging symbols are available."
                }

            # Calculate offset of 'list' field
            try:
                offset = module_type['list'].bitpos // 8
            except gdb.error:
                return {
                    "result": "error",
                    "message": "Field 'list' not found in 'struct module'. Check kernel version."
                }

            # Start at modules.next
            current = modules['next']
            modules_list = []

            # Loop until we reach the head of the list
            while current != modules.address:
                # Compute the module pointer
                mod_ptr = current.cast(gdb.lookup_type("void").pointer()) - offset
                mod = mod_ptr.cast(module_type.pointer()).dereference()

                # Get the module name
                try:
                    mod_name = mod['name'].string()
                except gdb.error:
                    return {
                        "result": "error",
                        "message": "Failed to read module name. Possible corrupted module data."
                    }

                # Break if name is empty
                if mod_name == "":
                    break

                # Add module to list if it matches filter exactly (case-sensitive) or no filter
                if filter_str is None or filter_str == mod_name:
                    modules_list.append(f'{{name="{mod_name}"}}')

                # Move to next entry
                current = current['next']

            # Format MI output
            modules_str = f"[{', '.join(modules_list)}]" if modules_list else "[]"
            return {"result": "done", "modules": modules_str}

        except gdb.error as e:
            return {"result": "error", "message": f"GDB error: {str(e)}"}
        except Exception as e:
            return {"result": "error", "message": f"Unexpected error: {str(e)}"}

MIlsmod()
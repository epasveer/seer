import gdb

# The PrettyPrinter object.
class LocationPrinter:

    # Save a copy of the Location.
    def __init__(self, val):
        self.__val = val

    # Convert it to a string. gdb will call this function.
    def to_string(self):
        ret = ""

        # Construct a python string with the values of the Location struct.
        # It's good to handle any errors. Hence the try/catch.
        try:
            ret = "Location=(" + str(self.__val['city']) + "," + str(self.__val['state']) + "," + str(self.__val['zip']) + "," + str(self.__val['cell']) + ")"

        except Exception as e:
            # Any exception will be saved in the return string.
            ret = str(e)

        return ret

    # A hint for gdb on the return type.
    def display_hint (self):
        return 'string'


# Specify a function to detect a Location struct type and will call the PrettyPrinter object for it.
#
# gdb maintains a large list of PrettyPrinters for lots of data/class types. It will go through the
# list looking for one that matches the type. If one is found, it uses it. Otherwise, gdb will
# default to the regular way of printing data types.
def LocationPrinter_func(val):
    if str(val.type) == 'Location':
        return LocationPrinter(val)

# Add the function to gdb's list of PrettyPrinter functions.
gdb.pretty_printers.append(LocationPrinter_func)



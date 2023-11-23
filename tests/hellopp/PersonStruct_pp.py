import gdb

# The PrettyPrinter object.
class PersonPrinter:

    # Save a copy of the Person.
    def __init__(self, val):
        self.__val = val

    # Convert it to a string. gdb will call this function.
    def to_string(self):
        ret = ""

        # Construct a python string with the values of the Person struct.
        # It's good to handle any errors. Hence the try/catch.
        try:
            ret = "Person=(" + str(self.__val['name']) + "," + str(self.__val['age']) + "," + str(self.__val['salary']) + "," + str(self.__val['location']) + ")"

        except Exception as e:
            # Any exception will be saved in the return string.
            ret = str(e)

        return ret

    # A hint for gdb on the return type.
    def display_hint (self):
        return 'string'


# Specify a function to detect a Person struct type and will call the PrettyPrinter object for it.
#
# gdb maintains a large list of PrettyPrinters for lots of data/class types. It will go through the
# list looking for one that matches the type. If one is found, it uses it. Otherwise, gdb will
# default to the regular way of printing data types.
def PersonPrinter_func(val):
    if str(val.type) == 'Person':
        return PersonPrinter(val)

# Add the function to gdb's list of PrettyPrinter functions.
gdb.pretty_printers.append(PersonPrinter_func)



# SPDX-FileCopyrightText: 2026 Ernie Pasveer <epasveer@att.net>
#
# SPDX-License-Identifier: MIT

#
# Python MI command to evaluate an expression with a print format.
#
# Gdb's -data-evaluate-expression has no format option. This command is like
# 'output/FMT EXPRESSION', but walks the value itself so that some types stay
# readable no matter which format is asked for:
#
#   Pointers               Always natural. A hex address, plus the text for 'char*'.
#   char arrays            Always natural. Printed as a string.
#   Structs/unions/arrays  Formatted member by member.
#   Pretty-printed values  Handed to gdb's own formatter (std::string, std::vector, ...).
#   Everything else        Uses the requested format. Floats/doubles show their raw bits.
#                          With 'f', integers have their bits read as a float of the same size.
#
# For the 'd' format, unsigned integers are printed with 'u' so large values
# don't come out negative.
#
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/Output-Formats.html#Output-Formats
#   https://sourceware.org/gdb/current/onlinedocs/gdb.html/GDB_002fMI-Commands-In-Python.html#GDB_002fMI-Commands-In-Python
#

class MIEvaluateFormat(gdb.MICommand):
    """
    Evaluate an expression using a print format.

    -data-evaluate-expression-format FMT EXPRESSION

        FMT is one of:  x (hex), o (octal), d (decimal), t (binary), f (float)

    Returns the same result as -data-evaluate-expression:

        ^done,value="..."
    """

    _formats = ("x", "o", "d", "t", "f")

    def __init__(self, name):
        super(MIEvaluateFormat, self).__init__(name)

    def invoke(self, argv):

        if len(argv) < 2:
            raise gdb.GdbError("-data-evaluate-expression-format: Usage: FMT EXPRESSION")

        fmt        = argv[0]
        expression = " ".join(argv[1:])

        if fmt not in self._formats:
            raise gdb.GdbError("-data-evaluate-expression-format: Invalid format '%s'. Use one of: %s" % (fmt, " ".join(self._formats)))

        try:
            value = gdb.parse_and_eval(expression)

            return { "value": self._format(value, fmt) }

        except gdb.error as e:
            # Give back gdb's plain message, without the "Error occurred in Python:" prefix.
            raise gdb.GdbError(str(e))

    def _elementLimit(self):

        # Honor gdb's 'print elements' setting. None or 0 means unlimited.
        try:
            limit = gdb.parameter("print elements")
        except Exception:
            limit = 200

        if limit is None or limit <= 0:
            return None

        return limit

    def _isCharType(self, t):

        t = t.strip_typedefs()

        if t.code == gdb.TYPE_CODE_CHAR:
            return True

        if t.code == gdb.TYPE_CODE_INT and t.sizeof == 1:
            return True

        return False

    def _format(self, value, fmt):

        if value.is_optimized_out:
            return "<optimized out>"

        t = value.type.strip_typedefs()

        # Pointers (data, function, member) always print naturally.
        if t.code in (gdb.TYPE_CODE_PTR, gdb.TYPE_CODE_MEMBERPTR, gdb.TYPE_CODE_METHODPTR):
            return value.format_string()

        # References. Print like gdb does: "@0x1234: value".
        if t.code in (gdb.TYPE_CODE_REF, gdb.TYPE_CODE_RVALUE_REF):
            target = value.referenced_value()
            return "@" + target.address.format_string() + ": " + self._format(target, fmt)

        # Values with a pretty printer. Let gdb format them.
        if gdb.default_visualizer(value) is not None:
            return value.format_string(format=fmt)

        # Arrays.
        if t.code == gdb.TYPE_CODE_ARRAY:

            # Character arrays print as a string.
            if self._isCharType(t.target()):
                return value.format_string()

            low, high = t.range()
            count     = high - low + 1
            limit     = self._elementLimit()
            shown     = count if limit is None else min(count, limit)

            elements = []
            for i in range(low, low + shown):
                elements.append(self._formatChild(lambda: value[i], fmt))

            if shown < count:
                elements.append("...")

            return "{" + ", ".join(elements) + "}"

        # Structures, classes, and unions.
        if t.code in (gdb.TYPE_CODE_STRUCT, gdb.TYPE_CODE_UNION):

            members = []

            for field in t.fields():

                if field.artificial and not field.is_base_class:
                    # Compiler generated fields, other than base classes.
                    # Keep vtable pointers; gdb shows them too.
                    if field.name is None or not field.name.startswith("_vptr"):
                        continue

                if field.is_base_class:
                    text = self._formatChild(lambda: value.cast(field.type), fmt)
                    members.append("<" + str(field.type) + "> = " + text)
                    continue

                if not hasattr(field, "bitpos"):
                    # Static member.
                    text = self._formatChild(lambda: value[field.name], fmt)
                    members.append("static " + field.name + " = " + text)
                    continue

                text = self._formatChild(lambda: value[field], fmt)

                if field.name:
                    members.append(field.name + " = " + text)
                else:
                    # Anonymous struct/union member.
                    members.append(text)

            return "{" + ", ".join(members) + "}"

        # Scalars: integers, chars, bools, enums, floats.
        if fmt == "d" and t.code in (gdb.TYPE_CODE_INT, gdb.TYPE_CODE_CHAR):
            try:
                if not t.is_signed:
                    fmt = "u"
            except (AttributeError, ValueError):
                # Type.is_signed needs gdb 12+.
                pass

        return value.format_string(format=fmt)

    def _formatChild(self, getter, fmt):

        # Format one member/element. Don't let one bad member spoil the whole value.
        try:
            return self._format(getter(), fmt)
        except gdb.error as e:
            return "<error: " + str(e) + ">"

MIEvaluateFormat("-data-evaluate-expression-format")

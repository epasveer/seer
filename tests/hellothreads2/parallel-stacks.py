import sys
import subprocess
import re
import argparse
from graphviz import Digraph
import html


class Frame:
    def __init__(self, level, address, function, parameters, filename):
        self.level = level
        self.address = address
        self.function = function
        self.parameters = parameters
        self.filename = filename

    def __str__(self):
        return "level: {}, address: '{}', function: '{}', parameters: '{}', filename: '{}'".format(
            self.level,
            self.address,
            self.function,
            self.parameters,
            self.filename,
        )


class Thread:
    def __init__(self, thread_id):
        self.id = thread_id
        self.frames = []

    def __str__(self):
        result = 'Thread {}'.format(self.id)
        for frame in self.frames:
            result += '\n  ' + str(frame)
        return result


def parse_gdb_output(gdb_output):
    threads = []
    current_thread = None

    for gdb_output_line in gdb_output:
        if gdb_output_line.startswith('Thread '):
            thread_info = gdb_output_line.split()
            thread_id = thread_info[1]
            current_thread = Thread(thread_id)
            threads.append(current_thread)
            continue
        elif current_thread:
            if len(gdb_output_line) == 0:
                current_thread = None
                continue
            match_obj = re.match('#(?P<level>\\d+) +((?P<address>0x[\\da-f]+) in |)(?P<function>.+) \\((?P<parameters>.*)\\)( (at|from) (?P<filename>.+)|)', gdb_output_line)
            if match_obj:
                frame = Frame(
                    level=int(match_obj.group('level')),
                    address=match_obj.group('address'),
                    function=match_obj.group('function'),
                    parameters=match_obj.group('parameters'),
                    filename=match_obj.group('filename')
                )
                current_thread.frames.append(frame)
            else:
                break
                #raise SyntaxError("gdb output: '{}'".format(gdb_output_line))

    return threads


class Node:
    def __init__(self):
        self.function = None
        self.nodes = []
        self.depth = 0
        self.threads = []


def get_parallel_stacks(threads, current_function=None, depth=0):
    node = Node()
    node.depth = depth
    node.function = current_function
    node.threads = threads
    function_threads = {}
    level = -depth - 1
    for thread in threads:
        if depth >= len(thread.frames):
            continue
        function = thread.frames[level].function
        threads = function_threads.get(function, None)
        if threads:
            threads.append(thread)
        else:
            function_threads[function] = [thread]

    for function in function_threads:
        child_node = get_parallel_stacks(function_threads[function], function, depth + 1)
        node.nodes.append(child_node)
    return node


def print_parallel_stack(node, first_node=True, indent='', last_node=False):
    base_indent = '   '
    thread_indent = ''
    before_thread_indent = ''
    if indent != '':
        before_thread_indent = indent + '│'
        thread_indent = indent + ('╰' if last_node else '├')
        function_indent = indent + ('' if last_node else '│') + base_indent
    if first_node:
        thread_count = len(node.threads)
        print(before_thread_indent)
        print('{}Threads: {}'.format(thread_indent, thread_count))
    if node.function:
        print('{}{}'.format(function_indent, node.function))
    if len(node.nodes) == 1:
        print_parallel_stack(node.nodes[0], False, indent, last_node)
        return
    last_i = len(node.nodes) - 1
    if indent == '' or last_node:
        indent = base_indent
    else:
        indent += '│' + base_indent
    for i in range(len(node.nodes)):
        print_parallel_stack(node.nodes[i], True, indent, i == last_i)


class Stack:
    def __init__(self):
        self.functions = []
        self.stacks = []
        self.thread_count = []


def fill_stack(node, stack):
    stack.thread_count = len(node.threads)
    if node.function:
        stack.functions.append(node.function)
    if len(node.nodes) == 1:
        fill_stack(node.nodes[0], stack)
        return
    for child_node in node.nodes:
        child_stack = Stack()
        fill_stack(child_node, child_stack)
        stack.stacks.append(child_stack)
    assert len(stack.stacks) == 0 or len(stack.stacks) > 1


def add_stack_to_graph(dot, stack, node_id=0, parent_node_name=None):
    rows = ''
    node_name = None
    if len(stack.functions):
        row_template = '<tr><td align="{}">{}</td></tr>'
        rows += row_template.format('right', '<b>{} Threads</b>'.format(stack.thread_count))
        for function in reversed(stack.functions):
            rows += row_template.format('left', '<font color="darkgreen">{}</font>'.format(html.escape(function)))
        table = '<table BORDER="0" CELLBORDER="1" CELLSPACING="0">{}</table>'.format(rows)
        node_name = ''
        if parent_node_name:
            node_name += parent_node_name + '_'
        node_name += str(node_id)
        dot.node(node_name, '<{}>'.format(table))
        if parent_node_name:
            dot.edge(parent_node_name, node_name)
    child_node_id = 0
    for child_stack in stack.stacks:
        add_stack_to_graph(dot, child_stack, child_node_id, node_name)
        child_node_id += 1


def get_graph(node):
    dot = Digraph(node_attr={'shape': 'plaintext'})
    dot.graph_attr['rankdir'] = 'BT'
    stack = Stack()
    fill_stack(node, stack)
    add_stack_to_graph(dot, stack)
    return dot


def main():
    arg_parser = argparse.ArgumentParser(description='Shows Parallel Stacks of a Process',
                                         epilog='Data sources are a process, a log file or standard input.')
    data_source_group = arg_parser.add_mutually_exclusive_group()
    data_source_group.add_argument('-l', '--log_file', metavar='filename', help='a GDB log file with backtraces',
                                   type=argparse.FileType('r'), default=sys.stdin)
    data_source_group.add_argument('-p', '--pid', type=int, required=False, help='a process ID')
    args = arg_parser.parse_args()

    gdb_output = []
    process_id = args.pid
    input_file = args.log_file
    if process_id:
        gdb_command = ['gdb', '--batch', '-ex', 'thread apply all bt', '-pid', str(process_id)]
        gdb_result = subprocess.run(gdb_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        gdb_output = gdb_result.stdout.decode('utf-8').splitlines()
    elif input_file:
        gdb_output = input_file.read().splitlines()
    threads = parse_gdb_output(gdb_output)
    for thread in threads:
        print(thread)

    tree = get_parallel_stacks(threads)
    print_parallel_stack(tree)
    get_graph(tree).render(view=True)
    #print(get_graph(tree).source)


if __name__ == "__main__":
    main()

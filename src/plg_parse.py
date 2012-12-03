#! /usr/bin/env python

import networkx as nx
import logging

class Literal :
    name = ''
    rules = 0
    references = 0

    def __init__ (self, name) :
        self.name = name

    def __str__ (self) :
        return self.name

    def __eq__ (self, other) :
        return self.name == other.name

def is_empty(line) :
    return line.isspace() or line == ''

def is_state_start (line) :
    line = line.split()
    return len(line) == 2 and line[0] == 'state' and line[1].isdigit()

def parse (filename) :
    graph = nx.DiGraph()
    lines = open(filename).readlines()
    i = 0

    # Skip lines until we get to the terminal list
    while not lines[i].startswith('Terminals, with rules where they appear') :
        i += 1

    # Skip the delimiting line
    i += 1

    # Parse terminals
    terminals = []
    while not lines[i].startswith('Nonterminals, with rules where they appear') :
        if is_empty(lines[i]) :
            i += 1
            continue

        # parse the initial line "TERM (id) ref ref..."
        line = lines[i].split()
        term = Literal(line[0])
        term.references = len(line[2:])

        # parse trailing lines of reference numbers "    ref ref ref..."
        i += 1
        while lines[i].startswith('    ') :
            term.references += len(lines[i].split())
            i += 1

        terminals.append(term)

    # Skip the delimiting line
    i += 1


    # Parse nonterminals
    nonterminals = []
    while not is_state_start(lines[i]) :
        if is_empty(lines[i]) :
            i += 1
            continue

        # Parse the initial line "nt_name (id)"
        line = lines[i].split()
        nt = Literal(line[0])
        i += 1

        # Parse subsequent lines "on left: ref ref...ref, on right: ref ref..."
        tail = ''
        while(lines[i].startswith('    ')) :
            tail += lines[i]
            i += 1

        if nt.name == '$accept' :
            continue

        tail = tail.split('on right:')
        rules = tail[0].strip('    on left: ').split()
        nt.rules = len(rules)
        refs = tail[1].split()
        nt.references = len(refs)

        nonterminals.append(nt)

    for i in range(i, len(lines)) :
        line = lines[i]
        if not is_state_start(line) :
            continue

        state_lines = []
        state_lines.append(line)
        for i in range(i+1, len(lines)) :
            line = lines[i]
            if len(line) == 0 or line.isspace() :
                continue
            elif is_state_start(line) :
                break
            else :
                state_lines.append(line)

        read_state(graph, state_lines)

    return graph, terminals, nonterminals

def read_state (graph, state_lines) :
    state_id = int(state_lines[0].split()[1])
    graph.add_node(state_id)

    log.debug('Parsing state %s: %s lines' %(state_id, len(state_lines)))

    for line in state_lines :
        if 'shift, and go to state' in line :
            to_state = int(line.split()[-1])
            graph.add_edge(state_id, to_state, edge_type='shift')
            log.debug('shift edge from %s to %s' %(state_id, to_state))

        elif 'go to state' in line :
            to_state = int(line.split()[-1])
            graph.add_edge(state_id, to_state, edge_type='goto')
            log.debug('goto  edge from %s to %s' %(state_id, to_state))

        # TODO: Track reduction? Does this matter?

    if graph.out_degree(state_id) == 0 :
        log.debug('state %s is a leaf!' %(state_id))

def measure (filename) :
    import table_printer as tp
    from collections import namedtuple
    G, terminals, nonterminals = parse(filename)

    """
    print '%s terminals, sorted by ref count:' %len(terminals)
    rows = []
    Row = namedtuple('Row', ['name', 'referenced'])
    for tok in terminals :
        rows.append(Row("'%s'" %tok.name, tok.references))
    rows.sort(key=lambda x: -x[1])
    tp.print_table(rows)

    print '\n%d nonterminals, sorted by num rules:' %len(nonterminals)
    rows = []
    Row = namedtuple('Row', ['name', 'rules', 'referenced'])
    for nt in nonterminals :
        rows.append(Row(nt.name, nt.rules, nt.references))
    rows.sort(key=lambda x: -x[1])
    tp.print_table(rows)
    """

    num_rules = 0
    for nt in nonterminals :
        num_rules += nt.rules

    print '\n%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s' %(
            filename,
            len(terminals),
            len(nonterminals),
            num_rules,
            G.number_of_nodes(),
            G.number_of_edges(),
            G.number_of_edges() * 1.0 / G.number_of_nodes(),
            nx.average_clustering(G.to_undirected()),
            nx.average_shortest_path_length(G),
            nx.degree_assortativity_coefficient(G))
    return G


def init_log () :
    global log
    logging.basicConfig(format='%(levelname)s:%(message)s', level=logging.DEBUG)
    log = logging.getLogger()

def set_log_level(level) :
    log.setLevel(level)


init_log()
if __name__ == '__main__' :
    import sys
    if len(sys.argv) < 2 :
        print 'Usage: plg_parse <path/to/file.plg>'

    set_log_level(logging.WARNING)
    graph = measure(sys.argv[1])






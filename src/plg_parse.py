#! /bin/env python

import networkx as nx
import logging

def is_state_start (line) :
    line = line.split()
    return len(line) == 2 and line[0] == 'state' and line[1].isdigit()

def parse (filename) :
    graph = nx.DiGraph()
    lines = open(filename).readlines()
    for i in range(0, len(lines)) :
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

        parse_state(graph, state_lines)

    return graph

def parse_state (graph, state_lines) :
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
    G = parse(filename)
    print 'Graph info for plg file\t%s' %(filename)
    print 'Number of nodes\t%s' %(G.number_of_nodes())
    print 'Number of edges\t%s' %(G.number_of_edges())
    print 'Edge density\t%s' %(G.number_of_edges() / G.number_of_nodes())
    print 'Average clustering\t%s' %(nx.average_clustering(G.to_undirected()))
    print 'Average shortest path length\t%s' %(nx.average_shortest_path_length(G))
    print 'Degree correlation coef\t%s' %(nx.degree_assortativity_coefficient(G))
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






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

    logging.debug('Parsing state %s: %s lines' %(state_id, len(state_lines)))

    for line in state_lines :
        if 'shift, and go to state' in line :
            to_state = int(line.split()[-1])
            graph.add_edge(state_id, to_state, edge_type='shift')
            logging.debug('shift edge from %s to %s' %(state_id, to_state))

        elif 'go to state' in line :
            to_state = int(line.split()[-1])
            graph.add_edge(state_id, to_state, edge_type='goto')
            logging.debug('goto  edge from %s to %s' %(state_id, to_state))

        # TODO: Track reduction? Does this matter?

    if graph.out_degree(state_id) == 0 :
        logging.debug('state %s is a leaf!' %(state_id))



def init_log () :
    logging.basicConfig(format='%(levelname)s:%(message)s', level=logging.DEBUG)


init_log()





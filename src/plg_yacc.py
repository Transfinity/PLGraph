import re
PATTERN = re.compile(r'''((?:[^\s"']|"[^"]*"|'[^']*')+)''')

def parse_tokens (line) :
    if line.startswith('%token') :
        return line.split()[1:]
    else :
        return []

def parse_file(name='../yacc_files/ansi_c.y') :
    f = open(name)
    lines = f.readlines()

    named_tokens = []
    unnamed_tokens = []
    nonterminals = []

    i = 0
    while lines[i].strip() != '%%' :
        named_tokens.extend(parse_tokens(lines[i]))
        i += 1

    print 'Parsed %d named tokens:' %len(named_tokens)
    print named_tokens

    # Skip the %% line
    i += 1

    while lines[i].strip() != '%%' :
        line = lines[i].strip()
        if line.isspace() or line == '' :
            continue

        # Must be the begining of a definition
        current_nt = line.split(':')[0]
        print 'Processing definition of nonterminal', current_nt

        if current_nt not in nonterminals :
            nonterminals.append(current_nt)
        else :
            print 'Problem: repeat definition of nonterminal', current_nt





    print PATTERN.split(data)[1::2]







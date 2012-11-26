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
        if lines[i].strip().isspace() or lines[i].strip() == '' :
            continue



    print PATTERN.split(data)[1::2]







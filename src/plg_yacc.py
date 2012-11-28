import re
NT_LIST = re.compile(r'''((?:[^\s"']|"[^"]*"|'[^']*')+)''')
C_COMMENT = re.compile(r'''(/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/)|(//.*)''')

class NonTerminal :
    name = ''
    rules = 0
    references = 0

    def __init__ (self, name) :
        self.name = name

    def __str__ (self) :
        return self.name

    def __eq__ (self, other) :
        return self.name == other.name

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

    for i in range(i, len(lines)) :
        if lines[i].strip() == '%%' :
            break

        if  (   lines[i].strip().isspace()
                or  lines[i].strip()
                or  C_COMMENT.match(lines[i]) == '' ) :
            continue

        print 'Processing line %s' %lines[i]

        if not lines[i].startswith(':') and not lines[i].startswith('|') :
            current_nt = NonTerminal(lines[i].split(':')[0].strip())
            print 'Found non-terminal: %s' %current_nt
            if current_nt in nonterminals :
                print 'Problem: non-terminal %s defined more than once!' %nt.name

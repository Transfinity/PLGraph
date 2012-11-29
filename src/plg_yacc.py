import re
import sys
from collections import namedtuple

"""
Rule for a nonterminal.  Delimited by '|'.  Examples (each line would match):
    word1 word2
    | word3 "literal can contain | symbol"
    | word5
"""
NT_RULE = re.compile(r'''((?:[^\s"']|"[^"]*"|'[^']*')+)''')

""" A standard C comment """
C_BLOCK_COMMENT = re.compile(r'''(/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/)''')
C_LINE_COMMENT = re.compile(r'''//.*$''')

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

def prep(line) :
    rtn = line.strip()
    return C_LINE_COMMENT.sub('', line).strip()

class YaccParser :
    named_tokens = {}
    unnamed_tokens = {}
    nonterminals = {}

    def __init__ (self, filename='../yacc_files/ansi_c.y') :
        self.filename = filename

    def extract_tokens (self, line) :
        tokens = []
        if (  line.startswith('%token')
                or line.startswith('%left')
                or line.startswith('%right')
                or line.startswith('%nonassoc')) :
            tokens = line.split()[1:]

        if len(tokens) > 1 and re.match('<.*>', tokens[0]) :
            tokens = tokens[1:]

        for tok in tokens :
            if tok.startswith('"') :
                continue
            self.named_tokens[tok] = Literal(tok)
            self.named_tokens[tok].rules += 1

    def extract_rules (self, current_nt, line) :
        line = line.strip(': \t')
        rules = line.split('|')
        for rule in rules :
            rule = rule.strip()
            if is_empty(rule) :
                if current_nt.rules == 0 :
                    current_nt.rules = 1
                else :
                    continue

            print '    production rule: %s' %rule
            current_nt.rules += 1
            for match in NT_RULE.findall(rule) :
                if match == ';' :
                    break

                if match.startswith('"')  or match.startswith("'") :
                    print '        unnamed token:', match
                    match = match[1:-1]
                    if match not in self.unnamed_tokens :
                        self.unnamed_tokens[match] = Literal(match)
                    self.unnamed_tokens[match].references += 1

                elif match in self.named_tokens :
                    print '        named token:', match
                    self.named_tokens[match].references += 1

                else :
                    print '        non-terminal:', match
                    if match not in self.nonterminals :
                        self.nonterminals[match] = Literal(match)
                    self.nonterminals[match].references += 1

    def parse_file(self) :
        f = open(self.filename)
        # Remove block comments, even multi-liners
        lines = C_BLOCK_COMMENT.sub('', f.read()).split('\n')

        i = 0
        while lines[i].strip() != '%%' :
            self.extract_tokens(lines[i])
            i += 1

        while i < len(lines)-1 :
            i += 1
            line = prep(lines[i])

            if line == '%%' :
                break

            if is_empty(line) or line.startswith('.') :
                continue

            print 'Parsing line %d: %s' %(i, line)

            # Should be the first line of a nonterminal definition
            nt_name = line.split(':')[0].strip()
            if nt_name not in self.nonterminals :
                self.nonterminals[nt_name] = Literal(nt_name)
            current_nt = self.nonterminals[nt_name]
            print 'Definition for non-terminal "%s"' %current_nt

            # Get the rules for this nonterminal
            remainder = ''.join(line.split(':')[1:]).strip()
            while not line.endswith(';') :
                i += 1
                line = prep(lines[i])
                remainder += ' ' + line

            # Extract individual rules
            self.extract_rules(current_nt, remainder)

            print 'End non-terminal "%s"\n' %current_nt
            current_nt = None


        print '-----FINISHED PARSING-----'
        print 'Found %d named tokens:' %len(self.named_tokens)
        rows = []
        Row = namedtuple('Row', ['name', 'referenced'])
        for tok in self.named_tokens.values() :
            rows.append(Row(tok.name, tok.references))
        rows.sort(key=lambda x: -x[1])
        pprinttable(rows)

        print '\nFound %d unnamed tokens:' %len(self.unnamed_tokens)
        rows = []
        Row = namedtuple('Row', ['name', 'referenced'])
        for tok in self.unnamed_tokens.values() :
            rows.append(Row("'%s'" %tok.name, tok.references))
        rows.sort(key=lambda x: -x[1])
        pprinttable(rows)

        print '\nFound %d nonterminals:' %len(self.nonterminals)
        rows = []
        Row = namedtuple('Row', ['name', 'rules', 'referenced'])
        for nt in self.nonterminals.values() :
            rows.append(Row(nt.name, nt.rules, nt.references))
        rows.sort(key=lambda x: -x[1])
        pprinttable(rows)






# Someone else's function, prints data in a pretty table
def pprinttable(rows):
    if len(rows) > 1:
        headers = rows[0]._fields
        lens = []
        for i in range(len(rows[0])):
            lens.append(len(max([x[i] for x in rows] + [headers[i]],key=lambda x:len(str(x)))))
        formats = []
        hformats = []
        for i in range(len(rows[0])):
            if isinstance(rows[0][i], int):
                formats.append("%%%dd" % lens[i])
            else:
                formats.append("%%-%ds" % lens[i])
            hformats.append("%%-%ds" % lens[i])
        pattern = " | ".join(formats)
        hpattern = " | ".join(hformats)
        separator = "-+-".join(['-' * n for n in lens])
        print hpattern % tuple(headers)
        print separator
        for line in rows:
            print pattern % tuple(line)
    elif len(rows) == 1:
        row = rows[0]
        hwidth = len(max(row._fields,key=lambda x: len(x)))
        for i in range(len(row)):
            print "%*s = %s" % (hwidth,row._fields[i],row[i])

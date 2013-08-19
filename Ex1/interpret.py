
"""Interpreter for a mini-language with variables and loops.

   Operator trees that are interpreted are defined as

PTREE ::=  [ DLIST, CLIST ]
DLIST ::=  [ DTREE* ]
           where  DTREE*  means zero or more DTREEs
DTREE ::=  ["int", VAR]  |  ["proc", VAR, CLIST]
CLIST ::=  [ CTREE+ ]
           where  CTREE+  means one or more CTREEs
CTREE ::=  ["=", VAR, ETREE]  |  ["print", ETREE]  |  ["while", ETREE, CLIST]
        |  ["if", ETREE, CLIST, CLIST]  |  ["call", VAR]
ETREE ::=  NUMERAL  |  VAR  |  [OP, ETREE, ETREE]
           where  OP  is either "+" or "-"

There is one crucial data structure:

  ns is a namespace --- it holds the program's variables
  and their values.  It is a Python hash table (dictionary).
  For example,
     ns = {'x': 2,
           'p': [['=' 'y', ['+', 'x', '1']], ['print', 'y']],
           'y': 0},
  holds vars  x, p, and y,  where int x has value 2,
  proc p has the command list for  y=x+1; print y  as its value,
  and int y has value 0.
"""
ns      = {}
intset  = set()
procset = set()

def interpretPTREE(p):
    """pre: p  is a program represented as a PTREE ::=  [ DLIST, CLIST ]
       post:  ns  holds all the updates commanded by program  p
    """
    interpretDLIST(p[0])   # extract the declarations and process them
    interpretCLIST(p[1])   # extract the commands and execute them

def interpretCLIST(clist):
    """pre: clist  is a list of command trees:  CLIST ::=  [ CTREE+ ]
       post:  ns  holds all the updates commanded by   clist
    """
    for command in clist :
        interpretCTREE(command)

def interpretDLIST(dlist):
    """pre:  dlist is a list of declaration trees: DLIST ::=  [ DTREE* ]
       post: ns holds all the declarations described by dlist
    """
    for declaration in dlist :
      interpretDTREE(declaration)

def interpretCTREE(c) :
    """pre: c  is a command represented as a CTREE:
         CTREE ::= ["=", VAR, ETREE] | ["print", VAR] | ["while", ETREE, CLIST]
                 | ["if", ETREE, CLIST, CLIST] | ["call", VAR]
       post:  ns  holds the updates commanded by  c
    """
    operator = c[0]
    if operator == "=" :                   # assignment command, ["=", VAR, ETREE]
        var = c[1]                         # get left-hand side
        if var in ns :                     # if already declared
            exprval = interpretETREE(c[2]) # evaluate the right-hand side
            ns[var] = exprval              # do the assignment
        else :
            msg = "undeclared variable %s" % (var)
            crash(msg)

    elif operator == "print" :             # print command, ["print", ETREE]
        exprval = interpretETREE(c[1])     # evaluate the expression
        print exprval                      # print the value

    elif operator == "while" :             # while command,  ["while", ETREE, CLIST]
        expr = c[1]
        body = c[2]
        while (interpretETREE(expr) != 0) :
            interpretCLIST(body)

    elif operator == "if" :                # if condition ["if", ETREE, CLIST, CLIST]
        exp  = c[1]
        body1 = c[2]
        body2 = c[3]
        if interpretETREE(exp) == 0 :      # test the computed value if false
            interpretCLIST(body2)          # execute then condition
        else :                             # otherwise
            interpretCLIST(body1)          # execute the if condition

    elif operator == "call" :              # call procedure ["call", VAR]
        proc = c[1]
        if proc in procset :
            if proc in ns :                # see if variable name is defined
                exp = ns[proc]             # look up its value
                interpretCLIST(exp)        # evaluate the expression
            else :
                msg = "undeclared procedure %s" % (proc)
                crash(msg)
        else :
            msg = "%s is not a procedure" % (proc)
            crash(msg)

    else :   # error
        crash("invalid command")

def interpretETREE(e) :
    """pre: e  is an expression represented as an ETREE:
            ETREE ::=  NUMERAL  |  VAR  |  [OP, ETREE, ETREE]
                       where OP is either "+" or "-"
      post:  ans  holds the numerical value of  e
      returns:   ans
    """
    if isinstance(e, str) and  e.isdigit() :   # a numeral
        ans = int(e)
    elif isinstance(e, str) and len(e) > 0  and  e[0].isalpha() :  # var name
        if e not in intset : # is var name e decalared as an int
            msg = "variable name %s not declared int" % (e)
            crash(msg)
        elif e not in ns:    # is var name e assigned a value in the namespace ?
            msg = "variable name %s not declared" % (e)
            crash(msg)
        else :
            ans = ns[e]      # look up its value
    else :   #  [op, e1, e2]
        op   = e[0]
        ans1 = interpretETREE(e[1])
        ans2 = interpretETREE(e[2])
        if op == "+" :
            ans = ans1 + ans2
        elif op == "-" :
            ans = ans1 - ans2
        else :
            msg = "illegal arithmetic operator %s" % (op)
            crash(msg)
    return ans

def interpretDTREE(d) :
    """pre:  d is a delaration represented as a DTREE:
             DTREE ::=  ["int", VAR]  |  ["proc", VAR, CLIST]
       post: int var is added to ns with default value 0 or
             proc name VAR is added to ns with value CLIST
       returns:
    """
    if d[0] == "int" :                       # ["int", VAR]
        var = d[1]
        if var in ns :                       # if already declared
            msg = "cannot redefine variable %s" % (var)
            crash(msg)
        else :
            ns[var] = 0                      # add to ns
            intset.add(var)                  # add to set of ints
    elif d[0] == "proc" :                    # ["proc", VAR, CLIST]
        proc = d[1]
        if proc in ns :                      # if already declared
            msg = "cannot redefine procedure %s" % (proc)
            crash(msg)
        else :
            ns[proc] = d[2]                  # add proc to ns
            procset.add(proc)                # add proc to procset
    else :
        msg = "expected declaration of int or proc, found %s" % (d[0])
        crash(msg);

def crash(message) :
    """pre: message is a string
       post: message is printed and interpreter stopped
    """
    print message + "! crash! core dump:", ns
    raise Exception   # stops the interpreter


def main(program) :
    """pre:  program is a  PTREE ::=  CLIST
       post:  ns  holds all updates within  program
    """
    global ns #  ns  is global to main
    ns = {}
    interpretPTREE(program)
    print "final namespace =", ns



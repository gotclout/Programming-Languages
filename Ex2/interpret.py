
### INTERPRETER FOR OBJECT-ORIENTED LANGUAGE

"""The interpreter processes parse trees of this format:
PTREE ::= [DLIST, CLIST]
DLIST ::= [ DTREE* ]
          where  DTREE*  means zero or more DTREEs
DTREE ::= ["int", ID, ETREE]
          | ["proc", ID, ILIST, CLIST, DLIST]
          NOTE that the local decls come last and might be []
CLIST ::= [ CTREE* ]
CTREE ::= ["=", LTREE, ETREE]  |  ["if", ETREE, CLIST, CLIST]
          | ["print", ETREE]  |  ["call", LTREE, ELIST]
ELIST ::= [ ETREE* ]
ETREE ::= NUM  |  [OP, ETREE, ETREE] |  ["deref", LTREE]
          where  OP ::=  "+"  | "-"
LTREE ::= ID
ILIST ::= [ ID* ]
ID    ::= a nonempty string of letters
NUM   ::= a nonempty string of digits

The interpreter computes the meaning of the parse tree, which is
a sequence of updates to heap storage.
"""

from heapmodule import *   # import the contents of the  heapmodule.py  module 

### INTERPRETER FUNCTIONS, one for each class of parse tree listed above.
#   See the end of program for the driver function,  interpretPTREE
def interpretDLIST(dlist) :
    """pre: dlist  is a list of declarations,  DLIST ::=  [ DTREE+ ]
       post:  memory  holds all the declarations in  dlist
    """
    for dec in dlist :
      interpretDTREE(dec)

def interpretELIST(elist) :
  for exp in elist :
    interpretETREE(exp)

def interpretDTREE(d) :
    """pre: d  is a declaration represented as
            DTREE ::=  ["int", ID, ETREE] |
                       ["proc", ID, ILIST, CLIST, []]
       post:  heap is updated with  d
    """
    if d[0] == "int" :         #process int declarations
      declare(activeNS(), d[1], interpretETREE(d[2]))
    elif d[0] == "proc" :      #process proc declarations
      if inActiveNS(d[1]) :    #cannot redeclare process in NS
        msg = "%s already declared in %s" % (proc, activeNS())
        crash(d, msg)
      else :                   #allocate closre and NS for proc
        c = allocateClosure(d[1], d[2], d[3], d[4])
        cns = activeNS()
        pns = allocateNS()
        heap[cns][d[1]] = pns  #put the new ns in active ns
        heap[pns] = c          #set the closure for the new ns
    else :
      msg = "expected declaration of int or proc, found %s" % (d)
      crash(d, msg)

def interpretCLIST(clist) :
    """pre: clist  is a list of commands,  CLIST ::=  [ CTREE+ ]
                  where  CTREE+  means  one or more CTREEs
       post:  memory  holds all the updates commanded by program  p
    """
    for command in clist :
      interpretCTREE(command)

def interpretCTREE(c) :
    """pre: c  is a command represented as a CTREE:
            CTREE ::=  ["=", LTREE, ETREE] |
                       ["if", ETREE, CLIST, CLIST] |
                       ["print", ETREE] |
                       ["call", LTREE, ELIST]
       post:  heap  holds the updates commanded by  c
    """
    operator = c[0]
    if operator == "=" :              # , ["=", LTREE, ETREE]
        handle, field = interpretLTREE(c[1])
        rval = interpretETREE(c[2])
        store(handle, field, rval)
    elif operator == "print" :        # ["print", LTREE]
        print interpretETREE(c[1])
        printHeap()
    elif operator == "if" :           # ["if", ETREE, CLIST1, CLIST2]
        test = interpretETREE(c[1])
        if test != 0 :
          interpretCLIST(c[2])
        else :
          interpretCLIST(c[3])
    elif operator == "call" :         # ["call", LTREE, ELIST]
        handle, field = interpretLTREE(c[1]) # compute the meaning of L
        closure = getClosure(field)   # retrieves the closure for L
        if len(closure) < 1 :         # L is not bound to a closure
          msg = "Could not find closure for: ", field
          crash(c, msg)
        else :
          ilist = closure[1]          # IL
          cns = activeNS()            # save current NS
          ns = allocateNS()           # allocate new NS
          astackpush(ns)              # push new NS to activation stack
          if len(closure) > 3 :       # process local declarations
            if closure[3][0][0] == "int" :
              interpretDTREE(closure[3][0])
            if len(closure[3]) > 1 and closure[3][1][0] == "proc" :
              interpretDTREE(closure[3][1])
          ilistlen = len(ilist)
          if ilistlen == len(c[2]) :  # bind EL to IL
            for i in range(0, ilistlen) :
              heap[ns][ilist[i]] = interpretETREE(c[2][i])
            interpretCLIST(closure[2])# execute CL
            astackpop()               # pop activation stack
          else :
            msg = "IListLen != EListLen" # args in EL must equal params in IL
            crash(c, msg)
    else :  crash(c, "invalid command")


def interpretETREE(etree) :
    """interpretETREE computes the meaning of an expression operator tree.
         ETREE ::=  NUM  |  [OP, ETREE, ETREE] |  ["deref", LTREE] 
         OP ::= "+" | "-"
        post: updates the heap as needed and returns the  etree's value
    """
    if isinstance(etree, str) and etree.isdigit() :  # NUM
        ans = int(etree)
    elif  etree[0] in ("+", "-") :                   # [OP, ETREE, ETREE]
        ans1 = interpretETREE(etree[1])
        ans2 = interpretETREE(etree[2])
        if isinstance(ans1,int) and isinstance(ans2, int) :
          if etree[0] == "+" :
            ans = ans1 + ans2
          elif etree[0] == "-" :
            ans = ans1 - ans2
        else :
          msg = "addition error --- nonint value used", "ans1: ",\
                ans1, "ans2: ", ans2
          crash(etree, msg)
    elif  etree[0] == "deref" :                      # ["deref", LTREE]
        handle, field = interpretLTREE(etree[1])
        ans = lookup(handle,field)
    else :  crash(etree, "invalid expression form")
    return ans


def interpretLTREE(ltree) :
    """interpretLTREE computes the meaning of a lefthandside operator tree.
          LTREE ::=  ID
       post: returns a pair,  (handle,varname),  the L-value of  ltree
    """
    if isinstance(ltree, str) :  #  ID ?
      ns = resolveNS(ltree, activeNS()) # retrieve the NS ltree belongs to
      ans = (ns, ltree)                 # use the handle to the active namespace
    else :
      crash(ltree, "illegal L-value")
    return ans

def crash(tree, message) :
    """pre: tree is a parse tree,  and  message is a string
       post: tree and message are printed and interpreter stopped
    """
    print "Error evaluating tree:", tree
    print message
    print "Crash!"
    printHeap()
    raise Exception   # stops the interpreter


### MAIN FUNCTION
def interpretPTREE(tree) :
    """interprets a complete program tree
       pre: tree is a  PTREE ::= [ DLIST, CLIST ]
       post: heap holds all updates commanded by the  tree
    """
    initializeHeap()
    interpretDLIST(tree[0])
    interpretCLIST(tree[1])
    print "Successful termination."
    printHeap()


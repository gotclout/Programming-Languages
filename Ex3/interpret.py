
### INTERPRETER FOR OBJECT-ORIENTED LANGUAGE

"""The interpreter processes parse trees of this format:
PTREE  ::= [DLIST, CLIST]
DLIST  ::= [ DTREE* ]
            (where  DTREE*  means zero or more DTREEs)
DTREE  ::= ["int", ID, ETREE] |
           ["proc", ID, ILIST, CLIST, DLIST] |
            (NOTE that the local decls come last and might be [])
           ["ob", ID, ETREE]
CLIST  ::= [ CTREE* ]
CTREE  ::= ["=", LTREE, ETREE] | ["if", ETREE, CLIST, CLIST] |
           ["print", ETREE] | ["call", LTREE, ELIST]
ELIST  ::= [ ETREE* ]
ETREE  ::= NUM | [OP, ETREE, ETREE] | ["deref", LTREE] |
            (where  OP ::=  "+"  | "-")
           "nil" | ["new",  TTREE]
TTREE  ::= ["struct", DLIST]
LTREE  ::= ID | ["dot", LTREE, ID]
NUM    ::= a nonempty string of digits
IDLIST ::= [ ID+ ]
ID     ::= a nonempty string of letters

The interpreter computes the meaning of the parse tree, which is
a sequence of updates to heap storage.
"""
from heapmodule import *   #import the contents of the  heapmodule.py  module 

### INTERPRETER FUNCTIONS, one for each class of parse tree listed above.
#   See the end of program for the driver function,  interpretPTREE
def interpretDLIST(dlist) :
  """pre: dlist  is a list of declarations,  DLIST ::=  [ DTREE+ ]
     post:  memory  holds all the declarations in  dlist
  """
  for dec in dlist :
    interpretDTREE(dec)

def interpretELIST(elist) :
  ans = []
  for exp in elist :
    ans.append(interpretETREE(exp))
  return ans

def interpretDTREE(d) :
  """pre: d  is a declaration represented as
          DTREE ::=  ["int", ID, ETREE] |
                     ["proc", ID, ILIST, CLIST, []] |
                     ["class", ID, TTREE] |
                     ["ob", ID, ETREE]
     post:  heap is updated with  d
  """
  if d[0] == "int" :                               #["int", ID, ETREE]
    declare(activeNS(), d[1], interpretETREE(d[2]))
  elif d[0] == "proc" :                            #["proc", ID, ILIST, CLIST, []]
    ns = allocateNS()
    declare(activeNS(), d[1], ns)
    c = allocateClosure(ns, "proc", d[2], d[3], d[4])
  elif d[0] == "ob" :                              #["ob", ID, ETREE]
    e = interpretETREE(d[2])                       #computes the meaning of E
                                                   #E is a handle to  an object or nil
    if e == "nil" or e in heap :
      declare(activeNS(), d[1], e)                 #bind I to E in the activeNS
    else :
      msg = "Error: %s must be a valid handle or nil" % e
      crash(d, msg)
  elif d[0] == "class" :                           #["class", ID, TTREE]
    ns = allocateNS()
    declare(activeNS(), d[1], ns)
    c = allocateClosure(ns, "class", [], d[2], []) #bind I to a closure containing T
  else :
    msg = "expected declaration of int, ob, proc, or class found %s" % (d[0])
    crash(d, msg)

def interpretCLIST(clist) :
  """pre: clist  is a list of commands
          CLIST ::=  [ CTREE+ ] (where  CTREE+  means  one or more CTREEs)
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
  if operator == "=" :                   #["=", LTREE, ETREE]
    handle, field = interpretLTREE(c[1])
    rval = interpretETREE(c[2])
    store(handle, field, rval)
  elif operator == "print" :             #["print", LTREE]
    print interpretETREE(c[1])
    printHeap()
  elif operator == "if" :                #["if", ETREE, CLIST1, CLIST2]
    test = interpretETREE(c[1])
    if test != 0 :
      interpretCLIST(c[2])
    else :
      interpretCLIST(c[3])
  elif operator == "call" :              #["call", LTREE, ELIST]
    handle, field = interpretLTREE(c[1]) #compute the meaning of L
    value = lookup(handle, field)
    closure = lookupClosure(value, field)
    if len(closure) < 1 :                #L is not bound to a closure
      msg = "Could not find closure for: ", field
      crash(c, msg)
    elif closure[0] == "proc" :
      ilist = closure[1]                 #IL
      elist = interpretELIST(c[2])
      if len(ilist) == len(elist) :      #bind EL to IL
        curr = allocateNS()              #allocate new NS
        for i in range(0, len(ilist)) :
          declare(curr, ilist[i], elist[i])
        astackpush(curr)                 #push new NS to activation stack
        interpretDLIST(closure[3])
        interpretCLIST(closure[2])       #execute CL
        astackpop()                      #pop activation stack
        if "dot" in c[1] :
          astackpop()
      else :
        msg = "num args not eq params"   #args in EL must equal params in IL
        crash(c, msg)
    else :
      msg = "called closure is not a proc: " + handle + " " + field
      crash(c, msg)
  else : crash(c, "invalid command")

def interpretETREE(etree) :
  """interpretETREE computes the meaning of an expression operator tree.
       ETREE ::=  NUM |
                  [OP, ETREE, ETREE] |
                  ["deref", LTREE] |
                  "nil" |
                  ["new", TTREE] (where OP ::= "+" | "-")
      post: updates the heap as needed and returns the  etree's value
  """
  if isinstance(etree, str) and etree.isdigit() : #NUM
    ans = int(etree)
  elif etree[0] in ("+", "-") :                   #[OP, ETREE, ETREE]
    ans1 = interpretETREE(etree[1])
    ans2 = interpretETREE(etree[2])
    if isinstance(ans1,int) and isinstance(ans2, int) :
      if etree[0] == "+" :
        ans = ans1 + ans2
      elif etree[0] == "-" :
        ans = ans1 - ans2
      else :
        msg = "addition error --- nonint value used","ans1: ",ans1,"ans2: ",ans2
        crash(etree, msg)
  elif etree[0] == "deref" :                       #["deref", LTREE]
    handle, field = interpretLTREE(etree[1])
    ans = lookup(handle,field)
  elif isinstance(etree, str) and etree == "nil" : #nil
    ans = str("nil")
  elif etree[0] == "new" :                         #["new", TTREE]
    ans = interpretTTREE(etree[1])
  else :  crash(etree, "invalid expression form: " + etree[0])
  return ans

def interpretTTREE(ttree) :
  """
     pre: receives arguments of the form, ["struct", DLIST] |
                                          ["call", LTREE]
     ttree:
  """
  if ttree[0] == "struct" :                  #["struct", DLIST]
    newns  = allocateNS()                    #allocates a new namespace
    astackpush(newns)                        #pushes ns onto the activation stack
    interpretDLIST(ttree[1])                 #evaluates DLIST
    return astackpop()                       #pops activation stack and return
  elif ttree[0] == "call"  :
    handle, field = interpretLTREE(ttree[1]) #compute the meaning of L
    value = lookup(handle, field)
    closure = lookupClosure(value, field)    #retrieve the closure
    if closure[0] == "class" :
      return interpretTTREE(closure[2])      #interpret the enclosed ttree
    else :
      msg = "error: interpretTTREE could not retrieve closure"
      crash(ttree, msg)
  else :
    msg = "error: interpretTTREE expected call, encountered ", ttree[0]
    crash(ttree, msg)

def interpretLTREE(lt) :
  """interpretLTREE computes the meaning of a lefthandside operator tree.
        LTREE ::=  ID |
                   ["dot", LTREE, ID]
     lt:
     post: returns a pair,  (handle,varname),  the L-value of  ltree
  """
  if isinstance(lt, str) :       #ID
    return (resolveNS(lt), lt)   #use the handle to the active namespace
  elif lt[0] == "dot" :          #["dot", LTREE, ID]
    v, f = interpretLTREE(lt[1]) #compute the handle named by L
    h = lookup(v, f)
    if isLValid(h, lt[2]) :      #check if the pair, (h,I) is a valid L-value
      astackpush(h)              #variable I is a field inside the object named by h
      return (h, lt[2])
    else :
      msg = "Error: invalid handle ", v, lt[2]
      crash(lt, msg)
  else :
    crash(lt, "illegal L-value")

def crash(tree, message) :
  """pre: tree is a parse tree,  and  message is a string
     post: tree and message are printed and interpreter stopped
  """
  print "Error evaluating tree:", tree
  print message
  print "Crash!"
  printHeap()
  raise Exception #stops the interpreter


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


###  HEAP-STORAGE MODULE
"""The program's heap is a dictionary that maps handles to namespaces.
   An object is itself a namespace (dictionary).
      heap : { (HANDLE : NAMESPACE)+ }
             where  HANDLE = a string of digits
                    NAMESPACE = a dictionary that maps var names to ints:
                                { (ID : INT)* }
   Example:
     heap = { "0": {"x":7, "y":1, "z":2} }
     heap_count = 1
        is an example heap, where handle "0" names a namespace
        whose  x  field holds int 7, "y" field holds int 1,
        and "z" holds int 2.

   The above example heap was generated from this sample program:
        int y = 1;  int x = (6 + y);  int z = y;
        z = (z + y)
"""
heap = {}
heap_count = 0  # how many objects stored in the heap

### Activation Stack Module
"""The activation stack represented as a stack of handles,
   where the handles named objects in the heap.

   Ex:
   activation stack =  ['h0', 'h2']
   heap = {
     h0 : {'parentns': 'nil', 'p': 'h1', 'x': 2}
     h1 : ['proc', [], [['print', ['deref', 'x']], ['=', 'x', ['+', ['deref', 'x'], '1']]], 'h0']
     h2 : {'parentns': 'h0'}
   }

   astacksize = 2
   astackpush("h3")
   astacksize = 3
   activation stack = ['h0', 'h2', 'h3']
   astackpop removes h3 from the top
   astacksize = 2
   astackempty() returns astacksize == 0
"""
astack = []     # the activation stack
astacksize = 0  # size of the activation stack

### Closure Module
"""The set of closures implemented as a dictionary. A closure holds (at least)
   two items:
    - the code for the procedure (or, a pointer to the address where the code
      is stored) and
    - the handle of the parent (global) namespace that the procedure's body
      will use to look up nonlocal variables.
      ex: closures[p] =
        ['proc', [], [['print', ['deref', 'x']], ['=', 'x', ['+', ['deref', 'x'], '1']]], 'h0'
"""
closures = {}

### Maintenance functions:

def astackpush(h):
  """Pushes an activation record to the top of the stack
     h: The handle to be pushed
     post: h is at the top of the stack
  """
  global astacksize, astack
  astacksize += 1
  astack.append(h)

def astackempty():
  """Indicates whether or not the stack is empty
     return: true if empty false otherwise
  """
  return astacksize == 0

def astackpop():
  """Erases the activation record from the top of the stack
     return: nil if empty the top element othersie
  """
  global astacksize, astack
  if astackempty() :
    return "nil"
  else :
   astacksize -= 1
   return astack.pop()

def astacktop():
  """Retrieves the activation record from the top of the stack
     return: top element or nil if empty
  """
  global stacksize, astack
  if astackempty() :
    return "nil"
  else :
    return astack[astacksize - 1]

def activeNS():
    """returns the handle of the namespace that holds the currently visible
       program variables
    """
    return astacktop()

def inActiveNS(uid):
  """determines whether or not the specified uid is in the active NS
     uid: the identifier in question
     return: true if uid is in activeNS false otherwise
  """
  if uid in heap[activeNS()] :
    return 1
  else :
    return 0

def initializeHeap():
    """resets the heap for a new program"""
    global heap_count, heap, ns, astack, astacksize
    astackpush(allocateNS())  # create namespace in  heap  for global variables

def printHeap():
    """prints contents of  stack  and  heap"""
    print "activation stack = ", astack

    print "heap = {"
    handles = heap.keys()
    handles.sort()
    for h in handles:
        print " ", h, ":", heap[h]
    print "}"

def getClosure(uid) :
  """Retrieves the closure for the specified uid
     uid:
     return:
  """
  c = []
  if uid in closures :
    c = closures[uid]
  return c

def allocateClosure(proc, ilist, clist, l) :
  """ Allocates a closure for the specified proc
      proc: proc id
      ilist: identifier list
      clist: command list
      l: remaining declarations
      post: the newly allocated closure is added to the set of closures
      return: the newly allocated closure
  """
  global closures
  c = []
  if not inActiveNS(proc) :
    c.append("proc")
    c.append(ilist)
    c.append(clist)
    if len(l) > 0 :
      c.append(l)
    c.append(activeNS())
    closures[proc] = c

  return c

def allocateNS() :
    """allocates a new, empty namespace in the heap and returns its handle"""
    global heap_count, heap
    newloc = "h" + str(heap_count)  # generate handle of form,  hn,  where  n  is an int
    heap[newloc] = {"parentns" : astacktop()}
    heap_count = heap_count + 1
    return newloc

def isLValid(handle, field):
    """checks if  (handle, field)  is a valid L-value, that is, checks
       that  heap[handle]  is a namespace  and   field  is found in it.
       returns  True  if the above holds true; returns  False  otherwise.
    """
    return (handle in heap) and (field in heap[handle])


def lookup(handle, field) :
    """looks up the value of  (handle,field)  in the heap
       param: handle,field -- such that  isLValid(handle, field)
       returns: The function extracts the object at  heap[handle],
                indexes it with field,  and returns  (heap[handle])[field]
    """
    if isLValid(handle, field) :
        return  heap[handle][field]
    else :
      pns = heap[handle]["parentns"]
      if pns == "nil" :
        crash("invalid lookup address: " + handle + " " + field)
      elif field in heap[pns] :
        return heap[pns][field]
      else :
         return lookup(pns, field)

def resolveNS(uid, ns) :
  """Locates the NS to which the uid belongs
     uid: the identifier to be resolved
     ns: the top level namespace
     return: the namespace to which the identifier belongs error if not found
  """
  if uid in heap[ns].keys() :
    return ns
  else :
    pns = heap[ns]["parentns"]
    if pns == "nil" :
      msg = "could not resolve ns for: ", uid
      crash(msg)
    else :
      return resolveNS(uid, pns)

def declare(handle, field, rval) :
    """creates a new definition in the heap at (handle, field) and initializes
       it with rval, provided that  heap[handle][field] does not already exist!
       (else crashes with a "redeclaration error")
       params: handle, field, as described above
               rval -- an int or a handle
    """
    if handle in heap :
      if field not in heap[handle] :
        heap[handle][field] = rval
      else :
        msg = "%s already declared in active namespace %s " % (field, handle)
        crash(msg)

def store(handle, field, rval) :
    global heap
    """stores  rval  at heap[handle][field], provided that
         (i)  isLValid(handle,field)
         (ii) the type of  rval  matches the type of what's already stored at
              heap[handle][field]
       (else crashes with a type-error message)

       params:  handle, field, as described above
                rval -- an int or a handle
    """
    if isLValid(handle, field) :
      heap[handle][field] = rval
    else :
      msg = "L value invalid for handle %s : field %s" % (handle, field)
      crash(msg)

def crash(message) :
    """prints message and stops execution"""
    print "Heap error: ", message, " Crash!"
    printHeap()
    raise Exception   # stops the interpreter


fun intListToString nil =
    "nil" |
      intListToString (x::xs) =
          Int.toString(x) ^ "::" ^ intListToString(xs);

(* insert(m, ns) inserts m in the correct position within list  ns.
   Precondition:  m is an int and ns is a sorted list of ints.
   Postcondition: the returned answer is a sorted list containing exactly
                 m and the elements in  ns.
   Example:  insert(3,[2,4]) returns [2,3,4].
   use "q2.sml"; insert(2,[]);
   use "q2.sml"; insert(3,[1,4,5]);
   use "q2.sml"; insert(4,[1,3]);
*)
fun insert(m, nil)   = [m] |
    insert(m, n::ns) =
    (
      print( "m: "); print( Int.toString(m) ); print("\n");
      print( "n: "); print( Int.toString(n) ); print("\n");
      print( "ns: "); print( intListToString ns ); print("\n");
      if m < n then m::n::ns
      else n::insert(m, ns)
    );

(* isortloop(ns, ans) calls  insert  to insert the elements of ns into ans.
   Precondition:  ns is a list of ints and  ans  is a sorted list of ints
   Postcondition: the answer returned is a sorted list of ints containing
                  exactly the elements of ns and ans.
   Call it like this: isortloop(ns,[]),
   e.g., isortloop([4,2,3,5,1],[]) returns [1,2,3,4,5]
   use "q2.sml"; isortloop([],[]);
   use "q2.sml"; isortloop([3,2,1],[]);
   use "q2.sml"; isortloop([4,2,3,5,1],[]);
*)
fun isortloop(nil, ans)   = ans |
    isortloop(n::ns, ans) =
    (
      print( "n: " ^ Int.toString(n) ^ "\n");
      print( "ns: "^ intListToString(ns) ^ "\n");
      print( "ans: " ^ intListToString(ans) ^ "\n");
      if ans = nil orelse ns = nil orelse n < hd(ans) then isortloop(ns, n::ans)
      else isortloop(tl(ns), hd(ans)::n::tl(ans))@[hd(ns)]
    )

(* isort(ns)  calls  insert  to sort  ns.
   Precondition:  ns  is a list of ints
   Postcondition:  the answer returned is a sorted list of ints containing
   exactly the elements of  ns.
   Example:  isort([4,2,3,5,1])  returns  [1,2,3,4,5]
   use "q2.sml"; isort([]);
   use "q2.sml"; isort([3,2,1]);
   use "q2.sml"; isort([4,2,3,5,1]);
*)
fun isort(nil) = nil | isort(n::ns) = insert(n, isort(ns))

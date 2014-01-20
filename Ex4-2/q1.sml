(*fun intListToString nil =
    "nil" |
      intListToString (x::xs) =
          Int.toString(x) ^ "::" ^ intListToString(xs);

 least : int list -> int
   returns the smallest int in NONEMPTY list, ns.
   E.g.,   least [3,1,2]  returns  1 
           least [4]  returns  4
           least [5, 3, 8, 3]  returns  3  *)
fun least [n] = n |
    least (n::ns) =
    (
      (*print( "least "); print( Int.toString(n) ); print("::");
      print( intListToString ns); print("\n");*)
      let val m = least ns
        in if n < m then n else m
      end
    ) |
    least nil = raise Empty (* the "empty list" error.  This equation is optional *)

(*
  use "q2.sml"; least [3,1,2];
  use "q2.sml"; least [4];
  use "q2.sml"; least [5,3,8,3];
*)

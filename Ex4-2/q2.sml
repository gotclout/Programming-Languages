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
  Standard ML of New Jersey v110.74 [built: Mon Aug 19 11:36:30 2013]
  - use "q2.sml"; least [3,1,2];
  [opening q2.sml]
  val least = fn : int list -> int
  val it = () : unit
  val it = 1 : int

  Standard ML of New Jersey v110.74 [built: Mon Aug 19 11:36:30 2013]
  - use "q2.sml"; least [4];
  [opening q2.sml]
  val least = fn : int list -> int
  val it = () : unit
  val it = 4 : int

  use "q2.sml"; least [5,3,8,3];
  Standard ML of New Jersey v110.74 [built: Mon Aug 19 11:36:30 2013]
  - use "q2.sml"; least [5,3,8,3];
  [opening q2.sml]
  val least = fn : int list -> int
  val it = () : unit
  val it = 3 : int
*)

(* remove : int * int list  -> int list
   builds an answer list that is exactly  ns  but with the first occurrence, if any, of  m  removed.
   For example,  remove(2, [3, 2 ,7])  returns  [3, 7]
                 remove(2, [3, 2, 7, 2])  returns  [3, 7, 2]
                 remove(2, [3])  returns  [3]
*)
fun remove(m, nil) = nil |
    remove(m, (n::ns)) =
    (
      if m = n then ns (*remove(m, ns)*)
      else if m <> n then n::remove(m, ns)
      else []
    )

(**

  Standard ML of New Jersey v110.74 [built: Mon Aug 19 11:36:30 2013]
  [opening q2.sml]
  q2.sml:55.12 Warning: calling polyEqual
  q2.sml:56.17-56.19 Warning: calling polyEqual
  val least = fn : int list -> int
  val remove = fn : ''a * ''a list -> ''a list
  val ssort = fn : int list -> int list
  - remove(2, [3, 2 ,7])
  = ;
  val it = [3,7] : int list
  - remove(2, [3, 2, 7, 2]);
  val it = [3,7,2] : int list
  - remove(2, [3]);
  val it = [3] : int list

**)

(* ssort : int list ->  int list   uses the selection-sort strategy to build
   a list that contains the ints in the argument list, sorted in ascending
   order
   E.g.,  ssort [3,2,5,1,4]  returns  [1,2,3,4,5] 
          ssort [3]  returns  [3] 
          ssort [3,2,1,2]  returns  [1,2,2,3] 
*)
fun ssort nil = nil  (* empty-list case     *) |
    ssort xs  =      (* non-empty list case *)
    (
      let val min = least(xs)
        in min::ssort(remove(min, xs))
      end
    )

(**
  Standard ML of New Jersey v110.74 [built: Mon Aug 19 11:36:30 2013]
  [opening q2.sml]
  q2.sml:55.12 Warning: calling polyEqual
  q2.sml:56.17-56.19 Warning: calling polyEqual
  val least = fn : int list -> int
  val remove = fn : ''a * ''a list -> ''a list
  val ssort = fn : int list -> int list
  -  ssort [3,2,5,1,4]
  = ;
  val it = [1,2,3,4,5] : int list
  - ssort [3] 
  = ;
  val it = [3] : int list
  - ssort [3,2,1,2]
  = ;
  val it = [1,2,2,3] : int list
  - 
**)

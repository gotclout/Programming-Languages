
(****** Baby database management system *)

(* A database is a structure of key,value pairs,
          where a key is a char and a value is an int
          and the  pairs  are stored as a binary tree of  Leafs and Nodes,
              ordered on the keys.
   When a user does a new update to the database, the update is patched to
     the front of the database and NOT inserted into the ordered tree.
   When the user requests a "commit", the entire contents of the database
     is rebuilt into a new ordered tree.
   See the Assignment 4 sheet for examples.
*)

datatype DB = Leaf 
           |  Node of (char * int * DB * DB)
           |  Update of (char * int * DB * DB)


(****** Functions that convert data structures to strings for printing:  *)

(* converts a list of  key,value  pairs to a string for printing: *)
fun pairslist2string(ns) = 
    let fun convert(nil) = ""
        |   convert((p,q)::rest) = "("^str(p)^","^Int.toString(q)^"), "^convert(rest)
    in "["^convert(ns)^"]"
    end

(**
 * Finds the specified key value pair in a list of pairs 
 * Returns true if item is found, false otheriwse
 *)
fun findinlist((k,v), p::pairs) =
      let val (kk,vv) = p
      in
        if (k,v) = p then true
        else if pairs = nil then false
        else findinlist((k,v), pairs)
      end


(**
 * Retrieves the position of a key value pair in a list of pairs
 *
 * Returns the position the pair in the list if found offset by list
 * size, -1 otherwise
 *)
fun getpairpos(x::lpl, rpl) =
    (
        let val (k, v) = x
            val ll = length(lpl)
          val lll = ll + 1
        in
        if findinlist(x, rpl) then length(lpl) + 1
        else getpairpos(lpl, rpl)
        end
    ) |
    getpairpos(nil, rpl) = ~1

(* converts a  DB  to a formatted string for printing: *)
fun DB2string(tree) =
    let fun format(Leaf, indent) = indent ^ "Leaf"
        |   format(Node(c,i,d1,d2),indent) = 
                indent ^ "Node("^str(c)^"," ^Int.toString(i) ^", "
                ^"\n" ^format(d1, indent^"    ")^", "
                ^"\n" ^format(d2, indent^"    ")^")"
        |   format(Update(c,i,d1,d2),indent) = 
                indent ^ "Update("^str(c)^"," ^Int.toString(i) ^", "
                ^"\n"^indent^"    IGNORE KEYS >= "^str(c)^" IN LEFT SUBTREE:"
                ^"\n" ^format(d1, indent^"    ")^", "
                ^"\n"^indent^"    IGNORE KEYS <= "^str(c)^" IN RIGHT SUBTREE:"
                ^"\n" ^format(d2, indent^"    ")^")"

    in format(tree, "")
    end
;

(****** Helper functions for indexing and slicing lists:  
        Note: these operations are built into ML's  String  module,
        but I am listing them here to give you more examples to read!
*)

(* indexAt : int * 'a list -> 'a
   indexAT(n, xs)  returns the  nth  item in list xs,  counting by 0,1,2,...
*)
fun indexAt(0, (x::xs)) = x
|   indexAt(n, (x::xs)) = indexAt(n-1, xs)

(* itemsUpToIndex : int * 'a list ->  'a list
   itemsUpToIndex(n, xs)  returns the prefix of  xs  up to item #n
*)
fun itemsUpToIndex(n, []) = []
|   itemsUpToIndex(0, xs) = []
|   itemsUpToIndex(n, (x::xs)) = x :: itemsUpToIndex(n-1, xs)

(* itemsAfterIndex : int * 'a list ->  'a list
   itemsAfterIndex(n, xs)  returns the suffix of  xs  after item #n
*)
fun itemsAfterIndex(n, []) = []
|   itemsAfterIndex(0, (x::xs)) = xs
|   itemsAfterIndex(n, (x::xs)) = itemsAfterIndex(n-1, xs)
;
(***** Operations on DB values *****)

(* buildDB : (char * int) list -> DB
   constructs a tree from a list of  key,value  pairs.
   If the list is sorted on its chars, 
   the resulting tree will be ordered and balanced.
   param: pairs - a list of char,int  pairs, sorted on the char values
   returns: an ordered DB tree holding all the  pairs
*)
fun buildDB [] =  Leaf
|   buildDB(pairs : (char * int) list) =
    let
        val mid = length(pairs) div 2   (* get index number of middle of list *)
        val (k,v) = indexAt(mid, pairs)  (* get middle pair in list *)
        val lefthalf = itemsUpToIndex(mid, pairs)  (* get prefix of list *)
        val righthalf = itemsAfterIndex(mid, pairs) (* get suffix of list *)
    in  Node(k, v, buildDB(lefthalf), buildDB(righthalf))
    end
;

(* update  pastes a new key,value update to the "top" of the database
     and sets the left and right subtree links to point into the database,
     preserving the lookup properties of an ordered binary-search tree.
   params: k - a char,   v - an int,   db - the database
   returns: an updated database
*)
fun update(k, v, rootnode) =
  let fun extract(Node(kk,vv,left,right)) = (kk,vv,left,right) |
          extract(Update(kk,vv,left,right)) = (kk,vv,left,right)
      val (kk,vv,left,right) = extract rootnode
      (* Determines an updates right child node **)
      fun rightchild(pk, node) =
      (
        let val (kk,vv,left,right) = extract node in
        if pk < kk then node
        else if pk > kk then rightchild(pk, right)
        else Leaf
        end
      )
      (** Determines an updates left child node **)
      fun leftchild(pk, node) =
      (
        let val (kk,vv,left,right) = extract node in
        if pk > kk then node
        else if pk < kk then leftchild(pk, left)
        else Leaf
        end
      )
      val lchild = leftchild(k, rootnode)
      val rchild = rightchild(k, rootnode)
  in Update(k, v, lchild, rchild) end
;

exception LookupError  (* declaration of run-time error message *)

(* lookup(k, db)   finds the value associated with a key in the database
   params:  k - the key,   db - the database
   returns the value, v,  such that  k,v  is saved in  db
*)
fun lookup(k, Leaf) = raise LookupError
|   lookup(k, currentNode) =
        let fun extract(Node(kk,vv,left,right)) = (kk,vv,left,right)
            |   extract(Update(kk,vv,left,right)) = (kk,vv,left,right)
            val (kk,vv,left,right) = extract currentNode
        in  if k = kk then vv
            else if k < kk then lookup(k, left)
                           else lookup(k, right)
        end  
;

(* collect : DB -> (char * int) list
   collect(db)  traverses DB and extracts all the reachable  (key,value)
   pairs and returns them in a sorted list.
   IMPORTANT:  key,value pairs that are overridden by updates are removed
   from the answer.
*)
(* THIS CODE IS INCORRECT: IT COLLECTS DUPLICATE (K,V)-PAIRS AND
   DOES NOT DELETE OVERRIDDEN (K,V) PAIRS.  FIX IT.  *)
fun collect(Leaf) = [] |
    collect(Node(k,v,left,right)) =
    (
      let fun contains([], (k,v)) = false |
              contains(p::pl, (k,v)) =
              (
              let val (kk, vv) = p
                in
                  if (k, v) = p then true
                  else contains(pl, (k,v))
                end
              )
        val cleft = collect(left)
        val cright = collect(right)
      in
          cleft@[(k,v)]@cright
      end
    ) |
    collect(Update(k,v,left,right)) =
    (
      (** collect left and right hand side **)
      let val lhs   = collect(left)
          val rhs   = collect(right)

          (** get the first/last position of a duplicate pair in lhs **)
          val lfirst = length(lhs) - getpairpos(lhs, rhs)
          val llast  = length(lhs) - getpairpos(itemsAfterIndex(lfirst, lhs), rhs)

          (** get the first/last position of a duplicate pair in rhs **)
          val rfirst = length(rhs) - getpairpos(rhs, lhs)
          val rlast  = length(rhs) - getpairpos(itemsAfterIndex(rfirst, rhs), lhs)

          (** indicates k, v is the element being updated **)
          val (lk, lv) = indexAt(llast - 1, lhs);
      in
        (* lsubtree > r & lsubtree contains k, v *)
        if length(lhs) > length(rhs) andalso k = lk then
           itemsUpToIndex(llast - 1, lhs) @
           [(k,v)] @
           itemsAfterIndex(rlast - 1, rhs)
        (* lsubtree > r *)
        else if length(lhs) > length(rhs) then
            itemsUpToIndex(llast, lhs) @
            [(k,v)] @
            itemsAfterIndex(rlast, rhs)
        else
            lhs @
            [(k,v)] @
            itemsAfterIndex(rlast, rhs)
      end
    )
 ;

(**** DRIVER FOR SYSTEM ****)

(* run(data)  loads the data base and "loops", asking for user requests, 
      till terminated by  !    The requests can be of these forms:
      p        (print database)
      l k      (lookup k     where  k  is a char)
      u k i    (update k i   where  k  is a char and  i  is a digit)
      r        (revert)
      c        (commit updates)
      !        (quit)

    param:  data  is a list of  char,int  pairs, where the chars are ordered
    Example:   run([(#"a",6), (#"c",7), (#"d",2)])
    or         run([])
*)
exception CommandError
exception RevertError

fun run(data) =
    let
        fun loop(db, history) = 
            let val txt = (print "> "; explode(valOf(TextIO.inputLine TextIO.stdIn)))  (* read next input command *)
            (*  val txt = explode(TextIO.inputLine TextIO.stdIn)   *)  (* use this on older implementations *)
                val code = hd txt
            in  
            if code = #"!"          (* if head char is "!" *)
            then (print "Final database:\n";
                  print (DB2string(db));  print("\n")
                 )
            else if code = #"p"     (* print *)
                  then (print "Database:\n";
                        print (DB2string(db) ^ "\n");
                        loop(db, history)
                        )
            else if code = #"u"     (* update:  u k v *)
                 then let val k = hd(tl(tl txt))
                          val v = ord(hd(tl(tl(tl(tl txt))))) - ord(#"0")
                      in (print "update performed\n"; 
                          loop(update(k, v, db), db::history)
                          )
                      end
            else if code = #"l"  (* lookup:  l k *)
                  then let val k = hd(tl(tl txt))
                       in (print "lookup\n";
                          print (Int.toString(lookup(k, db)));  print "\n";
                          loop(db, history)
                          )
                       end
            else if code = #"r"    (* revert:  r   *)
                  then (print "revert\n"; 
                        if not(history = [])
                        then loop(hd history, tl history)
                        else raise RevertError
                        )
            else if code = #"c"     (* commit checkpoint *)
                  then (print "Commit database:\n";
                        print (DB2string(db) ^ "\n") ;
                        let val pairs = collect(db)
                        in (print("Active values: " ^ pairslist2string(pairs) ^ "\n");
                           let val newdb = buildDB(pairs)
                           in
                           (print("New database:\n");
                            print (DB2string(newdb) ^ "\n");
                            loop(newdb, nil))  (* history is emptied *)
                           end)
                       end
                       )
            else raise CommandError
            end
            handle LookupError => (print("\nlookup error\n"); loop(db,history)) |
                   RevertError => (print("\nrevert error\n"); loop(db,history)) |
                   CommandError => (print("\ncommand error\n"); loop(db, history))
        
    in loop(buildDB(data), nil)   (* start with empty history *)
    end

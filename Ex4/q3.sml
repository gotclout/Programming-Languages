
(* BABY DATABASE MANAGEMENT SYSTEM *)

(* A database is a structure of key,value pairs,
         where a key is a string  and  a value is an int
         and the  pairs  are stored as a binary-search tree of  Leafs and Nodes,
         ordered on the keys.
   When a user does a new update to the database, the update is patched to
     the front of the database and NOT inserted into the ordered tree.
   When the user requests a "commit", the entire database
     is rebuilt into a new tree.  Examples follow.
*)

datatype DB = Leaf |  Node of (char * int * DB * DB)  | Update of (char * int * DB)

(* Say we start with these entries:  [ ("c",6), ("d",7), ("a",2) ].
   The ordered tree constructed from these entries is

   Node("c",6, Node("a",2,Leaf,Leaf), Node("d",7,Leaf,Leaf))

   Say these two updates are requested:  ("a",5), ("b",8).
   The database looks like this:

   Update(("b",8),
          Update(("a",5),
                 Node("c",6, Node("a",2,Leaf,Leaf), Node("d",7,Leaf,Leaf))))

   This means a lookup of key "a" gives 5, a lookup of "c" gives 6, etc.
   If we "commit" now, we would rebuild the above database as the tree:

   Node("b",8, Node("a",5,Leaf,Leaf), 
               Node("c",6,Leaf, Node("d"7,Leaf,Leaf)))

   The old entry for "a" is discarded.
*)


(***** Functions that convert data structures to strings for printing:  *****)

(* converts a list of key,value pairs to a string: *)
fun pairslist2string(ns) = 
    let fun convert(nil) = ""
        |   convert((p,q)::rest) = "("^str(p)^","^Int.toString(q)^"), "^convert(rest)
    in "["^convert(ns)^"]"
    end

(* converts a DB to a string: *)
fun DB2string(Leaf) = "Leaf"
|   DB2string(Node(c,i,d1,d2)) = "Node("^str(c)^","^Int.toString(i)^", "^DB2string(d1)^", "^DB2string(d2)^")"
|   DB2string(Update(c,i,d)) = "Update("^str(c)^","^Int.toString(i)^", "^DB2string(d)^")"



(***** Operations on DB values *****)

exception BuildError   (* this defines an error value *)

(* build  constructs an ordered-tree database from a list of  key,value pairs
   param: pairs - a list of char,int  pairs, where all chars are distinct
   returns: a DB that is an ordered tree holding all the  pairs
   If there are two chars that are the same, then  BuildError  is raised.
*)
fun build(pairs) =
    let fun insert(k, v, Leaf) = Node(k, v, Leaf, Leaf)
        |   insert(k, v, Node(kk, vv, left, right)) =
                if k = kk then raise BuildError
                else if k < kk then Node(kk, vv, insert(k, v, left), right)
                               else Node(kk, vv, left, insert(k, v, right))

    and build1(nil, tree) = tree
    |   build1((k, v)::rest, tree) = build1(rest, insert(k, v, tree))
    in
    build1(pairs, Leaf)
    end


(* update  pastes a new key,value update to the front of the database.
   params: k - a char,   v - an int,   db - the database
   returns: an updated database
*)
fun update(k, v, db) = Update(k, v, db)


exception LookupError

(* lookup(k, db)   finds the value associated with a key in the database
   params:  k - the key,   db - the database
   returns the value, v,  such that  k,v  is saved in  db.
   If  k  isn't found in  db,  raises LookupError.
   The type of this function should be   lookup : char * DB -> int
*)
fun lookup(k, db) = (print "lookup: Write Me\n"; raise LookupError)


exception RevertError

(* revert(db)  removes the last-made update to the database  db  and
   returns the smaller database.  If  db  was not updated, raises RevertError
*)
fun revert(Leaf) = raise RevertError
|   revert(Node(_,_,_,_)) = raise RevertError
|   revert(Update(_,_,db)) = db


(* collect(db)  traverses the database and
   returns a list of all the visible (key,value) pairs in  db, removing any entries 
   that are cancelled by updates.   For example,
   collect( Update(("b",8),
              Update(("a",5),
                Node("c",6, Node("a",2,Leaf,Leaf), Node("d",7,Leaf,Leaf)))) )
    returns  [("b",8), ("a",5), ("c",6), ("d",7)]
   The type of this function should be   collect : DB -> (char * int) list
*)
fun collect(db) = (print "collect: Write Me\n"; nil)


(**** DRIVER FOR SYSTEM ****)

(* run(data)  loads the data base and loops, asking for user requests
      till terminated by  !    The requests can be of these forms:
      l k      (lookup k     where  k  is a letter)
      u k i    (update k i   where  k  is a letter and  i  is a digit)
      r        (revert (undo) last update)
      c        (commit all updates)
      !        (quit)
    param: data is a list of  char,int  pairs, where the chars must be distinct
    Examples:   run([(#"c",6), (#"d",7), (#"a",2)])
                run([])
*)
exception CommandError

fun run(data) =
    let
        fun loop(db) = 
            let val txt = (print "> "; explode(valOf(TextIO.inputLine TextIO.stdIn)))  (* read next input command *)
            (*  val txt = explode(TextIO.inputLine TextIO.stdIn)   *)  (* use this on older implementations *)
            in  
            case hd txt of   (* what is the value of the head char? *)
               #"!"  =>   (* quit *)
                      (print "Final database:\n";
                       print (DB2string(db));  print("\n");
                       pairslist2string(collect(db))
                       )
            |  #"u"  =>   (* update:  u k v *)
                      let val k = hd(tl(tl txt))
                           val v = ord( hd(tl(tl(tl(tl txt))))) - ord(#"0")
                      in (print "update performed\n"; 
                          loop(update(k, v, db))
                          )
                      end
            | #"l"  => (* lookup:  l k *)
                       let val k = hd(tl(tl txt))
                       in (print "lookup\n";
                          print (Int.toString(lookup(k, db)));
                          print "\n";
                          loop(db)
                          )
                       end
            |  #"r"  =>  (* revert:  r   *)
                       (print "revert performed\n"; 
                        loop(revert(db)))
            |  #"c"  =>    (* commit:  c  *)
                       (print "Commit database:\n";
                        print (DB2string(db) ^ "\n");
                        let val pairs = collect(db)
                        in (print ("Active values: " ^ pairslist2string(pairs) ^ "\n");
                           let val newdb = build(pairs)
                           in
                            (print("New database:\n");
                            print (DB2string(newdb) ^ "\n");
                            loop(newdb))
                           end)
                       end)
            | _ => raise CommandError
            (* end case *)
            end
            handle LookupError => (print("\nlookup error\n"); loop(db)) |
                   RevertError => (print("\nrevert error\n"); loop(db)) |
                   CommandError => (print("\ncommand error\n"); loop(db))
        
    in loop(build(data))
    end


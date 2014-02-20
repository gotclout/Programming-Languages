/*
  Baby database implemented by Prolog predicates,
  mapsto(Key, Value)   and    upd(Key, Value)  
  Assertions will be inserted by using  asserta
  and removed by using  retract  and  retractall :  
*/
:- dynamic mapsTo/2, upd/2.

/* clauses to build a database as a set of  mapsTo  clauses:  */
/* addDB(List)  holds true exactly when all the  (K,V)  pairs in  List
are asserted as   mapsTo(K,V)  in the database   */
addDB([]).
addDB([(K,V)|Rest]) :- asserta(mapsTo(K,V)), addDB(Rest).

getupdatemaplist(MULD) :- findall((K,V), mapsTo(K,V), ML), findall((KK, VV), upd(KK, VV), UL), append(UL, ML, MUL), removedups(MUL, MULD).

/*
  lookup(K)  holds true when  K,V  is found in the database.  V  prints.  )irs, mapsTo(KK, VV), Pairs)
*/
lookup(K) :- findall((K,V), mapsTo(K,V), P), findall((KK, VV), upd(KK, VV), Q), ((member((XX, YY), Q), XX = K, write(YY)) ; (member((X, Y), P), X = K, write(Y))), nl.

/*
  update(K,NewV)  holds true when  upd(K,NewV)  is asserted in the database
*/
update(K, NewV) :- asserta(upd(K,NewV)), write(K), write(' updated'), nl.

/*
  revert  holds true when the most recently asserted  upd  is retracted.
*/
revert :- retract(upd(K,V)), write(K), write(' reverted'), nl, !.

/*
  Note: The cut, !, ensures that the call to  revert  is
  satisfied/proved-true/executed successfully exactly one time.
*/

/* commit  holds true when:
   1.  one finds all the (K,V) pairs such that  upd(K,V)  holds true 
       and saves them in a list (Hint: use findall);
   2.  one retains only the most recent (visible) updates of the (K,V) pairs
       from the list in Step 1 (Hint: use  removedups  below).
   3.  one retracts all the  upd(K,_) and mapsTo(K,_) predicates
       for each (K,V) in the list in Step 2.  (Hint: use  retractall)
   4.  one adds  mapsTo(K,V)  for each (K.V) pair in the list in Step 2.
       (Hint: use  addDB.)
*/

getupdates(U) :- write('Updates to commit: ['), findall((K, V), upd(K, V), U), member((KK, VV), U), ( write('('), write(KK','VV), write(')')), write(' ]'), nl.

commit :- getupdates(UL), removedups(UL, DUL),
          retractalltheupdates(DUL),
          getupdatemaplist(UML), addDB(DUL).

/*
  retractalltheupdates(L)  holds true exactly when all the  (XX,VV)  pairs in  L  are retracted from the database
*/
retractalltheupdates([]).   /* nothing to do --- finished. */
retractalltheupdates([(XX,_)|Rest]) :- retract(upd(XX,_)), retract(mapsTo(XX, _)), retractalltheupdates(Rest) .   /* retract the  XX  binding and do TheRest */
                                       removedups(L, M) :- nodups(L, M), !.

nodups([], []).
nodups([(K,V)|R], [(K,V)|T]) :- nodups(R, S), select((K,_), S, T).
nodups([(K,V)|R], [(K,V)|T]) :- nodups(R, T).

/*
  Note:  select(I, L, M)  is a built-in Prolog predicate 
  that succeeds if it finds I in L and removes it, yielding M.)
*/

/*
  FYI: this version avoids cut (!), but it is less nice to use:
  removedups([], []).
  removedups([(K,V)|R], [(K,V)|T]) :- removedups(R, S), select((K,_), S, T).
  removedups([(K,V)|R], [(K,V)|T]) :- removedups(R, T), notmember(K, T).
  notmember(K, []).
  notmember(K, [(KK,V)|T]) :- K \= KK,  notmember(K, T).
*/


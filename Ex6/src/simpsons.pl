/*
  female facts
 */
female('Carnival Woman').
female('Penelope Olsen').
female('Lisa Simpson').
female('Patricia Bouvier').
female('Maggie Simpson').
female('Selma Bouvier').
female('Marge Bouvier').
female('Jacqueline Gurney').
female('Ling Bouvier').

/*
  males facts
*/
male('Herbert Powell').
male('Clancy Bouvier').
male('Abraham Simpson').
male('Homer Simpson').
male('Bart Simpson').


/*
   predicate: parents(CHILD, FATHER, MOTHER)
   asserts that CHILD has FATHER as its father and MOTHER as its mother:
*/
parents('Herbert Powell',  'Abraham Simpson', 'Carnival Woman').
parents('Homer Simpson',   'Abraham Simpson', 'Penelope Olsen').
parents('Bart Simpson',    'Homer Simpson',   'Marge Bouvier').
parents('Lisa Simpson',    'Homer Simpson',   'Marge Bouvier').
parents('Maggie Simpson',  'Homer Simpson',   'Marge Bouvier').
parents('Selma Bouvier',   'Clancy Bouvier',  'Jacqueline Gurney').
parents('Marge Bouvier',   'Clancy Bouvier',  'Jacqueline Gurney').
parents('Patricia Bouvier','Clancy Bouvier',  'Jacqueline Gurney').
parents('Ling Bouvier',    '',                'Selma Bouvier').

/*
  sister(X, Y) defines that X has Y as a sister if
  Y is female,
  X's and Y's parents are the same and
  X != Y
*/
sister(X,Y) :-
  female(Y),
  parents(X,Father,Mother),
  parents(Y,Father,Mother),
  X \= Y.

/*
  brother(X, Y) defines that X has Y as a brother if
  Y is male,
  X's and Y's parents are the same and
  X != Y
*/
brother(X,Y) :-
  male(Y),
  parents(X,Father,Mother),
  parents(Y,Father,Mother),
  X \= Y.

/*
  aunt(X, A)  defines that  X  has  A  as an aunt
*/
aunt(X, A) :-
  parents(X, Father, Mother),
  (sister(Father, A) ; sister(Mother, A)).

/*
  uncle(X, U)  defines that  X  has  U  as an uncle
*/
uncle(X, U) :-
  parents(X, Father, Mother),
  (brother(Father, U) ; brother(Mother, U)).


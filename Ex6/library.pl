/* Functor for defining book and dvd objects:
      ITEM ::=  book(TITLE, AUTHOR)  |  dvd(TITLE)
                       where TITLE and AUTHOR are strings

   Predicates that define ownership of an object
   and when an object is borrowed from the library:
      PRED ::=  owns(KEY, ITEM)  |  borrowed(KEY, PERSON, DUEDATE)
                      where KEY is an atom that begins with  k
                            ITEM is defined above
                            PERSON is an atom
                            DUEDATE is an int
*/
owns(k0, book('David Copperfied', 'Charles Dickens')).
owns(k1, book('Tale of Two Cities', 'Charles Dickens')).
owns(k2, book('Tale of Two Cities', 'Charles Dickens')).
owns(k3, dvd('Tale of Two Cities')).
owns(k4, book('Moby Dick', 'Herman Melville')).
owns(k5, dvd('Brothers Karamazov', 'Fyodor Dostoyevski')).
borrowed(k2, 'Homer', 10).
borrowed(k4, 'Homer', 20).
borrowed(k3, 'Lisa', 40).
borrowed(k0, 'Lisa', 45).
borrowed(k5, 'Homer', 90).

/* overdue  holds true when  ItemKey is borrowed (by  Who)  and is due earlier than  Today */
overdue(ItemKey, Today) :- borrowed(ItemKey, _, DueDate),
                           Today > DueDate.

/* fine  calculates the int value of  HowMuch  when  ItemKey  is overdue as of  Today */
fine(ItemKey, Today, HowMuch) :- overdue(ItemKey, Today),
                                 borrowed(ItemKey, _, DueDate),
                                 HowMuch is  Today - DueDate.

/* getBorrowed(Who, ItemList)  calculates a list of  KEYs
     of all the items  Who  has borrowed. */
getBorrowed(Who, ItemList) :- findall(Key, borrowed(Key, Who, _), ItemList).

/* getOverdue(Who, Today, ItemList)  calculates  ItemList,  which is a list of  KEYs
     of all the items  Who  has borrowed that are overdue as of  Today. */
getOverdue(Who, Today, ItemList) :-
  findall(Key, ( borrowed(Key, Who, Day), ( Today > Day) ), ItemList).

/* sum(L, T)  holds true when  T  is the sum of all the ints in list  L. */
sum([], 0).
sum([N|Rest], Total) :- sum(Rest, M), Total is N + M.

/* totalfine(Who, Today, Amount)  calculates  Amount,  which is an int
      that states the total fine that  Who  owes due to overdue items as of  Today  */
totalfine(Who, Today, Amount) :-
  getOverdue(Who, Today, Keys), findall(F, (member(M, Keys), fine(M, Today, F)), Fines), sum(Fines, Amount).

/*

?- [library]. totalfine(_, 100, Total).

% library compiled 0.00 sec, 1 clauses
true.

Total = 295.

?- totalfine('Lisa', 25, Amt).
Amt = 0.

?- totalfine('Homer', 25, Amt).
Amt = 20.

?- 

*/




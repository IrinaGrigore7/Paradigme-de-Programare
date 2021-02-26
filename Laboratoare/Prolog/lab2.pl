%1. Write the predicate sublist/2 which constructs each sublist of a given list.
app([],L,L).
app([H|T],L, [H|Rp]) :- app(T,L,Rp).

sublist(_,[]).
sublist(L,[H|T]) :- app(R1,_,L), app(_,[H|T],R1).

%3. Write the predicate natlist/1 which generates each finite list of natural number in ascending order. 
natlist([0]).
natlist(L) :- natlist(N), app(_, [X], N), Y is X + 1, app(N, [Y], L).

%4. Write the predicate oddOnly/2 which removes all even integers from a list. 
oddOnly([], []).
oddOnly([H|T], [H|R]) :- H mod 2 =:= 0, oddOnly(T, R). 
oddOnly([H|T], R) :- not(H mod 2 =:= 0), oddOnly(T, R).

%5. Write the predicate oddList/1 which generates all finite lists of odd numbers, in ascending order. 
oddOnly1([0]).
oddOnly1(L) :- oddOnly1(N), app(_, [X], N), Y is X + 2, app(N, [Y], L).

%6. Write a predicate eqelem/1 which generates all lists of uninstantiated variables which must be equal.

eqelem([_]).
eqelem(L) :- eqelem(X), .
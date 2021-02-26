student(gigel).
student(ionel).
student(john).
student(mary).

nat(zero).
nat(X) :- X = succ(Y), nat(Y).

add(zero, X, E) :- E = X.
add(succ(X), Y, succ(Res)) :- add(X, Y, Res).

minus(X, zero, R) :- R = X.
minus(X, Y, R) :- X = succ(M), Y = succ(N), minus(M, N, R1), R = R1.

toNat(0,zero).
toNat(X,succ(Res)) :- X>0, Xp is X-1, toNat(Xp,Res). 
 
fromNat(zero,0).
fromNat(succ(X),R) :- fromNat(X,Rp), R is Rp + 1. 

min(zero, _, R) :- R = zero.
min(_, zero, R) :- R = zero.
min(X, Y, R) :- X=succ(M), Y=succ(N), min(M, N, R1), R = succ(R1), !.

max(zero, X, R) :- R = X.
max(X, zero, R) :- R = X.
max(X, Y, R) :- X = succ(M), Y = succ(N), max(M, N, R1), R = succ(R1), !.

greater(succ(_), zero).
greater(succ(X), succ(Y)) :- greater(X, Y), !.

less(zero, zero).
less(zero, succ(_)).
less(succ(X), succ(Y)) :- less(X, Y).

div(X, Y, zero) :- less(X, Y).
div(X, Y, R) :- minus(X, Y, D), div(D, Y, Rp), R = succ(Rp).

mod(X, Y, R) :- less(X,Y), R = X, !.
mod(X, Y, R) :- minus(X, Y,D), mod(D, Y, R).

gcd(zero , X, X) :- greater(X, zero), !.
gcd(X, Y, Z) :- greater(X, Y), minus(X,Y, X1), gcd(X1, Y, Z).
gcd(X, Y, Z) :- less(X, Y), minus(Y, X, X1), gcd(X1, X,Z).


isList(void).
isList(cons(_,T)) :- isList(T).

head(cons(H, _), H).
tail(cons(_, T), T).
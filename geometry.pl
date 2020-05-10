/** # Functional Geometry
  
The classic paper by Peter Henderson "Functional Geometry" explains how to create an algebra
than can describe complex objects and shows how to use that to render Escher's Square Limit.
 
This file will explore the concept in the context of Prolog.

@author Daan van Berkel
*/

/* ## Algebraic Terms
Below we will describe the terms that we will use to describe complex pictures. They are
divided in the following classes

### Complex Terms
The following complex terms all accepts other terms. For a discription see the 
Functional Geometry paper.

* `turn(T)`
* `flip(T)`
* `toss(T)`
* `above(U, V)`
* `beside(U, V)`
* `quartet(U, V, W, X)`
* `nonet(P, Q, R, S, T, U, V, W, X)`

## Rendering
A rendering is a list of drawing instructions

* `d` represents the letter d. It is a-symmetric so operations on it are clearly visible.
* `fish` Escher fish.

*/
stamp(d).

escher(N, C) :-
    corner(N, P),
    side(N, Q),
    utile(T),
    nonet(P,
          Q,
          turn(turn(turn(P))),
          turn(Q),
          T,
          turn(turn(turn(Q))),
          turn(P),
          turn(turn(Q)),
          turn(turn(P)),
          C).

corner(0, Stamp) :-
    stamp(Stamp).
corner(N, C) :-
    succ(M, N),
    corner(M, U),
    side(M, V),
    utile(X),
    quartet(U, V, turn(V), X, C).

side(0, Stamp) :-
    stamp(Stamp).
side(N, C) :-
    succ(M, N),
    side(M, U),
    ttile(W),
    quartet(U, U, turn(W), W, C).

utile(quartet(U, turn(turn(turn(U))), turn(turn(U)), turn(U))) :-
    stamp(U).

ttile(over(Stamp, over(U, turn(turn(turn(U)))))) :-
    stamp(Stamp),
    U=flip(toss(Stamp)).

nonet(P, Q, R, S, T, U, V, W, X, C) :-
    column(P, S, V, Left),
    column(Q, T, W, Middle),
    column(R, U, X, Right),
    row(Left, Middle, Right, C).

column(P, Q, R, PQR) :-
    above(Q, R, QR),
    aboveRatio(1, 2, P, QR, PQR).

row(P, Q, R, PQR) :-
    beside(Q, R, QR),
    besideRatio(1, 2, P, QR, PQR).

quartet(S, T, U, V, C) :-
    above(S, U, L),
    above(T, V, R),
    beside(L, R, C).

above(U, V, C) :-
    aboveRatio(1, 1, U, V, C).

beside(U, V, C) :-
    besideRatio(1, 1, U, V, C).

aboveRatio(M, N, U, V, todo(M, N, U, V)).

besideRatio(M, N, U, V, todo(M, N, U, V)).

shape(d,
    [ polygon(vec(0.3, 0.2), vec(0.3, 0.5), vec(0.4, 0.6), vec(0.6, 0.6), vec(0.6, 0.9), vec(0.7, 0.9), vec(0.7, 0.1), vec(0.4, 0.1))
    , polugon(vec(0.40, 0.24), vec(0.40, 0.46), vec(0.44, 0.50), vec(0.60, 0.50), vec(0.60, 0.20), vec(0.44, 0.20))]).

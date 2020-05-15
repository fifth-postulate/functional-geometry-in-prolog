/** # Functional Geometry
  
The classic paper by Peter Henderson "Functional Geometry" explains how to create an algebra
than can describe complex objects and shows how to use that to render Escher's Square Limit.
 
This file will explore the concept in the context of Prolog.

@author Daan van Berkel
*/

/* ## Stamp
Eschers Square Limit is produced by stamping a base image. Here we define what it is.
*/
stamp(d).

defaultBox(box(vec(75.0, 75.0), vec(640.0, 0), vec(0.0, 640.0))).

/* ## Algebraic Terms
Below we will describe the terms that we will use to describe complex pictures. They are
divided in the following classes

### Fundamental Terms
These fundamental terms are the building blocks for more complex terms. 

* `scaleX(F, T)`
* `scaleY(F, T)`
* `moveX(F, T)`
* `moveY(F, T)`
* `turn(T)`
* `flip(T)`
* `toss(T)`
* `over(U, V)`

### Complex Terms
The following complex terms all defined in terms of fundamental terms. For a discription see the 
Functional Geometry paper.

* `aboveRatio(M, N, U, V, C)
* `besideRatio(M, N, U, V, C)
* `above(U, V, C)`
* `beside(U, V, C)`
* `column(P, Q, R, C)`
* `row(P, Q, R, C)`
* `quartet(U, V, W, X, C)`
* `nonet(P, Q, R, S, T, U, V, W, X, C)`
* `utile(T)`
* `ttile(T)`
* `corner(N, T)`
* `side(N, T)`
* `escher(N, T)`

*/

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

corner(0, C) :-
    utile(X),
    quartet(blank, blank, blank, X, C).
corner(N, C) :-
    succ(M, N),
    corner(M, U),
    side(M, V),
    utile(X),
    quartet(U, V, turn(V), X, C).

side(0, C) :-
    ttile(W),
    quartet(blank, blank, turn(W), W, C).
side(N, C) :-
    succ(M, N),
    side(M, U),
    ttile(W),
    quartet(U, U, turn(W), W, C).

utile(C) :-
    stamp(U),
    quartet(U, turn(turn(turn(U))), turn(turn(U)), turn(U), C).

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

aboveRatio(M, N, U, V, over(S, T)) :-
    F1 is float(M) / float(M+N),
    F2 is 1.0-F1,
    S = moveY(F2, scaleY(F1, U)),
    T = scaleY(F1, V).

besideRatio(M, N, U, V, over(S, T)) :-
    F1 is float(M) / float(M+N),
    F2 is 1.0-F1,
    S = scaleX(F1, U),
    T = moveX(F1, scaleX(F2, V)).

/* ## Box
A box is the sub-canvas to wich shapes are painted on. It is a term `box(A, B, C)`
where the `A`, `B` and `C` are 2 dimensional vectors.

Vectors are an other term `vec(X, Y)` and represent a point in the coordinate system.

## Rendering
Give a description of a scene, a box to paint it in and a number of shapes to paint a
rendering will form a description a picture.
*/

render(scaleX(F, T), box(A, vec(Bx, By), C), Result) :-
    FBx is F * Bx,
    render(T, box(A, vec(FBx, By), C), Result).

render(scaleY(F, T), box(A, B, vec(Cx, Cy)), Result) :-
    FCy is F * Cy,
    render(T, box(A, B, vec(Cx, FCy)), Result).

render(moveX(F, T), box(vec(Ax, Ay), vec(Bx, By), C), Result) :-
    Ax_ is Ax + F*Bx,
    render(T, box(vec(Ax_, Ay), vec(Bx, By), C), Result).

render(moveY(F, T), box(vec(Ax, Ay), B, vec(Cx, Cy)), Result) :-
    Ay_ is Ay + F*Cy,
    render(T, box(vec(Ax, Ay_), B, vec(Cx, Cy)), Result).

render(turn(T), box(A, B, C), Result) :-
    add(A, B, A_),
    rotate90(C, B_),
    negate(B, C_),
    render(T, box(A_, B_, C_), Result).

render(flip(T), box(A, B, C), Result) :-
    add(A, B, A_),
    negate(B, B_),
    render(T, box(A_, B_, C), Result).

render(toss(T), box(A, B, C), Result) :-
    add(B, C, BC),
    scale(0.5, BC, B_),
    add(A, B_, A_),
    subtract(C, B, CB),
    scale(0.5, CB, C_),
    render(T, box(A_, B_, C_), Result).

render(over(U, V), Box, Result) :-
    render(U, Box, UResult),
    render(V, Box, VResult),
    append(UResult, VResult, Result).

render(Shape, Box, Result) :-
    shape(Shape, DrawingPrimitives),
    fit(DrawingPrimitives, Box, Result).

/* ## Vector Algebra
These clauses help in doing vector algebra.
*/

add(vec(Ax, Ay), vec(Bx, By), vec(Cx, Cy)) :-
    Cx is Ax + Bx,
    Cy is Ay + By.

subtract(vec(Ax, Ay), vec(Bx, By), vec(Cx, Cy)) :-
    Cx is Ax - Bx,
    Cy is Ay - By.

negate(vec(Ax, Ay), vec(Cx, Cy)) :-
    Cx is -Ax,
    Cy is -Ay.

rotate90(vec(Ax, Ay), vec(Cx, Cy)) :-
    Cx is -Ay,
    Cy is Ax.

scale(F, vec(Ax, Ay), vec(Cx, Cy)) :-
    Cx is F * Ax,
    Cy is F * Ay.

/*
## Shape
A rendering is a list of drawing instructions

* `blank` represents no picture
* `d` represents the letter d. It is a-symmetric so operations on it are clearly visible.
* `fish` Escher fish.


*/

fit(DrawingPrimitives, Box, Result) :- fit(DrawingPrimitives, Box, [], Result).

fit([], _, Result, Result).
fit([polygon(Points)|Rest], Box, Accumulator, Result) :-
    transform(Points, Box, TransformedPoints),
    fit(Rest, Box, [polygon(TransformedPoints)|Accumulator], Result).

transform([], _, []).
transform([P|Rest], Box, [Q|TransformedRest]) :-
    transform(P, Box, Q),
    transform(Rest, Box, TransformedRest).

transform(vec(X, Y), box(vec(Ax, Ay), vec(Bx, By), vec(Cx, Cy)), vec(U, V)) :-
    U is Ax + (Bx + Cx) * X,
    V is Ay + (By + Cy) * Y.

shape(blank, []).
shape(d,
    [ polygon([vec(0.3, 0.2), vec(0.3, 0.5), vec(0.4, 0.6), vec(0.6, 0.6), vec(0.6, 0.9), vec(0.7, 0.9), vec(0.7, 0.1), vec(0.4, 0.1)])
    , polygon([vec(0.40, 0.24), vec(0.40, 0.46), vec(0.44, 0.50), vec(0.60, 0.50), vec(0.60, 0.20), vec(0.44, 0.20)])]).


/* ## Generate SVG
*/

svg -->
    open_svg,
    close_svg.

open_svg -->
    "<svg xmlns=""http://www.w3.org/2000/svg"">".

close_svg -->
    "</svg>".
/** # Functional Geometry
  
The classic paper by Peter Henderson "Functional Geometry" explains how to create an algebra
than can describe complex objects and shows how to use that to render Escher's Square Limit.
 
This file will explore the concept in the context of Prolog.

@author Daan van Berkel
*/

processTo(Name, Complex) :-
    open(Name, write, Output),
    current_output(StandardOutput),
    set_output(Output),
    defaultBox(Box),
    defaultBound(Bound),
    render(Complex, Box, Rendering),
    phrase(paint(Bound, Rendering), Svg),
    format("~s", [Svg]),
    close(Output),
    set_output(StandardOutput).

/* ## Stamp
Eschers Square Limit is produced by stamping a base image. Here we define what it is.
*/
stamp(d).


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
*/

defaultBox(box(vec(50.0, 50.0), vec(400.0, 0), vec(0.0, 400.0))).
unitBox(box(vec(0.0, 0.0), vec(1.0, 0.0), vec(0.0, 1.0))).

/* ## Rendering
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
    negate(B, C_),
    render(T, box(A_, C, C_), Result).

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

/* ## Shape
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

transform(U, box(A, B, C), W) :-
    U = vec(X, Y),
    scale(X, B, B_),
    scale(Y, C, C_),
    add(A, B_, V),
    add(V, C_, W).

shape(blank, []).
shape(d,
    [ polygon([vec(0.3, 0.2), vec(0.3, 0.5), vec(0.4, 0.6), vec(0.6, 0.6), vec(0.6, 0.9), vec(0.7, 0.9), vec(0.7, 0.1), vec(0.4, 0.1)])
    , polygon([vec(0.40, 0.24), vec(0.40, 0.46), vec(0.44, 0.50), vec(0.60, 0.50), vec(0.60, 0.20), vec(0.44, 0.20)])]).
shape(fish,
    [ curve([
            vec(0.116, 0.702),
            vec(0.260, 0.295),
            vec(0.330, 0.258),
            vec(0.815, 0.078)
        ]),
        curve([
            vec(0.564, 0.032),
            vec(0.730, 0.056),
            vec(0.834, 0.042),
            vec(1.000, 0.000)
        ]),
        curve([
            vec(0.250, 0.250),
            vec(0.372, 0.194),
            vec(0.452, 0.132),
            vec(0.564, 0.032)
        ]),
        curve([
            vec(0.000, 0.000),
            vec(0.110, 0.110),
            vec(0.175, 0.175),
            vec(0.250, 0.250)
        ]),
        curve([
            vec(-0.250, 0.250),
            vec(-0.150, 0.150),
            vec(-0.090, 0.090),
            vec(0.000, 0.000)
        ]),
        curve([
            vec(-0.250, 0.250),
            vec(-0.194, 0.372),
            vec(-0.132, 0.452),
            vec(-0.032, 0.564)
        ]),
        curve([
            vec(-0.032, 0.564),
            vec(0.055, 0.355), 
            vec(0.080, 0.330), 
            vec(0.250, 0.250)
        ]),
        curve([
            vec(-0.032, 0.564),
            vec(-0.056, 0.730),
            vec(-0.042, 0.834),
            vec(0.000, 1.000)
        ]),
        curve([
            vec(0.000, 1.000),
            vec(0.104, 0.938),
            vec(0.163, 0.893),
            vec(0.234, 0.798)
        ]),
        curve([
            vec(0.234, 0.798),
            vec(0.368, 0.650),
            vec(0.232, 0.540),
            vec(0.377, 0.377)
        ]),
        curve([
            vec(0.377, 0.377),
            vec(0.400, 0.350),
            vec(0.450, 0.300),
            vec(0.500, 0.250)
        ]),
        curve([
            vec(0.500, 0.250),
            vec(0.589, 0.217),
            vec(0.660, 0.208),
            vec(0.766, 0.202)
        ]),
        curve([
            vec(0.766, 0.202),
            vec(0.837, 0.107),
            vec(0.896, 0.062),
            vec(1.000, 0.000)
        ]),
        curve([
            vec(0.234, 0.798),
            vec(0.340, 0.792),
            vec(0.411, 0.783),
            vec(0.500, 0.750)
        ]),
        curve([
            vec(0.500, 0.750),
            vec(0.500, 0.625),
            vec(0.500, 0.575),
            vec(0.500, 0.500)
        ]),
        curve([
            vec(0.500, 0.500),
            vec(0.460, 0.460),
            vec(0.410, 0.410),
            vec(0.377, 0.377)
        ]),
        curve([
            vec(0.315, 0.710),
            vec(0.378, 0.732),
            vec(0.426, 0.726),
            vec(0.487, 0.692)
        ]),
        curve([
            vec(0.340, 0.605),
            vec(0.400, 0.642),
            vec(0.435, 0.647),
            vec(0.489, 0.626)
        ]),
        curve([
            vec(0.348, 0.502),
            vec(0.400, 0.564),
            vec(0.422, 0.568),
            vec(0.489, 0.563)
        ]),
        curve([
            vec(0.451, 0.418),
            vec(0.465, 0.400),
            vec(0.480, 0.385),
            vec(0.490, 0.381)
        ]),
        curve([
            vec(0.421, 0.388),
            vec(0.440, 0.350),
            vec(0.455, 0.335),
            vec(0.492, 0.325)
        ]),
        curve([
            vec(-0.170, 0.237),
            vec(-0.125, 0.355),
            vec(-0.065, 0.405),
            vec(0.002, 0.436)
        ]),
        curve([
            vec(-0.121, 0.188),
            vec(-0.060, 0.300),
            vec(-0.030, 0.330),
            vec(0.040, 0.375)
        ]),
        curve([
            vec(-0.058, 0.125),
            vec(-0.010, 0.240),
            vec(0.030, 0.280), 
            vec(0.100, 0.321)
        ]),
        curve([
            vec(-0.022, 0.063),
            vec(0.060, 0.200), 
            vec(0.100, 0.240), 
            vec(0.160, 0.282)
        ]),
        curve([
            vec(0.053, 0.658),
            vec(0.075, 0.677),
            vec(0.085, 0.687),
            vec(0.098, 0.700)
        ]),
        curve([
            vec(0.053, 0.658),
            vec(0.042, 0.710),
            vec(0.042, 0.760),
            vec(0.053, 0.819)
        ]),
        curve([
            vec(0.053, 0.819),
            vec(0.085, 0.812),
            vec(0.092, 0.752),
            vec(0.098, 0.700)
        ]),
        curve([
            vec(0.130, 0.718),
            vec(0.150, 0.730),
            vec(0.175, 0.745),
            vec(0.187, 0.752)
        ]),
        curve([
            vec(0.130, 0.718),
            vec(0.110, 0.795),
            vec(0.110, 0.810),
            vec(0.112, 0.845)
        ]),
        curve([
            vec(0.112, 0.845),
            vec(0.150, 0.805),
            vec(0.172, 0.780),
            vec(0.187, 0.752)
        ])
  ]).

/* ## Generate SVG
*/

defaultBound([500, 500]).

paint(Bound, Content) -->
    svg(["viewbox"=viewbox(Bound)], [
        group(["stroke"="black", "fill"="none", "transform"=transformation(Bound)], Content)
    ]).

viewbox([Width, Height]) -->
    { number_string(Width, W), number_string(Height, H)},
    "0 0 ", W, " ", H.

transformation([_, Height]) -->
    {number_string(Height, H)},
    "scale(1, -1) translate(0, -" , H, ")".

svg(Attributes, Content) -->
    node("svg", ["xmlns"="http://www.w3.org/2000/svg"|Attributes], Content).

group(Attributes, Content) -->
    node("g", Attributes, Content).

svg_polygon(Points) -->
    tag("polygon", ["points"=points(Points)]).

points([]) -->
    "".

points([vec(Px, Py)|Points]) -->
    {number_string(Px, X), number_string(Py, Y)},
    " ", X, ",", Y,
    points(Points).

node(Type, Attributes, Content) -->
    "<", Type, attributes(Attributes), ">",
    content(Content),
    "</", Type, ">".

attributes([]) -->
    "".

attributes([Key=Value|Attributes]) -->
    " ", Key, "=""", Value, """",
    attributes(Attributes).

content([]) -->
    "".

content([polygon(Points)|Rest]) -->
    svg_polygon(Points),
    content(Rest).

content([Content|Rest]) -->
    Content,
    content(Rest).

tag(Type, Attributes) -->
    "<", Type, attributes(Attributes), "/>".
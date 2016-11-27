%%% Antonio Vivace 793509

%% Parsing
as_polynomial(X, poly(O)) :-
  as_polynomial_p(X, P),
  norm_p(P, O).

as_monomial(X, NM) :-
  as_monomial_p(X, C, Vd),
  td(Vd, TD),
  norm_m(m(C, TD, Vd), NM).

as_polynomial_p(X + Y, [M | Ms]) :-
  as_monomial(Y, M),
  !,
  as_polynomial_p(X, Ms).

as_polynomial_p(X - Y, [M | Ms]) :-
  as_monomial_i(Y, M),
  !,
  as_polynomial_p(X, Ms).

as_polynomial_p(X, [M]) :-
  as_monomial(X, M).

as_monomial_i(X, m(OC, TD, Vs)) :-
  as_monomial_p(X, C, Vs),
  OC is -C,
  td(Vs, TD).

as_monomial_p(X, X, []) :-
  number(X),
  !.

as_monomial_p(X * Y, C, [V | Vs]) :-
  as_monomial_p(X, C, Vs),
  !,
  asvar(Y, V).

as_monomial_p(X, 1, [V]) :-
  asvar(X, V).

% FIXME variables/atom question
% asvar(_ ^ 0, []) :- !.
asvar(X ^ N, v(N, X)) :-
  integer(N),
  !,
  N >= 0.

asvar(X ^ N, v(N, X)) :-
  atom(N),
  !.

asvar(X, v(1, X)) :-
  atom(X).

td([v(N1, _) | Vs], N) :-
  td(Vs, N2),
  !,
  N is N1+N2.

td([v(N1, _)], N1).

td([], 0).

%% Normalization
norm_m(m(C, Td, []), m(C, Td, [])) :- !.
norm_m(m(0, _, _), m(0, 0, [])) :- !.
norm_m(m(C, 0, _), m(C, 0, [])) :- !.

norm_m(m(C, G, X), m(C, G, O)) :-
  sort(2, @=<, X, SX),
  norm_mm(SX, O).

norm_mm([v(C1, X) , v(C2, X) | Ms], MMs) :-
  !,
  C3 is C1+C2,
  norm_mm([v(C3,X) | Ms] , MMs).

norm_mm([v(C1, X), v(C2, Y) | Ms], [v(C1,X) | MMs]) :-
  !,
  X \= Y,
  norm_mm([v(C2,Y) | Ms], MMs).

norm_mm([v(C1, X)], [v(C1,X)]).

norm_p(Ms, OMs) :-
  sort(3, @=<, Ms, O),
  sort(2, @>=, O, Or),
  norm_pp(Or, OMs).

norm_pp([m(0, _, _), m(C2, S2, Y) | Ms], MMs) :-
  !,
  norm_pp([m(C2, S2, Y) | Ms], MMs).

norm_pp([m(C1, S1, X), m(C2, _, X) | Ms], MMs ) :-
  !,
  C3 is C1+C2,
  norm_pp([m(C3, S1, X) | Ms], MMs).

norm_pp([m(C1, S1, X), m(C2, S2, Y) | Ms], [m(C1, S1, X) | MMs]) :-
  !,
  X \= Y,
  norm_pp([m(C2, S2, Y) | Ms], MMs).

% norm_pp([m(0, _, _)], []) :- !.
norm_pp([m(C1, S1, X)], [m(C1, S1, X)]).

%% Checking
is_monomial(m(_, TD, VPs)) :-
  integer(TD),
  TD >= 0,
  is_list(VPs).

is_varpower(v(Power, VarSymbol)) :-
  integer(Power),
  Power >= 0,
  atom(VarSymbol).

is_polynomial(poly(Monomials)) :-
  is_list(Monomials),
  foreach(member(M, Monomials), is_monomial(M)).

%% Printing
pprint_polynomial(poly(L)) :-
  pprint_pp(L).

pprint_pp([]) :-
  !,
  write(0).

pprint_pp([m(A, B, C)]) :-
  !,
  pprint_m(m(A, B, C)).

pprint_pp([M | Ms]) :-
  !,
  pprint_m(M),
  write(' + '),
  pprint_pp(Ms).

pprint_m(m(1, _, L)) :-
  !,
  pprint_mm(L).
pprint_m(m(X, 0, [])) :-
  !,
  write(X).

pprint_m(m(X, _, L)) :-
  write(X),
  write(' * '),
  pprint_mm(L).

pprint_mm([L]) :-
  asvar(_, L),
  !,
  pprint_v(L).

pprint_mm([L | Ls]) :-
  !,
  pprint_v(L),
  write(' * '),
  pprint_mm(Ls).

pprint_v(v(1, Y)) :-
  !,
  upcase_atom(Y, Yc),
  write(Yc).

pprint_v(v(X, Y)) :-
  upcase_atom(Y, Yc),
  write(Yc),
  write('^'),
  write(X).

%% Operations
coefficients(poly(Ms), C) :-
  coefficients_l(Ms, C).
coefficients_l([], []) :- !.

coefficients_l([m(C, _, _)], [C]) :- !.

coefficients_l([m(C, _, _) | Ms], [C | Cs]) :-
  coefficients_l(Ms, Cs).

% FIXME variables should be lexicographically ordered
variables(poly(Ms), C) :-
  variables_m(Ms, Cd),
  list_to_set(Cd, C).

variables_m([m(_, _, [])], []) :- !.

variables_m([m(_, _, Vars)], V):-
  !,
  variables_vars(Vars, V).

variables_m([m(_, _, Vars) | Ms], R) :-
  variables_vars(Vars, V),
  variables_m(Ms, Vs),
  append(V, Vs, R).

variables_vars([v(_, N)], [N]) :- !.
variables_vars([v(_, N) | Vs], [N | Ns]) :-
  variables_vars(Vs, Ns).

% FIXME this output is acceptable?
monomials(poly(Ms), Ms).

maxdegree(poly([]), MinInf) :-
  !,
  MinInf is -1.

maxdegree(poly([m(_, MaxD, _) | _]), MaxD).

mindegree(poly(Ms), MinD) :-
  reverse(Ms, ([m(_, MinD, _) | _])).

mixdegree(poly([]), MinInf) :-
  !,
  MinInf is -1.

% polyplus accepts monomials too, as arguments.
polyplus(m(C, Td, Vars), m(C2, Td2, Vars), Result) :-
  !,
  polyplus(poly([m(C, Td, Vars)]), poly([m(C2, Td2, Vars)]), Result).

polyplus(m(C, Td, Vars), poly(P), Result) :-
  !,
  polyplus(poly([m(C, Td, Vars)]), poly(P), Result).

polyplus(poly(P), m(C, Td, Vars), Result) :-
  !,
  polyplus(poly(P), poly([m(C, Td, Vars)]), Result).

% 0 is neutral for +
polyplus(P1, poly([m(0, _, _)]), P1) :- !.
polyplus(poly([m(0, _, _)]), P1, P1) :- !.

polyplus(poly(P1), poly(P2), poly(P3)) :-
  append(P1, P2, P3o),
  norm_p(P3o, P3).

% polyminus accepts monomials too, as arguments.
polyminus(m(C, Td, Vars), m(C2, Td2, Vars), Result) :-
  !,
  polyminus(poly([m(C, Td, Vars)]), poly([m(C2, Td2, Vars)]), Result).

polyminus(m(C, Td, Vars), poly(P), Result) :-
  !,
  polyminus(poly([m(C, Td, Vars)]), poly(P), Result).

polyminus(poly(P), m(C, Td, Vars), Result) :-
  !,
  polyminus(poly(P), poly([m(C, Td, Vars)]), Result).

polyminus(P1, P2, P3) :-
  poly_i(P2, P2i),
  polyplus(P1, P2i, P3).

poly_i(poly(Ms), poly(IMs)) :-
  poly_ii(Ms, IMs).

poly_ii([ M | Ms], [OM | OMs] ) :-
  monomial_i(M, OM),
  poly_ii(Ms, OMs).

poly_ii([M], [Om]) :-
  monomial_i(M, Om).

monomial_i(m(C, G, Vars), m(Ci, G, Vars)) :-
  Ci is -C.

monotimes(m(C1, Td1, Vars1), m(C2, Td2, Vars2), m(C3, Td3, Vars3n)) :-
  C3 is C1 * C2,
  Td3 is Td1 + Td2,
  append(Vars1, Vars2, Vars3),
  norm_m(m(C3, Td3, Vars3), m(C3, Td3, Vars3n)).


% polytimes accepts monomials too, as arguments.
polytimes(m(C, Td, Vars), m(C2, Td2, Vars), Result) :-
  !,
  polytimes(poly([m(C, Td, Vars)]), poly([m(C2, Td2, Vars)]), Result).

polytimes(m(C, Td, Vars), poly(P), Result) :-
  !,
  polytimes(poly([m(C, Td, Vars)]), poly(P), Result).

polytimes(poly(P), m(C, Td, Vars), Result) :-
  !,
  polytimes(poly(P), poly([m(C, Td, Vars)]), Result).

% 0 is absorbing for *
polytimes(_, poly([m(0, _, _)]), poly([m(0, 0, [])])) :- !.
polytimes(poly([m(0,_, _)]), _, poly([m(0, 0, [])])) :- !.

% 1 is neutral for *
polytimes(A, poly([m(1, _, [])]), A) :- !.
polytimes(poly([m(1, _, [])]), A, A) :- !.

polytimes(poly(M1s), poly(M2s), poly(Result)) :-
  polytimes_m(M1s, M2s, Result).

polytimes_m(M1s, [m(C, Td, Vars)], [R]) :-
  !,
  polymono(M1s, m(C, Td, Vars), R).

polytimes_m(M1s, [M2 | M2s], [R | Rs]) :-
  polymono(M1s, M2, R),
  polytimes_m(M1s, M2s, Rs).

polymono([m(C1, Td1, Vars1)], m(C2, Td2, Vars2), R) :-
  !,
  monotimes(m(C1, Td1, Vars1), m(C2, Td2, Vars2), R).

polymono([M | Ms], M2, [R | Rs]) :-
  monotimes(M, M2, R),
  polymono(Ms, M2, Rs).

% TODO
polyval(Poly, Values, Result) :-
  with_output_to(string(PPoly), pprint_polynomial(Poly)),
  term_string(Term, PPoly, [variables(Values)]),
  Result is Term.

%%% Antonio Vivace 793509
%% Parsing
as_polynomial(X, poly(O)) :-
  as_polynomial_p(X, P),
  norm_p(P, O).

as_monomial(X, m(C, TD, Vs)) :-
  as_monomial_p(X, C, Vd),
  td(Vd, TD),
  norm_m(Vd, Vs).


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

% FIXME Variables with more that 1 chars names are legal?
% Current behaviour is ab^3 has ab as name =/= a*b^3
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
norm_m(X, O) :-
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
  sort(2, @>=, Ms, O),
  norm_pp(O, OMs).

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

norm_pp([m(0, _, _)], []) :- !.
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

coefficients_l([m(C, _, _)], [C]) :- !.

coefficients_l([m(C, _, _) | Ms], [C | Cs]) :-
  coefficients_l(Ms, Cs).

variables(poly(Ms), C) :-
  variables_m(Ms, Cd),
  list_to_set(Cd, C).       % Remove duplicates

variables_m([m(_, _, Vars)], [V]):-
  !,
  variables_vars(Vars, V).

variables_m([m(_, _, Vars) | Ms], [V | Vs]) :-
  variables_vars(Vars, V),
  variables_m(Ms, Vs).

variables_vars([v(_, N)], N) :- !.

variables_vars([v(_, N) | Vs], [N | Ns]) :-
  variables_vars(Vs, Ns).

% Is this acceptable as Output?
% Should we presume that the input is ordered? (e.g. generated with
% as_polynomials)
% 
% monomials(poly(Ms), C) :-
%  list_to_set(Ms, C).

monomials(poly(Ms), Ms).

maxdegree(poly([m(_, MaxD, _) | _]), MaxD).

mindegree(poly(Ms), MinD) :-
  reverse(Ms, ([m(_, MinD, _) | _])).

polyplus(poly(P1), poly(P2), poly(P3)) :-
  append(P1, P2, P3o),
  norm_p(P3o, P3).

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




% todo
polyminus(_).
polytimes(_).
polyval(_).















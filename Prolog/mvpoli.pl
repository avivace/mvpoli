%%%%% -*- Mode: Prolog -*-

%%%%% mvpoli.pl --
%%%%% 793307 Trovato Gaetano
%%%%% 793509 Vivace Antonio

%% TODO
%   ! Ordering (toString, 2 sorts?)
%
%% FIXME
%  - Polytimes still needs a cut. Unwanted backtracking (When?)
%  - Initial spaces in lists (polytimes results)

% PARSING %

%%%% as_polynomial(+Expression, -Poly)
%% True when poly(Monomials) unifies with the internal representation
%% of the normalised polynomial.
%
as_polynomial(Input, poly(Monomials)) :-
  as_polynomial_p(Input, RawMonomials),
  norm_p(RawMonomials, Monomials).

%%%% as_monomial(+Expression, -Monomial)
%% True when Monomial unifies with the internal representation
%% of the normalised monomial.
%
as_monomial(Input, Monomial) :-
  as_monomial_p(Input, C, Vd),
  td(Vd, TD),
  norm_m(m(C, TD, Vd), Monomial).

%%%% as_polynomial(+Expression, -Monomials)
%% True when Monomials unifies with the list of the Input
%% monomials.
%
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

%%%% as_monomial_i(-Mono1, +Mono2)
%% True when Mono1 unifies with the opposite of Mono2.
%
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
  as_var(Y, V).

as_monomial_p(X, 1, [V]) :-
  as_var(X, V).

%%%% as_var(+Expression, -Var)
%% True when Var unifies with v(N, X) where N is the exponent and
%% X the atom representing the variable.
%
as_var(X ^ N, v(N, X)) :-
  integer(N),
  !,
  N >= 0.

as_var(X ^ N, v(N, X)) :-
  atom(N),
  !.

as_var(X, v(1, X)) :-
  atom(X).

%%%% td(+VarList, -N)
%% True when N unifies with the sum of the grade of
%% every variable in VarList
%
td([v(N1, _) | Vs], N) :-
  td(Vs, N2),
  !,
  N is N1+N2.

td([v(N1, _)], N1).

td([], 0).

% NORMALISATION %

%%%% norm_m(+Mono1, -Mono2)
%% True when Mono2 unifies with Mono1 normalised.
%
norm_m(m(C, Td, []), m(C, Td, [])) :- !.
norm_m(m(0, _, _), m(0, 0, [])) :- !.
norm_m(m(C, 0, _), m(C, 0, [])) :- !.

norm_m(m(C, G, X), m(C, G, O)) :-
  sort(2, @=<, X, SX),
  norm_mm(SX, O).

%%%% norm_mm(+VarList1, -VarList2)
%% True when VarList2 unifies with VarList1 simplified.
%
norm_mm([v(C1, X) , v(C2, X) | Ms], MMs) :-
  !,
  C3 is C1+C2,
  norm_mm([v(C3,X) | Ms] , MMs).

norm_mm([v(C1, X), v(C2, Y) | Ms], [v(C1,X) | MMs]) :-
  !,
  X \= Y,
  norm_mm([v(C2,Y) | Ms], MMs).

norm_mm([v(C1, X)], [v(C1,X)]).

%%%% norm_p(+Monomials, -NMonomials)
%% True when NMonomials unifies with the list of normalised
%% Monomials.
%
norm_p(Monomials, NMonomials) :-
  sort(3, @=<, Monomials, FOMonomials),
  sort(2, @>=, FOMonomials, SOMonomials),
  norm_pp(SOMonomials, PNSOMonolias),
  norm_ms(PNSOMonolias, NMonomials).
  %sort(3, @<=, NMonomials, FOMonomials_1),
  %sort(2, @>=, FOMonomials_1, SOMonomials_1).


norm_ms([m(C, Td, Vars)], [NMonomial]) :-
  !,
  norm_m(m(C, Td, Vars), NMonomial).

norm_ms([m(C, Td, Vars) | Ms], [NMonomial | NMs]) :-
  norm_m(m(C, Td, Vars), NMonomial),
  norm_ms(Ms, NMs).

%%%% norm_pp(+Monomials, -SMonomials)
%% True when SMonomials unifies with the list of simplyfied Monomials.
%
norm_pp([m(C2, S2, Y), m(0, _, _) | Ms], MMs) :-
  !,
  norm_pp([m(C2, S2, Y) | Ms], MMs).

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

norm_pp([m(C1, S1, X)], [m(C1, S1, X)]).

% CHECKING %

%%%% is_monomial(+Monomial)
%% True when Monomial is a valid Monomial (internal representation).
%
is_monomial(m(_, TD, VPs)) :-
  integer(TD),
  TD >= 0,
  is_list(VPs).

%%%% is_varpower(+Var)
%% True when Var is a valid Variable (internal representation).
%
is_varpower(v(Power, VarSymbol)) :-
  integer(Power),
  Power >= 0,
  atom(VarSymbol).

%%%% is_polynomial(+Polynomial)
%% True when Polynomial is a valid Polynomial (internal representation).
%
is_polynomial(poly(Monomials)) :-
  is_list(Monomials),
  foreach(member(M, Monomials), is_monomial(M)).

% PRINTING %

%%%% pprint_polynomial(+Poly)
%% True when the Prolog interpreter succeds to print a human representation
%% of Poly.
%
pprint_polynomial(poly(L)) :-
  pprint_pp(L).

pprint_pp(m(0, _, _)) :-
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
  as_var(_, L),
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

% OPERATIONS %

%%%% coefficients(+Poly, -Cofficients)
%% True when Coefficients unifies with the list of every monomial coefficient
%% of Poly.
%
coefficients(poly(Ms), C) :-
  coefficients_l(Ms, C).

coefficients_l([], []) :- !.

coefficients_l([m(C, _, _)], [C]) :- !.

coefficients_l([m(C, _, _) | Ms], [C | Cs]) :-
  coefficients_l(Ms, Cs).

%%%% variables(+Poly, -Vars)
%% True when Vars unifies with the list of every Variable in Poly in a
%% lexicographical order. Duplicates are removed (the highest 
%% grade is kept).
% 
variables(P, Vars) :-
  variables_ao(P, AOVars),
  sort(0, @=<, AOVars, Vars).

%%%% variables_ao(+Poly, -Vars)
%% True when Vars unifies with the list of every Variable in Poly in the
%% appearing order. Duplicates are removed (the highest 
%% grade is kept).
% 
variables_ao(poly(Ms), Vars) :-
  variables_m(Ms, Cd),
  list_to_set(Cd, Vars).

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

%%%% monomials(+Poly, -Monomials)
%% True when Monomials unifies with the list of every monomail in Poly.
% 
monomials(poly(Ms), Ms).

%%%% maxdegree(+Poly, -MaxD)
%% True when MD unifies with the maximum degree of the monomials in Poly.
%
maxdegree(poly([m(0, _, _)]), 0).
maxdegree(poly([]), 0) :- !.

maxdegree(poly([m(_, MaxD, _) | _]), MaxD).

%%%% mindegree(+Poly, -MinD)
%% True when MD unifies with the minimum degree of the monomials in Poly.
%
mindegree(poly(Ms), MinD) :-
  reverse(Ms, ([m(_, MinD, _) | _])).

mindegree(poly([m(0, _, _)]), 0).
mindegree(poly([]), 0) :- !.

%%%% polyplus(+P1, +P2, -Sum)
%% True when Sum unifies with the polynomial sum of P1 and P2.
%

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

% 0 is neutral for polyplus (and polyminus)
polyplus(P1, poly([m(0, _, _)]), P1) :- !.
polyplus(poly([m(0, _, _)]), P1, P1) :- !.
polyplus(P1, poly([]), P1) :- !.
polyplus(poly([]), P1, P1) :- !.

polyplus(poly(P1), poly(P2), poly(P3)) :-
  append(P1, P2, P3o),
  norm_p(P3o, P3).

%%%% polyplus(+P1, +P2, -Diff)
%% True when Diff unifies with the polynomial difference of P1 and P2.
%

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

%%%% monotimes(+Mono1, +Mono2, -MonoProduct)
%% True when MonoProduct unifies with the monomial product 
%% of Mono1 and Mono2.
%
monotimes(m(C1, Td1, Vars1), m(C2, Td2, Vars2), m(C3, Td3, Vars3n)) :-
  C3 is C1 * C2,
  Td3 is Td1 + Td2,
  append(Vars1, Vars2, Vars3),
  norm_m(m(C3, Td3, Vars3), m(C3, Td3, Vars3n)).

%%%% polytimes(+Poly1, +Poly2, -PolyProduct)
%% True when PolyProduct unifies with the polynomial product 
%% of Poly1 and Poly2
%
% Polytimes accepts monomials too, as arguments.
polytimes(m(C, Td, Vars), m(C2, Td2, Vars), Result) :-
  !,
  polytimes(poly([m(C, Td, Vars)]), poly([m(C2, Td2, Vars)]), Result).

polytimes(m(C, Td, Vars), poly(P), Result) :-
  !,
  polytimes(poly([m(C, Td, Vars)]), poly(P), Result).

polytimes(poly(P), m(C, Td, Vars), Result) :-
  !,
  polytimes(poly(P), poly([m(C, Td, Vars)]), Result).

% 0 is absorbing for polytimes
polytimes(_, poly([m(0, _, _)]), poly([m(0, 0, [])])) :- !.
polytimes(poly([m(0,_, _)]), _, poly([m(0, 0, [])])) :- !.
polytimes(_, poly([]), poly([m(0, 0, [])])) :- !.
polytimes(poly([]), _, poly([m(0, 0, [])])) :- !.

% 1 is neutral for polytimes
polytimes(A, poly([m(1, _, [])]), A) :- !.
polytimes(poly([m(1, _, [])]), A, A) :- !.

polytimes(poly(M1s), poly(M2s), poly(ResultN)) :-
  polytimes_m(M1s, M2s, Result),
  flatten(Result, ResultF),
  norm_p(ResultF, ResultN).

polytimes_m(M1s, [m(C, Td, Vars)], R) :-
  !,
  polymono(M1s, m(C, Td, Vars), R).

polytimes_m(M1s, [M2 | M2s], [R | Rs]) :-
  polymono(M1s, M2, R),
  polytimes_m(M1s, M2s, Rs).

%%%% polymono(+Poly, +Mono, -PMProduct)
%% True when PMProduct unifies with the polynomial product of 
%% polynomial Poly and monomial Mono.
%
polymono([m(C1, Td1, Vars1)], m(C2, Td2, Vars2), R) :-
  !,
  monotimes(m(C1, Td1, Vars1), m(C2, Td2, Vars2), R).

polymono([M | Ms], M2, [R | Rs]) :-
  monotimes(M, M2, R),
  polymono(Ms, M2, Rs).


%%%% polyval(+Poly, +Values, -Result)
%% True when Result unifies with the computation of Poly using Values
%% for the variables appearing in Poly.
%% Values is a list of integers associating with the list variables
%% (lexicographical order).
%
polyval(Poly, InputValues, Result) :-
  vvList(Poly, InputValues, VVList),
  variables_ao(Poly, AOVars),
  getVValues(VVList, AOVars, AOVVars),
  stripValues(AOVVars, AOInputValues),
  with_output_to(string(PPoly), pprint_polynomial(Poly)),
  term_string(Term, PPoly, [variables(AOInputValues)]),
  Result is Term.

%%%% vvList(+Poly, +Values, -VarValues)
%% True when VarValues unifies with a list of (Var, Value) pairs.
%
vvList(Poly, Values, VarValues) :-
  variables(Poly, Vars),
  vvList_m(Vars, Values, VarValues).

vvList_m([Var], [Value], [(Var, Value)]) :-
  atom(Var),
  !.

vvList_m([Var | Vars], [Value | Values], [(Var, Value) | VarValues]) :-
  vvList_m(Vars, Values, VarValues).

%%%% getVValues(+VVList, +Variables, -ReorderedVVList)
%% True when ReorderedVVList unifies with a reordered version of VVList,
%% where VVList is a list of (Var, Value) pairs.
% 
getVValues(VVList, [Var], [(Var, Value)]) :-
  atom(Var),
  !,
  getValue(Var, VVList, Value).

getVValues(VVList, [Var | AOVars], [(Var, Value) | AOVVars]) :-
  getValue(Var, VVList, Value),
  getVValues(VVList, AOVars, AOVVars).

getValue(Var, [(Var, Value) | _], Value) :- !.
getValue(Var, [(_, _) | VVList], Value) :-
  getValue(Var, VVList, Value).

stripValues([(_, Value)], [Value]) :- !.
stripValues([(_, Value) | VValues], [Value | Values]) :-
  stripValues(VValues, Values).

%%%%% end of file -- mvpoli.pl --
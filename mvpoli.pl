%%% Antonio Vivace 793509

% PARSING %

%%%% as_polynomial/2
%% True when poly(Monomials) unifies with the internal representation
%% of the normalised polynomial.
%
as_polynomial(Input, poly(Monomials)) :-
  as_polynomial_p(Input, RawMonomials),
  norm_p(RawMonomials, Monomials).

%%%% as_monomial/2
%% True when Monomial unifies with the internal representation
%% of the normalised monomial.
%
as_monomial(Input, Monomial) :-
  as_monomial_p(Input, C, Vd),
  td(Vd, TD),
  norm_m(m(C, TD, Vd), Monomial).

%%%% as_polynomial/2
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


%%%% as_monomial_i/2
%%%% as_monomial_i(-Mono1, +Mono2)
%% True when Mono1 unifies with the opposite of Mono2.
%
as_monomial_i(X, m(OC, TD, Vs)) :-
  as_monomial_p(X, C, Vs),
  OC is -C,
  td(Vs, TD).

%%%% as_monomial/2
%% True when Monomial unifies with the internal representation
%% of the monomial.
%
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
%%%% asvar/2
%%%% asvar(+Expression, -Var)
%% True when Var unifies with v(N, X) where N is the exponent and
%% X the atom representing the variable.
%
asvar(X ^ N, v(N, X)) :-
  integer(N),
  !,
  N >= 0.

asvar(X ^ N, v(N, X)) :-
  atom(N),
  !.

asvar(X, v(1, X)) :-
  atom(X).

%%%% td/2
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

%%%% norm_m/2
%%%% norm_m(+Mono1, -Mono2)
%% True when Mono2 unifies with Mono1 normalised.
%% The monomial normalisation consists in simplifying the monomial
%% and reordering its variables in a lexicographical order.
%
norm_m(m(C, Td, []), m(C, Td, [])) :- !.
norm_m(m(0, _, _), m(0, 0, [])) :- !.
norm_m(m(C, 0, _), m(C, 0, [])) :- !.

norm_m(m(C, G, X), m(C, G, O)) :-
  sort(2, @=<, X, SX),
  norm_mm(SX, O).

%%%% norm_mm/2
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

%%%% norm_p/2
%%%% norm_p(+Monomials, -NMonomials)
%% True when NMonomials unifies with the list of normalised
%% Monomials.
%% Normalization consists in removing monomials with coefficient 0,
%% simplifying similar monomials, and reordering them by descendant grade.
%% When monomials have the same total degree, they should be discriminated
%% by lexicographical order.
%
norm_p(Ms, OMs) :-
  sort(3, @=<, Ms, O),
  sort(2, @>=, O, Or),
  norm_pp(Or, OMs).

%%%% norm_pp/2
%%%% norm_pp(+Monomials, -SMonomials)
%% True when SMonomials unifies with the list of simplyfied Monomials.
%
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

%%%% is_monomial/1
%%%% is_monomial(Monomial)
%% True when Monomial is a valid Monomial (internal representation).
%
is_monomial(m(_, TD, VPs)) :-
  integer(TD),
  TD >= 0,
  is_list(VPs).

%%%% is_varpower/1
%%%% is_varpower(Var)
%% True when Var is a valid Variable (internal representation).
%
is_varpower(v(Power, VarSymbol)) :-
  integer(Power),
  Power >= 0,
  atom(VarSymbol).

%%%% is_polynomial/1
%%%% is_polynomial(Polynomial)
%% True when Polynomial is a valid Polynomial (internal representation).
%
is_polynomial(poly(Monomials)) :-
  is_list(Monomials),
  foreach(member(M, Monomials), is_monomial(M)).

% PRINTING %

%%%% pprint_polynomial/1
%%%% pprint_polynomial(Poly)
%% True when the Prolog interpreter succeds to print a human representation
%% of Poly.
%
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

% OPERATIONS %

coefficients(poly(Ms), C) :-
  coefficients_l(Ms, C).
coefficients_l([], []) :- !.

coefficients_l([m(C, _, _)], [C]) :- !.

coefficients_l([m(C, _, _) | Ms], [C | Cs]) :-
  coefficients_l(Ms, Cs).

% Lexicographically ordered variables
variables(P, Vars) :-
  variables_ao(P, AOVars),
  sort(0, @=<, AOVars, Vars).

% Appearing ordered variables
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

% 0 is neutral for polyplus (and polyminus)
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

% 1 is neutral for polytimes
polytimes(A, poly([m(1, _, [])]), A) :- !.
polytimes(poly([m(1, _, [])]), A, A) :- !.

polytimes(poly(M1s), poly(M2s), poly(ResultF)) :-
  polytimes_m(M1s, M2s, Result),
  flatten(Result, ResultF).

polytimes_m(M1s, [m(C, Td, Vars)], R) :-
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

polyval(Poly, InputValues, Result) :-
  vvList(Poly, InputValues, VVList),
  variables_ao(Poly, AOVars),
  getVValues(VVList, AOVars, AOVVars),
  stripValues(AOVVars, AOInputValues),
  with_output_to(string(PPoly), pprint_polynomial(Poly)),
  term_string(Term, PPoly, [variables(AOInputValues)]),
  Result is Term.

vvList(Poly, Values, VarValues) :-
  variables(Poly, Vars),
  vvList_m(Vars, Values, VarValues).

vvList_m([Var], [Value], [(Var, Value)]) :-
  atom(Var),
  !.

vvList_m([Var | Vars], [Value | Values], [(Var, Value) | VarValues]) :-
  vvList_m(Vars, Values, VarValues).

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
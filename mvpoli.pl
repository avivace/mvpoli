%%% Antonio Vivace 793509
%% Parsing
as_polynomial(X, poly(O)) :-
  as_polynomial_p(X, P),
  sort(2, @>=, P, O).

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

as_monomial(X, m(C, TD, Vs)) :-
  as_monomial_p(X, C, Vd),
  td(Vd, TD),
  sort(2, @=<, Vd, Vs).

as_monomial_p(X, X, []) :-
  number(X),
  !.

as_monomial_p(X * Y, C, [V | Vs]) :-
  as_monomial_p(X, C, Vs),
  !,
  asvar(Y, V).

as_monomial_p(X, 1, [V]) :-
  asvar(X, V).

% Variables with more that 1 chars names are legal?
% Current behaviour is ab^3 has ab as name =/= a*b^3
asvar(X ^ N, v(N, X)) :-
  % atom(X)
  integer(N),
  !,
  N >= 0.

asvar(X ^ N, v(N, X)) :-
  % atom(X),
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

% todo
polyplus(_).
polyminus(_).
polytimes(_).
polyval(_).















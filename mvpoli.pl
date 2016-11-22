% 5 * x ^ 6: * y ^ 3
% as_monomial(C, X) :- C \= X, atomic(X).

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

pprint_m(m(X, Y, L)) :-
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

test2([X | Y], X, Y).
% test(X * Y, X, Y).
%sortm(m(X, Y, Vars), m(X, Y, Ordered)) :-
% sort(2, @=<, Vars, Ordered).

% coefficients(poly([X | Xs]), X).

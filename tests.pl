-? as_polynomial(4 * xy ^ 2 + 9 *y^10, X),
pprint_polynomial(X).


poly(0)


as_polynomial(7 * x ^ 3 * k^9 + k^9 + x^2 - 7 * x ^ 3 + 7 * x ^ 3, X),
  pprint_polynomial(X).


as_polynomial(0 * z - 0 * x ^ 2 + 1 * x, X),
pprint_polynomial(X),
polyplus(X,X, R),
write("  STAMPO RISULTATO EHEHEHE XD  "),
pprint_polynomial(R).



as_polynomial(5 * x - 5 * x  + y, X),
as_polynomial(z - y, Y),
write("X:"),
pprint_polynomial(X),
write("   Y:"),
pprint_polynomial(Y),
polyminus(X, Y, Result),
write("    X-Y:"),
pprint_polynomial(Result),
polyplus(X, Y, ResultS),
write("    X+Y:"),
pprint_polynomial(ResultS).


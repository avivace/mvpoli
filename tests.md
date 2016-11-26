# Tests

#### Polyval
Query
```
?- as_polynomial(5*x^2*y^3 + y + z^5 + j^2 + a^6 - 42*z^3*a, P),
variables(P, Vars),
pprint_polynomial(P),
polyval(P, [42, 2, 1, 22, 7], Val).
```

Expected Result
```
A^6 + 5 * X^2 * Y^3 + Z^5 + -42 * A * Z^3 + J^2 + Y
P = poly([m(1, 6, [v(6, a)]), m(5, 5, [v(2, x), v(3, y)]), m(1, 5, [v(5, z)]), m(-42, 4, [v(1, a), v(3, z)]), m(1, 2, [v(2, j)]), m(1, 1, [v(..., ...)])]),
Vars = [a, x, y, z, j],
Val = 5475402374.
```

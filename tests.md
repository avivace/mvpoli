# Tests
## Prolog
#### Polytimes
Query
```
?- as_polynomial(x^2 + a + k^4 * c, P1),
vvList(P1, InputList, VVList),
variables_ao(P1, AOVars),
getVValues(VVList, AOVars, ReorderedVVList),
stripValues(ReorderedVVList, FinalAOValues),
InputList = [10, 20, 30, 40],
variables(P1, LGOVars).
```

Expected Result
```
P1 = poly([m(1, 5, [v(1, c), v(4, k)]), m(1, 2, [v(2, x)]), m(1, 1, [v(1, a)])]),
InputList = [10, 20, 30, 40],
VVList = [ (a, 10), (c, 20), (k, 30), (x, 40)],
AOVars = [c, k, x, a],
ReorderedVVList = [ (c, 20), (k, 30), (x, 40), (a, 10)],
FinalAOValues = [20, 30, 40, 10],
LGOVars = [a, c, k, x].
```

Query
```
polytimes 5 * x + a X another long poly gives false
```

Expected Result
```
-
```


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

## Lisp

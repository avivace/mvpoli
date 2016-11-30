# Testing
## Known Problems
#### Prolog
  - Polytimes still needs a cut. Unwanted backtracking (When?)
  - Initial spaces in lists (polytimes results)
  - Variables/atom question. Current situation: `a*a =/= aa` and both accepted as variables.

#### LISP


## Tests

#### Test 1
```
QUERY
poly = 5 * x + y ^ 3 - k*x*x*x
polyval in [90, 80, 40]

RESULTS
- 1 * X ^ 3 * K + Y ^ 3 + 5 * X
Polyval = -46015600.

```

#### Test 2
```
QUERY
poly1 = x * x + x * y + a * y  
poly2 = 3 * y + x * x
polyminus(poly1,poly2)

RESULTS
polyminus = a * y + x * y - 3 * y

```

#### Test 3
```
QUERY
poly1 = x * x + x * y + a * y  
poly2 = 3 * y + x * x
polyplus(poly1, poly2)

RESULTS
Result = a * y + 2 * x ^ 2 + x * y + 3 * y
```

#### Test 4
```
QUERY
poly1 = 5 + 3 + 0 * y - 6*z*a^3
poly2 = u^10 + i + 14
polytimes(poly1, poly2)

RESULT
-6 * A^3 * U^10 * Z + 8 * U^10 + -6 * A^3 * I * Z + 8 * I + -84 * A^3 * Z + 112
```

#### Test 5
```
QUERY
poly1 = x + y
polytimes(poly1, poly1)

RESULT
2 * X * Y + X^2 + Y^2
```

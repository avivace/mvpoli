# Tests

#### 1 tofix
```
QUERY
poly = 5 * x + y ^ 3 - k*x*x*x
polyval in [90, 80, 40]

RESULTS
- 1 * X ^ 3 * K + Y ^ 3 + 5 * X
Polyval = -46015600.

```

#### 2 tofix
```
QUERY
poly1 = x * x + x * y + a * y  
poly2 = 3 * y + x * x
polyminus(poly1,poly2)

RESULTS
polyminus = a * y + x * y - 3 * y

```

#### 3 tofix
```
QUERY
poly1 = x * x + x * y + a * y  
poly2 = 3 * y + x * x
polyplus(poly1, poly2)

RESULTS
Result = a * y + 2 * x ^ 2 + x * y + 3 * y
```

#### 4 tofix
```
QUERY
poly1 = 5 + 3 + 0 * y - 6*z*a^3
poly2 = u^10 + i + 14
polytimes(poly1, poly2)

RESULT
-6 * A^3 * U^10 * Z + 8 * U^10 + -6 * A^3 * I * Z + -84 * A^3 * Z + 8 * I + 112
-6 * A^3 * U^10 * Z + 8 * U^10 + -6 * A^3 * I * Z + -84 * A^3 * Z + 8 * I + 112 Lisp result
```

#### 5 tofix
```
QUERY
poly1 = x + y
polytimes(poly1, poly1)

RESULT
2 * X * Y + X^2 + + Y^2
X^2 + 2 * X * Y + Y^2 Lisp result
```

#### 6 tofix
```
QUERY
poly1 = x^3 + x^2 + x*y^3 - 2*x^2 - x^3 + 5*x*y^3 + o + l
polytimes(poly1, poly1)

RESULT
36 * X^2 * Y^6 + -12 * X^3 * Y^3 + 12 * L * X * Y^3 + 12 * O * X * Y^3 + X^4 + -2 * L * X^2 + -2 * O * X^2 + 2 * L * O + L^2 + O^2
```

#### 7
```
QUERY
poly1 = x^3 + x^2 + x*y^3 - 2*x^2 - x^3 + 5*x*y^3 + o + l
polytimes(poly1, poly1)

RESULT
L^2 + 2 * L * O + O^2 + -2 * L * X^2 + -2 * O * X^2 + X^4 + 12 * L * X * Y^3 + 12 * O * X * Y^3 + -12 * X^3 *Y^3 + 36 * X^2 * Y^6
```

#### 8
```
QUERY
(n+1)^3

RESULT
1 + 3 * N + 3 * N^2 + N^3
```

#### 9
```
QUERY
as_polynomial(x-4, P),
polytimes(P,P, R1),
polytimes(R1,P, R),
pprint_polynomial(R).

RESULT
-64 + 48 * X + -12 * X^2 + X^3
```

#### 10, pdf example on sorting
```
QUERY
as_polynomial(y^42*x^4*s*z^2*t^2,P),
pprint_polynomial(P).

RESULT
S * T^2 * X^4 * Y^42 * Z^2
```

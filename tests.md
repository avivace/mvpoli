<<<<<<< HEAD
# Prolog Tests

## Coefficients
#### Query
```
coefficients(y^0, C1),
coefficients(0 * x -3, C2),
coefficients(-y * x + y, C3),
coefficients(0 * x, C4),
coefficients(poly([m(-3, 0, []), m(-4, 1, [v(1, x)])]), C5),
coefficients(m(-1, 1, [v(1, x)]), C6),
coefficients(m(1, 0, []), C7).
```
#### Results
```
C1 = [1]
C2 = [-3]
C3 = [-1, 1]
C4 = [] 
C5 = [-3, -4]
C6 = [-1]
C7 = [1]
```

## Variables
#### Query
```
variables(x * y * z, V1),
variables(0 * x * y, V2),
variables(x^0, V3),
variables(x + y ^2, V4),
variables(3 * b -a, V5),
variables(m(1, 0, []), V6),
variables(poly([m(-3, 0, []), m(-4, 1, [v(1, x)])]), V7).
```
#### Results
```
V1 = [x, y, z]
V3 = V2 = []
V4 = [x, y]
V5 = [a, b]
V6 = []
V7 = [x]
```

## Monomials
#### Query
```
monomials(x * y + 3, M1),
monomials(0 * x -y, M2),
monomials(-pippo^123 * pluto, M3),
monomials(x + x + x + x, M4),
monomials(x + x + x + 0 * x, M5). 
```
#### Results
```
M1 = [m(3, 0, []), m(1, 2, [v(1, x), v(1, y)])]
M2 = [m(-1, 1, [v(1, y)])]
M3 = [m(-1, 124, [v(123, pippo), v(1, pluto)])]
M4 = [m(4, 1, [v(1, x)])]
M5 = [m(3, 1, [v(1, x)])]
```

## Maxdegree
#### Query
```
maxdegree(x ^ 0, MAX1),
maxdegree(pippo ^ 3 * pippo, MAX2),
maxdegree(a ^ 3 -b + ciao^2, MAX3),
maxdegree(-a -b ^ 7 -c^2, MAX4),
maxdegree(-b -b ^ 7 -b^2, MAX5).
```

#### Results
```
MAX1 = 0
MAX2 = 4
MAX3 = 3
MAX4 = 7
MAX5 = 7 
```

#### 1
```
QUERY
poly = 5 * x + y ^ 3 - k*x*x*x
polyval in [90, 80, 40]

RESULTS
5 * X + Y^3 + -K * X^3
polyval = -46015600.

```

#### 2
```
QUERY
poly1 = x * x + x * y + a * y  
poly2 = 3 * y + x * x
polyminus(poly1,poly2)

RESULTS
polyminus = -X + -3 * Y + A * Y + X^3 * Y


#### 3
```
QUERY
poly1 = x * x + x * y + a * y  
poly2 = 3 * y + x * x
polyplus(poly1, poly2)

RESULTS
Result = 3 * Y + A * Y + 2 * X^2 + X * Y
```

#### 4
```
QUERY
poly1 = 5 + 3 + 0 * y - 6*z*a^3
poly2 = u^10 + i + 14
polytimes(poly1, poly2)

RESULT
112 + 8 * I + -84 * A^3 * Z + -6 * A^3 * I * Z + 8 * U^10 + -6 * A^3 * U^10 * Z
```

#### 5
```
QUERY
poly1 = x + y
polytimes(poly1, poly1)

RESULT
X^2 + 2 * X * Y + Y^2
```

#### 6
```
QUERY
as_polynomial(n^2 - l^54 + 2 * x * y + y^2 - x^3 + x^2 + x*y^3 - 2*x^2 - x^3 + 5*x*y^3 + o + l, P),
polytimes(P, P, R),
pprint_polynomial(R).

RESULT
L^2 + 2 * L * O + O^2 + 2 * L * N^2 + -2 * L * X^2 + 4 * L * X * Y + 2 * L * Y^2 + 2 * N^2 * O + -2 * O * X^2 + 4 * O * X * Y + 2 * O * Y^2 + -4 * L * X^3 + N^4 + -2 * N^2 * X^2 + 4 * N^2 * X * Y + 2 * N^2* Y^2 + -4 * O * X^3 + X^4 + 2 * X * Y^3 + -X^2 * Y^2 + 2 * X * Y^3 + 4 * X^2 * Y^2 + -2 * X^3 * Y + -X^2 * Y^2 + -2 * X^3 * Y + Y^4 + 12 * L * X * Y^3 + -4 * N^2 * X^3 + 12 * O * X * Y^3 + 4 * X^5 + -2 * X^3 * Y^2 + -4 * X^4 * Y + -2 * X^3 * Y^2 + -4 * X^4 * Y + 12 * N^2 * X * Y^3 + 4 * X^6 + 6 * X * Y^5 +12 * X^2 * Y^4 + -6 * X^3 * Y^3 + 6 * X * Y^5 + 12 * X^2 * Y^4 + -6 * X^3 * Y^3 + -24 * X^4 * Y^3 + 36* X^2 * Y^6 + -2 * L^55 + -2 * L^54 * O + -2 * L^54 * N^2 + 2 * L^54 * X^2 + -4 * L^54 * X * Y + -2 * L^54 * Y^2 + 4 * L^54 * X^3 + -12 * L^54 * X * Y^3 + L^108
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

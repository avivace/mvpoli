# Testing
## Known Problems
#### Prolog
  - Polytimes still needs a cut. Unwanted backtracking (When?)

#### LISP


## Tests

#### Test 1
Query
```
5 * x + y ^ 3 - k*x*x*x

polyval in [90, 80, 40]
```

Expected Result
```
- 1 * X ^ 3 * K + Y ^ 3 + 5 * X
Result = -46015600.

```

#### Test 2
Query
```

poly1 = x * x + x * y + a * y  
poly2 = 3 * y + x * x

polyminus: poly1 - poly2
```

Expected Result
```
Result = a * y + x * y - 3 * y

```

#### Test 3
Query
```
poly1 = x * x + x * y + a * y  
poly2 = 3 * y + x * x

polyplus: poly1 + poly2
```


Expected Result
```
Result = a * y + 2 * x ^ 2 + x * y + 3 * y

```


#### Test 4
Query
```
            QUERY
```
Expected Result
```
            EXPECTED RESULT

```

#### Test 5
Query
```
            QUERY
```
Expected Result
```
            EXPECTED RESULT

```
#### Test 6
Query
```
            QUERY
```
Expected Result
```
            EXPECTED RESULT

```
# Multivariate Polynomials - LP - 201701
Antonio Vivace

*TODO*
  - Fix Variables
  - polyval


## polyval

*the input list problem*
  (n1, n2, n3, ...)
    into
  ([n1, v1], [n2, v2], [n3, v3], ...)
    Where v1, v2, v3, ... are the variables predicate output.

*solution 1*
  The pprint-polynomial output is a correct (and valuable expression).
  Calling it with `Result is Output` and unification for every variable should
  work.
  pprint-polynomial has not a proper output, just writing it to the output
  

*solution 2*

  - Prepare to get some gonorrea in 3-4 innested recursions
  - Evaluate every monomial passing the VarValues and sum the results
  - Evaluate variables and multiply with the Coefficient
  - Scan the variables and pass each of them to evaluate variable
  - Scan the VarValues for the Value of the current variable and pass
      Value ^ Exp.
  - Honestly just die

polyval(poly(), VarsValues, Result)
  Check if VarValues is a list of numbers with the same number of elements of
  variables.
  Build a list of couples of elements [(VAR, VALUE), (VAR, VALUE), ... ]
  Given a list of Monomials, pass them to monoval.
  The sum of these results is Result.
  
monoval(Monomial, VarsValues, Result)
  Given a Monomial, pass the list of variable to varsvalue and multiply
  its value to the Coefficient.

varsval(Variables, VarsValues, Result)
  Given a list of variables, get the value of every variable 
  and multiply them.

varval(v(Exp, Variable), VarsValues, Result) :-
  getVarValue(VarsValue, Variable, VValue),
  Result is VValue ^ Exp.

getVarValue(VarsValues, Variable, Value)
  Scan the VarsValue list for the Variable Value.
  Returns the Value.

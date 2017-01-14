
---- Utilities functions ----

(flatten list): returns a single level-one list


---- Ispetioning monomials ----

(monomial-degree monomial) : returns the monomial total degree

(monomial-coeffcient monomial): returns the monomial coefficient

(varpowers monomial): returns the list of variables and related powers of the monomial

(var-of monomial): returns the list of variables of the monomial, it uses 
		   varpowers and var-of-helper

(var-of-helper varpower): returns the variable of a structured varpower


-----  Ispetioning polynomials ----

(monomials polynomial): returns the list of monomials which composes the polynomial

(variables polynomial): returns the list of variables that appears in the polynomial without any duplicates, 
flattened and lessycografically ordered. It uses variables-helpers to iterate on the list of monomials 

(variables-helper monomials): creates the list of variables of a polynomial applying var-of on the list of polynomial's monomials

(coefficients polynomial): returns the list of monomials' coefficients that compose the flattened polynomial. 
It uses coefficients-helpers to iterate the list of monomials 

(coefficients-helper monomials): creates the list of coefficients of a polynomial applying monomial-coefficients on 
the list of polynomial's monomials

(maxdegree polynomial): returns the maximum degree of the polynomial, it uses poly-degree

(mindegree polynomial): returns the minimum degree of the polynomial, it uses poly-degree

(poly-degrees monomials): creates a list of monomials' coefficients of the polynomial applying monomial-degree on the list
			  of polynomial monomials


---- Parsing of monomial ----

(as-monomial input): takes the input as "(* coefficient? variables*)" and returns a structured monomial like 
(M coefficient total-degree ((V var-degree var-simbol)*) ), it relies on the monomial-helper to build the list of structured variables 
and on m-norm to normalize and lessicografically sort the variables.

(m-norm variables-list): sorts lessicografically the monomial variables' list and uses m-normalizer to normalize it after the sort

(m-normalizer variables-list): normalizes the monomial variables list adding up similar variables


---- Parsing the polynomial ----


(as-polynomial input): takes an input like "(+ unstructured-monomials+)" or a structured monomial or an unstructured monomial and 
returns a structured polynomial like (P (structured-monomial+) ). It relies on as-monomial if the input is an unstructured monomial, 
on as-polynomial-helper to create a list of structured polynomials and on p-norm to sort the list by monomial's grade
		       
(as-polynomial-helper input): creates a list of structured monomials applying as-monomial on the input list

(p-norm monomials): sorts the list of normalized monomials by monomial-degree and uses p-norm-helper to obtain the normalized list

(p-norm-helper monomials): takes a list of monomials already lessicografically sorted and normalizes it adding up similar monomials

(vps monomial1 monomial2): sorts monomials by variables' lessicografically order


---- Polynomials' operations ----

(polyval polynomial values-list): evaluates a polynomial in a point of given coordinates. It can accept a structured or an unstructured polynomial.
It uses as-polynomial if the input is not a structured polynomial and polyval-helper1 to evaluate

(polyval-helper1 polynomial values-list): prepares the arguments for polyval-helper2. It uses var-values to retrieve a var-values-list

(polyval-helper2 polynomial var-values-list): applies monomialval to the monomials' list of the polynomial

(monomialval monomial var-values-list): multiplies the monomial evaluated for the monomial-coefficient. 
It uses monomialval-helper to evaluate the monomial

(mononomialval-helper monomial var-values-list accumulator): evaluates each variable of the monomial and stores the results in an accumulator
				      
(find-val variable var-values-list): finds the value of the variables in the var-values-list

(var-values variables values): creates a list of pairs of the polynomial's variables and the values

(polyplus polynomial1 polynomial2): takes two structured or not polynomials and add up each other, appending polynomial1 monomials to polynomial2
monomials and normalizing the result with p-norm

(polyminus polynomial1 polynomial2): takes two structured or not polynomials and subtracts polynomial2 from polynomial1. 
At first, it changes the sign of monomials' coefficiets of polynomial2 then it appends polynomial1 monomials to polynomial2 monomials and normalizes the result with p-norm

(polytimes polynomial1 polynomial2): takes two structured or not polynomials and multiplies them. 
It uses polytimes-helper1 and p-norm to normalize the final polynomial

(polytimes-helper1 monomials1 monomials2): creates the list of monomials from the result of the multiplication of monomials1 and monomials2

(polytimes-helper2 monomial monomials): multiplies the monomial for the monomials' list adjusting the coefficients and the degree of the final monomial






	as_valid/2
This predicate is used on *every* operation, preparsing every argument.
Every predicate simply calls as_valid on every argument, then calls
PREDICATENAME_v (the 'real' one) with valid arguments.
This allows every operation to accept an Expression, a Poly or a Monomial,
in the same way.

	norm_m/2
Monomial normalisation consists in simplifying the monomial
and reordering its variables in a lexicographical order.

	norm_p/2
Polynomial normalization consists in removing monomials with coefficient 0,
simplifying similar monomials, and reordering them by ascendant grade.
When monomials have the same total degree, they should be discriminated
by lexicographical order. Note that a last monomial normalisation is
needed for the cases in which new monomials appears from simplifying.

	polyval/2
Valutation of the polynomial with the given values for the variables is done 
using the term_string/3 predicate. with_output_to is used to 'print' the the 
result of pprint_polynomial (which happens to be a well formed expression that
can be valued with is, if you replace variable names with values) into a 
string.
term_string/3 allows passing Options to either read_term/2 or write_term/2, 
we pass Variables to read_term, making term_string/3 unify the Vars with a 
list of variables in the term.
Note that term_tring/3 reads the variables values in the *appearing order* 
(the same behiaviour is on variable_names) so we first 'map' the input values 
to the corresponding variables in alphabetical order, then 'remap' this list
to obey the appearing order and strip the names, finishing with a list of 
mapped values of variable in the appearing order.
Having the final string with the input values replacing the values, 
we can evaluate the whole thing with 'is'.
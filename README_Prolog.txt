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
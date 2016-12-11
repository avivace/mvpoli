# Multivariate polynomials
- **Variables/atom question**. Current situation: `a*a =/= aa` and both accepted as variables.
- **The Zero Polynomial/Monomial**. `0` as monomial is `m(0, 0, []`. `0` as polynomial is `p([m(0, 0, [])])` but appears if and only if it's the only monomial in the polynomial (5 * X + 0 = 5 * X).
- **The Zero Polynomial (2)** `poly([])` should be accepted too, as 0 but *when*? The internal representation of 0 is `p([m(0, 0, [])])` and they can't really coexists. Maybe `polytimes`, `polyminus` and `polyplus` *should* accept this notation, too? (not much sense since they should accept an `as_` predicate result).
- `maxdegree(poly([]), 0).`  **true.**
- `as_monomial(x^0,M)` is **different** from `as_monomial(x^3 * y^0 * z^2`. Reconsider semplifications (`^0`, `*0`) and adopt some kind of coherent behaviour? (thread on forum with meaningful cases?)

Peano Arithmetic
====================

A version of number theory for the natural numbers, dependent on the Peano's
following axioms:

 1. Zero is a number
 2. If `a` is a number, the successor of `a` is a number. ie: `forall m n. m == n iif S(m) == S(n)`; `S` is called an *injection*
 3. zero is not the successor of a number
 4. two numbers of which the successors are equal are, themselves, equal
 5. If a set `S` of numbers contains zero and also the successor of every number 
    in `S`, then every number is in `S`
 6. equality of natural numbers is reflexive: `forall x. x == x`
 7. equality of natural numbers is symmetric: `forall x y. if x == y then y == x`
 8. equality of natural numbers is transitive: `forall x y z. if x == y and y == z then x == z`
 9. natural numbers are closed under equality: `forall x y. if x is a natural number and x == y then y is a natural number`

Based on these five axioms, a full number theory can be extrapolated. Operations of addition, multiplication, total (linear) ordering on __N__, 


[wolfram]: http://mathworld.wolfram.com/PeanoArithmetic.html

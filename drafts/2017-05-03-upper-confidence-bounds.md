UCBs
============

UCB1 - arbitrary rewards

    Q(j) + sqrt [ 2 * ln(n) / n\_j ]

as j -> inf, we care less about the following term

works on the premise that `E[X_in] = q*(i)` (for stationary situations)

we can show that `E[T_j(n)] <= (8/delta) * ln n + c`

for a UCB, we can look at the concentration bounds (Chernoff-HoeHeling bound):

    P( S_n >= mu + eps ) <= e^(-2 eps^2 n)
    P( S_n <= mu - eps ) <= e^(-2 eps^2 n)

So the greater the n, the smaller the epsilon we allow.

    Q(a*) <= q(a*) - C_m,t(s)
    Q(a*) >= q(a*) + C_m,t(s)
    ...
    q_k(a_k) < q_k(i) + 2 * C_m,t(s_i)

...

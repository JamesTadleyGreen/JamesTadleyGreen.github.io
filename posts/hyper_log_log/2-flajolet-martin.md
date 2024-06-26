---
title: "Part II: Flajolet Martin"
tags: maths,algorithms,haskell,proofs
---

# Complexities
Now we have the structure of $R_n$, we can start to look at its limiting behavour. This
allows us to work out the big-$\mathcal{O}$ behavour.

## Theorem
The distribution of $R_n$ satisfies;

 - If $k<\log_2\log n$, then 
 $$q_{n,k} = 1-\mathcal{O}(ne^{-\log^2 n})$$,
 - if $k\leq \frac{3}{2} \log_2 n$ then,
 $$\begin{align}
 q_{n,k} &= \sum_{j=0} ((-1)^{v(j)}e^{-j\frac{n}{2^k}}) + \mathcal{O}(\frac{log^6 n}{\sqrt{n}})\\
&= \prod_{j=0} (1-e^{-2^j\frac{n}{2^k}}) + \mathcal{O}(\frac{log^6 n}{\sqrt{n}})
\end{align}$$
 - if $k\leq \frac{3}{2} \log_2 n + \delta$ for $\delta\geq 0$, the tail of the
distribution is exponential,
 $$q_{n,k} = \mathcal{O}\left(\frac{2^{-\delta}}{\sqrt{n}}\right)$$.


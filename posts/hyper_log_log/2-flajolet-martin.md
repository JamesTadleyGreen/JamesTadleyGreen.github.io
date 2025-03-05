---
title: "Part II: The Flajolet-Martin Algorithm"
tags: maths,algorithms,haskell
---

# Motivation

We covered off the basics of counting in the last section, and ended on a question:

> Can we count objects without using a store that is $\mathcal{O}(n)$?

The answer, surprisingly, is yes! But, we have to make some sacrifices:

 - We won't be 100% accurate in our answer
 - The more accurate we want our answers, the more space we need (with diminishing returns)

But, why do we care about this? What useful, real-world, application is this useful for.
As stated in the last section, we care when our datasets are extremely large, for example:

 - An app that gets billions of users per day, caring about distinct users and return rates
 - An app that gets billions of tweets per day, caring about distinct users tweeting
 - A database wanting to find unique IDs in a table

A fascinating example is given in the [original paper](http://algo.inria.fr/flajolet/Publications/FlMa85.pdf).
Consider a database, we may very well care about the intersection of two datasets, _e.g. for joins_.
But what is the best method for joining the sets? The paper lists three strategies, for datasets $A$ and $B$

 - Sort $A$ and search $B$ for each element
 - Sort $A$, sort $B$, then merge the two sets together
 - Remove duplicates in the above sets, then do either of the above methods

They proclaim that the complexities of the above strategies are as follows:

 - $\mathcal{O}(a\log\alpha + b\log\beta)$
 - $\mathcal{O}(a\log a + b\log b + a + b)$

Where $a=\lvert A\rvert$, $b=\lvert B\rvert$, and $\alpha$ and $\beta$ are the number of unique items
in $A$ and $B$ respectively.

## Complexity aside
### Strategy 1
Let's prove the above claims. We claim without proof that sorting a list is 
[$\mathcal{O}(n\log n)$](https://web.stanford.edu/class/archive/cs/cs161/cs161.1168/lecture7.pdf).
The first part comes from this fact, sorting $A$ takes $a\log\alpha$.
For the second part, we iterate over $B$, this is $\mathcal{O}(b)$. For each of
these items, we have to find it in $A$. To do this we perform binary search;
this involves splitting the array repeatedly in half and searching if our target
is above or below this value. [This article](../../posts/the_last_algorithms_course/2-search.html)
goes into more detail. Thus, we 'only' have to do $\log_2 b$ 'jumps'. So,
the total complexity is $b\log b$ as claimed.

### Strategy 2

# Proofs
## The probabilistic model

First a definition, 

> Let $\mathcal{B}$ denote the set of inifinite binary strings.

Alternatively, instead of binary strings we claim to be able to consider real 
numbers over the interval $[0,1]$.

To prove the above claim, we need to show there is a bijection between the 
two sets (as there isn't any structure to preserve). To show this, we lean
on another theorem, [Schröder–Bernstein theorem](https://en.wikipedia.org/wiki/Schr%C3%B6der%E2%80%93Bernstein_theorem),
given without proof. Now there's two things to prove, but hopefully they're
easier to prove. One direction is trivial, to show there is an injection
from $\mathcal{B}$ to $[0,1]$; we take a given string and calculate

$$\sum_{i=1}^{\infty} 2^{-i}B_i$$

where $B_i$ is the $i^{\text{th}}$ bit of the string. This is clearly
in the range $[0,1]$ and the limits make sense.

To show the converse is only marginally more difficult, take any $x\in[0,1]$.
Now compute its binary representation, to do this we take the limit of the 
below behaviour. _We won't prove this converges, or is unique, just take my
word for it._

For each $i$ as above, compute $X_i = x_0 + x_1 + \dots + x_i$, where
$x_j$ is either $0$ or $2^{-j}$. Simply work out if adding the $2^{-j}$
will take us over the target.

For example, consider $0.1875$ both $0.5$ and $0.25$ take us over the target.
So we start with $0.125 + 0.0625$ and we've our target.

Now take $0.\bar{3}$ we skip $0.5$, add $0.25$, skip $0.125$, add $0.0625$, and so on.
Thus the binary string is $0101010\dots$.

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


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
For the second part, we iterate over $B$, this is $\mathcal{O}(b). For each of
these items, we have to find it in $A$. To do this we perform binary search;
this involves splitting the array repeatedly in half and searching if our target
is above or below this value. [This article](../../posts/the_last_algorithms_course/2-search.html)
goes into more detail. Thus, we 'only' have to do $\log_2 b$ 'jumps'. So,
the total complexity is $b\log b$ as claimed.

### Strategy 2

---
title: "Part I: Probabilistic Counting Algorithms"
tags: maths,algorithms,haskell
---

# Counting

Sometimes we need to count things, sometimes we need to count distinct or unique things.
In computer science, counting things (ideally quickly) is very important. Counting 
distinct things is more important, and more difficult.

# The accurate way
## Time Complexity
### Mappings

Lets consider the naïve way. If I wanted to count the number the number of distinct 
integers in an array. Just think of this as a map from whatever important detail we care
about, to the integers. For example, if we wanted the number of distinct colour of balls,
we might use a mapping such as 
```
red -> 1
blue -> 2
green -> 3
```
Then in an array / list of numbers, all we care about is how many of these three 
integers occur. 

_Side note: Creating a mapping like this might not be easy, we don't know how many items
exist (obviously) up front. We'll ignore this fact for the time being._

### The Algorithm
Now to count distinct items, we use a hash table, this has different names in different
languages:

 - Python: `dict`
 - Javascript: `map`
 - Java, Rust, Haskell: `hashmap`
 - C: `good luck, build yourself`

The idea is the same across all of them:

 - Given a key
 - Hash the key
 - Check the hash in the hash table
 - If the hash exists, double check the key against the value in the hash table
 - Add the key if none exists

This means we're able to add an item to the hash table in $\mathcal{O}(h)$ time, where
$h$ is the time it takes to hash the value. Since the hashing function is independent of
the number of items, this is $\mathcal{O}(1)$ time.
You might look at this and think we've missed the time it takes to find and insert the 
value into the hash table.

### Hash table

How does the hash table actually work? In the general case, it's an array type structure
with a linked list at each node. However for our purposes we will consider it to simply
be an array. How big is said array? It's $n \times  m$ where $n$ is the number of distinct items
we can possibly store, and $m$ is the size of the hashed values.

To store a value it's clearly $\mathcal{O}(1)$ time, we simply add / check a value at 
position `hash(value)`$\times m$

_Technically here we're using a hashring, which is taking the hashes modulo some large
number, namely the upper bound on the possible distinct elements._

### Complexity
So inserting is $\mathcal{O}(1)$ time, counting the number of elements in the hash table
is $\mathcal{O}(n)$ time, as we need to iterate over all values. Finally, looping over 
the list is also $\mathcal{O}(n)$ time.

## Space Complexity
So we're fine in terms of time, but how do we looks for space? Well, we need to have a 
hash table of the same size of our input array.





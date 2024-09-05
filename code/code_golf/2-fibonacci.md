---
title: "Part II: Fibonacci"
tags: maths,algorithms,golf,python
---

# The Problem

A Fibonacci number is one that falls into the sequence
$$0, 1, 1, 2, 3, 5, 8, 13, 21, 34, \ldots$$.
This is characterised by the following relationship for $F_0 = 0$ and $F_1 = 1$:
$$F_n = F_{n-1} + F_{n-2}$$

Calculate, and print, the first 31 Fibonacci numbers.

# Attempts
## First attempt
```python
a=0
b=1
for i in range(31):
    print(a)
    a,b = b,a+b
```

This is already pretty short, at just 50 characters. But we can do better!

> 50 characters

## The `for` loop
This takes up a fair bit of space, my first thought is since we don't use `i` at any
point we can simply iterate over something other than a range.
```python
for _ in'a'*31:
```
This saves 4 characters (and makes it a lot less readable).

> 46 characters

It turns out we can do this in another way, using a `while` loop.
```python
a=0
b=1
while a<823041:
    print(a)
    a,b = b,a+b
```

## Big numbers
When we have a number that's a power of 2 (or near in our case) there is an easy bound
we can put on it.

```python
while a<2**20:
```

> 45 characters

Now we have a slightly _'mathsy'_ observation. Due to the $AM-GM$ inequality, we get
larger numbers when the exponent and the ... are closer in value.
Thus, and since our bound doesn't need to be exact, we can bring these closer inline,
and save a character.

```python
while a<9**6:
```
> 42 characters

## Inlining
We have a lot of indentation due to the `for` loop, lets try some horrid inlining to
save a couple more characters.

```
a=0
b=1
while b<9**6:print(a);a,b=b+a,a
```

> 39 characters

And that's the best I can do for now.

# Measuring success

At time of writing I'm 632^{nd} which is pretty good for an hour or so's work.
More importantly the best solution (in Python) is only 36 characters, so I'm within 3
characters of that, that's less than 10%. I'm pretty chuffed with that.

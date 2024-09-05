---
title: "Part IV: Divisors"
tags: maths,algorithms,golf,python,fractions
---

# The Problem

Print the divisors of each of the numbers from 1 to 100. For example, the divisors of;

- 1 are 1
- 2 are 1 and 2
- 12 are 1, 2, 3, 4, 6, and 12
- 30 are 1, 2, 3, 5, 6, 10, 15, 30
- 100 are 1, 2, 4, 5, 10, 20, 25, 50, 100

# Attempts
## First attempt

```python
for i in range(1,101):
	print(' '.join([str(j) for j in range(1,i+1) if i%j==0]))
```

> 81 characters.

This is a pretty simple solution, we join together a list comprehension. The obvious
pain points are converting the `j` to a string and the `join` itself.

## The `join`

To remove the join we use the advantage that the print statement auto-spaces arguments
given to it, thus we can write something like:
```python
print(*[...])
```

## The double range

The next obvious pain point is the repeating of the range. The first observation is we
needn't go up to `i`, we can go up to whatever value we like. Thus, we can repeat the
range object as before. Abstracting this out we get a solution of the form:

```python
k=range(1,101)
for i in k:print(*[j for j in k if i%j==0])
```

> 58 characters.

## The equality check

It was my friend who noticed this one, we are checking for equality with `0` in the
above solutions. But, this is a stricter condition than what is required. We can infact
check it is less than 1, saving one more character.

```python
k=range(1,101)
for i in k:print(*[j for j in k if i%j<0])
```

> 57 characters.

## Future thoughts

An alternative 58 character solution I saw is below, I think this has merit as a
potential improvement just because it is quite different.

```python
k=range(1,101)
for i in k:print(*filter(lambda x:i%x<0,k))
```

# Measuring success

I'm currently joint first on this problem, can't really ask for more than that. As
written in the [Future thoughts](#future-thoughts) section, there could be some scope
moving forward, but I probably won't revisit this anytime soon.

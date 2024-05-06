---
title: "Part V: Prime Numbers"
tags: maths,algorithms,golf,python,prime
---

# The Problem

Print all of the prime numbers from 1 to 100.

- 2,3,5,7,...

# Attempts
## First attempt

```python
for i in range(2,99):
 if not[j for j in range(2,i) if i%j<1]:print(i)
```

> 70 characters.

A nice first attempt, there's a couple of tricks like `range(2,i)if` and so on that save
minimal characters. But to make any real progress we need a new approach.

We only checked up to `99` since we know 100 isn't prime and it saves a character.

## Pseudo-primality testing

Since we're only interested in numbers below 100, we can use some _odd_ primality
testing. Did you know that for all primes less than `300` we can check if the number is
either `2` or $2^j mod 2$ is `2`. I didn't, but [this stackoverflow post](https://stackoverflow.com/questions/30216485/what-is-the-best-way-to-check-if-a-number-is-prime-in-python)
did.

I believe it's based of [Lucas pseudoprime](https://en.wikipedia.org/wiki/Lucas_pseudoprime)'s,
but I haven't bothered reading into it more.

So our solution becomes:
```python
[print(j)for j in range(1,98)if 2in[j,2**j%j]]
```

> 46 characters.

## Looking online (cheating)
In looking up the [prime testing](#pseudo-primality-testing) online I found
[this stackoverflow](https://codegolf.stackexchange.com/questions/5977/list-of-primes-under-a-million)
and adapted one of the solutions to save some additional characters.

```python
k=P=1
while k<99:P%k>0==print(k);P*=k*k;k+=1
```

This is a genius solution which the author on stackoverflow explains much more elegantly
than I could.

> 44 characters.

## Future thoughts

Because 100 is such a low bound, it feels like we might be able to encode all the primes
in a hard-coded string and simply 'split them out'.

My thought is we embed the differences between the primes, as they're all less than 10,
then print based on these differences. Something like the below.

```python
j=0
b='2122424'
for i in b:j+=int(i);print(j)
```

# Measuring success

I cheated a little but this only saved me two characters, I wouldn't have found this
solution by myself, but it's close enough I don't feel too bad.

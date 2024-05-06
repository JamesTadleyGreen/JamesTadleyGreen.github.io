---
title: "Part VI: Quine"
tags: maths,algorithms,golf,python,prime
---

# The Problem

Print all of the prime numbers from 1 to 100.

- 2,3,5,7,...

# Attempts
## First attempt

```python
s = r"print('s = r\"' + s + '\"' + '\nexec(s)')"
exec(s)

```

```python
s='print(f"{s=}\\nexec(s)")'
exec(s)

```

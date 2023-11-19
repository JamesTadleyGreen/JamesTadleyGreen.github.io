> :memo: *TL;DR* - Haskell lends itself nicely to basic raytracing, simple code (not mine) creates incredible first results.

---
## Table of Contents
&nbsp;1. [Intro](#intro)
&nbsp;2. [Vectors](#vectors)
&nbsp;3. [Geometric functions](#geometric-functions)
&nbsp;4. [Conclusion](#conclusion)

---

## Intro

Hello! This is my first post, and realistically, my first real attempt at blogging. Without further ado, we'll jump straight into the content.

We’re looking at functional programming. Specifically, we’re going to look at implementing some basic raytracing in Haskell. Firstly, the code for this project is all on my [GitHub](https://github.com/JamesTadleyGreen/Haskell-Raytracing). Hopefully, at some point moving forward I’ll have a website set-up to show off the results.

There are a couple of basic resources I’m using to have a first stab at the project:

* [Htrace](https://www.nobugs.org/developer/htrace/index.html) is the initial work we’ll be analysing and expanding upon.
* [This incredible video](https://www.youtube.com/watch?v=gsZiJeaMO48) explaining how modern CGI and ray tracing works. The implementation of any of this will be post the initial analysis.

---
<img src=https://www.nobugs.org/developer/htrace/htrace.jpg class=markdown-image>

![](https://www.nobugs.org/developer/htrace/htrace.jpg)

## Vectors

Let's jump into some code.


```python
def square(x: int)-> int:
    return x**2
```


```haskell
type Vector3 = (Float, Float, Float)
```

Here we have a definition of a vector, as you can see the syntax here is pretty self-explanatory. A 3-dimensional vector is an ordered 3-tuple. Next let's talk about some operations you'll be familiar with.

```haskell
add :: Vector3 -> Vector3 -> Vector3
add (x,y,z) (a,b,c) = (a+x, b+y, c+z)

sub :: Vector3 -> Vector3 -> Vector3
sub (a,b,c) (x,y,z) = (a-x, b-y, c-z)

squared_mag :: Vector3 -> Float
squared_mag (x,y,z) = (x*x + y*y + z*z)

mag :: Vector3 -> Float
mag v = sqrt (squared_mag v)
```

All of the above are pretty basic mathematically, and similarly the code is faily trivial. We could alternativly, and worsely (though pointfreely),  define:
```haskell
squared_mag :: Vector3 -> Float
squared_mag = sum map (**2)
```
But then we'd have to worry about defining `map` and `sum` for `Vector3` and suddenly I've lost interest.

```haskell
scalarmult :: Vector3 -> Float -> Vector3
scalarmult (x,y,z) c = (x*c, y*c, z*c)

dot :: Vector3 -> Vector3 -> Float
dot (x,y,z) (a,b,c) = x*a + b*y + c*z

cross :: Vector3 -> Vector3 -> Vector3
cross (a,b,c) (x,y,z) = (b*z + c*y, -(a*z + c*x), a*y + b*x)

normalize :: Vector3 -> Vector3
normalize v
  | (mag v) /= 0 = scalarmult v (1 / mag v)
  | otherwise    = (0,0,0)

neg :: Vector3 -> Vector3
neg (x,y,z) = (-x,-y,-z)
```
These again are all pretty standard, the only change I'd make is to make `scalarmult` left multiplication:

```haskell
scalarmult :: Float -> Vector3 -> Vector3
scalarmult c (x,y,z) = (c*x, c*y, c*z)

neg :: Vector3 -> Vector3
neg = scalarmult (-1)
```
---
## Geometric functions
Now we can move into some more geometric mathematics, we define some more datatypes.

```haskell
type Point3 = Vector3
type Direction3 = Vector3
type Time = Float
type Ray = (Point3, Direction3) -- base and direction

position_at_time :: Ray -> Time -> Point3
position_at_time (base, dir) t = base `add` (scalarmult dir t)
```

Note the `position_at_time` function assumes no acceleration (allowed as we're working with light). We'll omit code for the next couple of definitions for brevity, we define a quadratic solver and `xor`. These are the standard definition.

---
## Conclusion

The next steps are to start looking into colours, I have something substantially different intended for this. So we'll cut this post here for the time being.

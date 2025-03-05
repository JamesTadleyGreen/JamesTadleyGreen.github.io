---
title: "Part VI: Typeclasses"
tags: haskell,type,typeclasses
---

# Chapter Exercises
1. c)
1. b)
1. a)
1. c)
1. a)

# Does it Typecheck?
1. No, `bool` isn't a subclass of `Show`
1. No, `Mood` doesn't have the `Eq` typeclass
1. 
    a. Only `Blah` or `Woot`
    a. The type checker will complain that you can't compare `Num` with `Mood`
    a. They typechecker will complain that `Mood` doesn't have an `Ord` instance
1. This will typecheck, but `s1` isn't a sentence as it needs an `Object` to fill the hole

# Given a datatype declaration, what can we do?
1. Needs type declaration
1. Ok
1. Ok
1. No instance of `Ord` on `Papu`

# Match the Types
1. This fails as we're making the type more general
1. This fails as we're making the type more general
1. This works as `Fractional` is a subclass of `Float`
1. This works as `RealFrac` is a subclass of `Fractional`
1. This will work as we had no specifcation on the type
1. This will work for the same reason as above
1. This won't work as we're generalising to the most generic type here
1. This won't work as `Num` is a superclass of `Int`
1. This will work as `Int` is a subclass of `Ord`
1. This won't work as `Ord` is a superclass of `Char`
1. This won't work as above

# Type-Kwon-Do
1. `chk f a b = (f a) == b`
1. `arith f i a = i + (f a)`


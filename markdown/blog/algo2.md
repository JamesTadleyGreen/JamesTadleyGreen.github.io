# Search

## Linear Search
Currently I want to write in Haskell, mainly because it's 'mathsy'. It's been a while
so my first attempt looks like this.
```haskell
linearSearch :: Eq a => [a] -> a -> Int
linearSearch as s = linearSearch' as s 0

linearSearch' :: Eq a => [a] -> a -> Int -> Int
linearSearch' (a:as) s i
    | a == s    = i
    | otherwise = linearSearch' a as (i+1)
```

As soon as I wrote this, I remembered folds exist. Thus my second attempt was.
```haskell
linearSearch :: Eq a => [a] -> a -> Int
linearSearch as s =
    fst $ foldl (\(i, b) x ->
        if b
            then (i,True)
        else
            if x == s
                then (i, True)
            else
                (i + 1, False))
        (0, False) as
```

Well that was gross, firstly if the list is empty, the search returns 0.
Secondly, if the search fails, it returns the length of the list.
Thirdly, it doesn't short circuit.
Finally, it's just super gross to look at from a code perspective.

Both are $\mathcal{O}(n)$, which is good!

Now I watch a little more and it turns out we don't need the index! We only need to
return a `bool` indicating if the search was successful.
```haskell
linearSearch :: Eq a => [a] -> a -> Bool
linearSearch [] _ = False
linearSearch (a:as) s
    | a == s    = True
    | otherwise = linearSearch as s
```
### Notes
Had a [google](https://stackoverflow.com/questions/52518817/haskell-linear-search-returning-index)
after the event, I should be returning a `Maybe Int` in my first attempts.
Using fmap to lift the `(+1)` into the `Just` is something I would have missed.
## Binary Search
Things get a bit more orderly here. We assume we have ordered lists. Thus, our function
type is going to change. It's going to be `Ord a => [a] -> a -> Int`. I love Hasekll
for exactly this reason. The type signature is giving such a breadth of information for
free, allowing less cognitive overhead.

My first attempt is as follows:
```haskell
binarySearch :: Ord a => [a] -> a -> Bool
binarySearch as s = binarySearch' as 0 (length as) s

binarySearch' :: Ord a => [a] -> Int -> Int -> a -> Bool
binarySearch' as lo hi s
 | lo == hi  = False
 | s == v    = True
 | s > v     = binarySearch' as (m+1) hi s
 | otherwise = binarySearch' as lo m s
 where m = lo + (hi - lo) `div` 2
       v = as !! m
```

Interestingly The Primeagen uses `lo < hi` this confuses me, I can't think of a
situation where this could possibly occur. So I'll leave mine as is.

### Notes
Using [StackOverflow](https://codereview.stackexchange.com/questions/158096/binary-search-in-haskell)
I see I should have used `quot` instead of `div`. This is due to
[this answer](https://stackoverflow.com/questions/8111120/integral-operators-quot-vs-div)
regarding speed of the operations.

My implementation of this looks close enough that I am happy.

## Two Crystal Problem
First impressions, this is cool. It's very much similar to maths problems I've done in
interviews before. Second impression, i need to give this some thought. It's
obviously a binary search, but exactly how isn't obvious.

We can't 'hit' the max more than once, else we have no more balls. Some scenarios:
 - We drop from floor 3 and it breaks
   - We then have to drop the ball from the 1st floor, else if it breaks on the second
  we don't know if it'd break on the first.
   - This generalises to, if our first ball breaks, we're subject to using [linear search](#linear-search).
 - I think the general tactic, is that we want the number of subdivisions to equal the
 length of the linear search.
   - For example, with 10 floors this would be thirds, if our ball breaks on the first
   bounce, we have two additional checks.
   - For any additional break we still have two additional checks.
 - This means for x floors we want to have \\(\sqrt{x}\\) subdivisions.

This logic is probably "off-by-one" somewhere but I give up thinking about it any more!

_Welp, got that one right in one._

After his explanation there was one bit I wasn't quite satisfied with, why \\(\sqrt[3]{x}\\)
doesn't work as well. While I don't think it's as bad as \\(\mathcal{O}(n)\\) I think
it's worse than \\(\mathcal{O}(\sqrt{n})\\).

Let's consider \\(\sqrt[3]{x}\\), whilst our linear search becomes smaller. Our jumps
do also, we have to do \\(\sqrt[3]{x}^2\\) jumps. In general, for an \\(i^th\\) power
we have a run time of \\(x^(1/i) + x^(1 - i/1)\\). We want to minimise this,
which is just when the two terms are equal (big-\\(\mathcal{O}\\) notation 'takes' only
the largest term), _i.e._ taking the square-root.

Let's implement, we can do this in a really nice general way (and re-implement binary
search).

```haskell
generalisedSearch :: Ord a => [Int -> Int -> Int] -> [a] -> a -> Bool
generalisedSearch fs as s = generalisedSearch' fs as 0 (length as) s

generalisedSearch' :: Ord a => [Int -> Int -> Int] -> [a] -> Int -> Int -> a -> Bool
generalisedSearch' (f:fs) as lo hi s
 | lo == hi  = False
 | s == v    = True
 | s > v     = generalisedSearch' (f:fs) as (m+1) hi s
 | otherwise = generalisedSearch' fs as lo m s
 where m = f lo hi
       v = as !! m

binarySearch :: Ord a => [a] -> a -> Bool
binarySearch = generalisedSearch $ repeat $ \lo hi -> lo + (hi - lo) `quot` 2
```
Thus, our `crystalSearch` becomes:
```haskell
crystalSearch :: Ord a => [a] -> a -> Bool
crystalSearch = generalisedSearch [\lo hi -> min (lo + isqrt hi) (hi -1), \lo hi -> lo + 1]
```

The `min` in there is vile, and also shows that I'm too tired and shouldn't be doing
at 11:30 at night. But we have generalised fairly well!

Looking at The Primeagen's solution, he's hidden away this `min` function, but has a
similar implementation.

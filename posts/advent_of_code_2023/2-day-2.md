---
title: "Day II: Cube Conundrum"
tags: AOC,haskell
---

# [Cube Conundrum](https://adventofcode.com/2023/day/2)

This was easier than yesterday! Since this is an easier problem, let's focus on
datatypes. The question talks about games and colours, so let's define what these
look like.

:::{.code-include lexer="haskell" name="data"}
:::

# Part I
## Question
As the datatypes above alluded to, we have some records of games, in the format:

```
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
```

The question is simple, given the restriction of 12 red, 13 green, and 14 blue cubes,
how many of the above are valid games?

## My Solution

The first job is to parse the input into our data types.

:::{.code-include lexer="haskell" name="parseInput"}
:::

The above is mainly housekeeping and nothing clever happens.

:::{.code-include lexer="haskell" name="part1"}
:::

I inititally wrote this with folds, but in retrospect, I think the below approach of
recursion is prettier and easier to read.

:::{.code-include lexer="haskell" name="part1b"}
:::

Moving the checking logic to the `validGames'` function makes much more sense, it also
simplifies the data we're passing around.

# Part II
## Question

We slightly complicate the problem, instead of a fixed limit, we want to calculate what
the minimum possible constraint we can add to each set of games.

## My Solution
This time we fold over in exactly the same way, calculating the product of the tuple,
taking the maximum at each step. Annoying, to get pairwise maximum we have to write a
custom function, since, if we used the built in max, this is lexicographical ordering.

:::{.code-include lexer="haskell" name="part2"}
:::

# Others Solutions
Looking at [this](https://topaz.github.io/paste/#XQAAAQB6CgAAAAAAAAA2m8ixrhLu7WoI0BF4zkDp9jvUlcRHHvK3yKnG3P2kZ0VVAnVdxkqaaMuXbCDS2sUmeACmsqQV5sx1lUFUqxyA41kr3M2cyJLoDN5h9Qj1pgdNxrb6qHR7ztT9mF2TsvW3SiSo80gXLGt1MtjZlwKmIubb5Grf5moHMsy3xxji1KVu7dHt5riywlAzC+C8/LeeQVhaAmMoHlB35obNubL2cVbCtQBuKwL9QXfNUteVR3SRN4/9l3ybV95Bs/1cVPHq4I2A5ZLml6AqcLYBqYw8HFL/fSkIH7W96J5thynJSHR1GaGSApt4u7hstJ9Pcer6wJwBM5xD/VETvFEW5wewOX8f4d/vjI1yXkG8ghIccCBCDn0nb02eHs1rbCEPU8Gw7FkEku9jrk02lcztD4tZgTIia0RzFjaQzSynpoJadSWtcWnrtaNlrD604utNgKoRAj/stYutotEFSp2LNho2xxCAbUWIm/cRZKkNru7B8IrNJ/rJXGY5pb2Xc9mHhdvi6TcxRSDN6bg/cpA13MZ8/mjkaaYmnIt9tfpn4REBvXvI/bV1hbeuTsv2gX56bUL8NiyGHzgFTrcap/B85LkAvAUFH+snkQDwB6PFyuOvwPhv5rxWNwe68CEZNYV6bjNZ8FxgZdGTZOgM5puDraFlC0zsr8V5ZsutBM2XDEfuL54lZ3AAOroQnah7wMgAgFM0S7cxoXVINi7xhfu0KaKMQIQ3OfcK1c5SDQU635Ig4dntBqD2fGYNAMohXa59qCVxkMLxepJxjCqvfHLJJB9yZ/6SO06Oy9gREjuhmhiL8prov115/emHowzoBgWb1985KlpLbSdCIHB1DWVTR63X9m8rrl9b4KQZCzvgFU9T4lTROtgF2NEpwAD3v1/Wrh1jcquQ21R9IvVJRod5Te2zrV0Asc80HKtHyuuAd5EM0K9zMWgFkuLtWYMpaF0XUGFpPYniYLd4kXARgh6U0fQq+0m+rVuKy3ixaqPCyc6A1CVpAupDEfgr2FxgkMdRWZMwFHL+JoQIHqE2Py8h6TRoHMMVWcvMj+wmE7J4wXwkvp7DhCrax24Tx62Lzah06fJWvoO3N13K7hNNN4Gov8mIAT6n56Bn1Q6Hb6aXoYoFvkOCweJLd06Ht1ow6iPjdyerYX+XVLDOZjAVlq1398QY9T5ObObT05E1a5xCu9iCkKpcLVGBFavCsFTnmhnZx2//1v7bEw==)
solution, it turns out there are significantly more 'user friendly' ways of parsing data
in Haskell.

The key takeaway is the below:

```haskell
parseGame :: Parser Game
parseGame = do
  string "Game "
  id <- decimal
  char ':'
  gameShows <- parseGameShow `sepBy` char ';'
  return (id, gameShows)

parseGameShow :: Parser GameShow
parseGameShow = parseCubeCount `sepBy` (char ',')

parseCubeCount :: Parser CubeCount
parseCubeCount = do
  skipSpace
  count <- decimal
  space
  choice [Blue count <$ string "blue", Red count <$ string "red", Green count <$ string "green"]
```

The they're using [Attoparsec](https://hackage.haskell.org/package/attoparsec) for
parsing, and the `do`-notation is definitely cleaner than my approach.
A new function that I hadn't seen before is `<$`, which has type
`Functor f => a -> f b -> f a`. At first glace this appears pointless, we discard `b`?
The use in this case is to simply 'recognise' the structure and discard the result. Thus,
we 'recognise' and ignore `"blue"`, only caring about taking `Blue count`.
This has a lot of similarities to `>> :: Monad m => m a -> m b -> m b`.
The below are equal.
```haskell
y = do
    x <- a
    x <$ b

z = b >> a

z == y
```

The rest of their solution is pretty similar to mine, with the exception of utilising
the `all` operator.

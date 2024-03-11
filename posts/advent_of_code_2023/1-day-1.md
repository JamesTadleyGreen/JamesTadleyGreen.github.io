---
title: "Day I: Trebuchet?!"
tags: AOC,haskell
---

# Trebuchet?!

Compared to last year, this was a harder start. The crux of this problem was to parse a
string and filter out the first and last 'number'.

# Part I
## Question
Given a string, search from the start and get the first number. The perform the same
from the end of the string. Concatenate these strings, and convert to `int`. Then sum
these numbers together.

```
1abc2       -> 12
pqr3stu8vwx -> 37
a1b2c3d4e5f -> 15
treb7uchet  -> 77

answer = 12 + 37 + 15 + 77 = 142
```

## My Solution
The general plan of attack is as follows:

- Create a parser to extract the first and last numbers from the string.
- Concatenate these strings together and cast to int.
- Convert the input into an iterable.
- Fold over the iterable with the above parser summing as we go.

:::{.code-include lexer="haskell" name="part1"}
:::

# Part II
## Question
Now we have a harder task, some of the numbers are spelt-out. This means we can't do a
single pass (we actually can but the code is much more complex). The only part of the
above code we need to modify is our parser.

:::{.code-include lexer="haskell" name="part2"}
:::

This looks worse than it actually is, at each step we check if the string is in our
`numbers` map. If it is, we return the number. If not, we continue.

# Others Solutions
<blockquote class="reddit-embed-bq" data-embed-showtitle="true" data-embed-theme="dark" data-embed-height="833"><a href="https://www.reddit.com/r/haskell/comments/1885snc/comment/kbjvg1k/">Comment</a><br> by<a href="https://www.reddit.com/user/NonFunctionalHuman/">u/NonFunctionalHuman</a> from discussion<a href="https://www.reddit.com/r/haskell/comments/1885snc/advent_of_code_day_one_solution/"><no value=""></no></a><br> in<a href="https://www.reddit.com/r/haskell/">haskell</a></blockquote><script async="" src="https://embed.reddit.com/widgets.js" charset="UTF-8"></script>

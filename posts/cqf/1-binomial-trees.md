---
title: "Binomial Trees In Option Pricing"
tags: maths,algorithms,haskell,finance,options
---

# Binomial Trees in Option Pricing

The [Binomial Option Pricing Model](https://en.wikipedia.org/wiki/Binomial_options_pricing_model) is one of the most simplistic option modelling
frameworks that exists. We're going to discuss the motivation, simple code implimentation,
and finally some examples and thought's about application.


## Motivation

Think about the most simple pricing model you can for a discrete model of a stock option.
THe most simple one I can come up with is flipping a coin. If the coin comes up heads, the
price of the option increases by say $x$ and if the coin comes up tails, the option instead
loses value of say $y$. We can express this in a difference, and later clearer, way. Choose 
two values $u$, $d$, representing the up and down moves respectively. Then the value of the 
option from time $i$ to time $i+1$ is $S_i \cdot u$ on an up move, or $S_i\cdot d$ on a down
move, where $S_i$ is the value of the option at time $i$.

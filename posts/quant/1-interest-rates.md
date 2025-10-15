---
title: Interest Rates
tags: quant, interest, stochastic
---

# A brief aside

I had fully intended, and actually initially started this project in 
[Gleam](https://gleam.run/), sadly I couldn't get past some versioning issues.
As the focus of this project was more on the implementaiton / learnings,
I decided to move into something more familier, Haskell.

# The basics
Interest rates can be modeled as stochastic random processes of the form:
    $$\mathrm{d}x = \theta (\mu - x_t) \mathrm{d}t + \sigma \mathrm{d}W$$

But, what does the above _mean_?

## Randomnes

First, $\mathrm{d}W$ is a [Weiner process](https://en.wikipedia.org/wiki/Wiener_process),
this is where the randomness comes from. What this is exactly is a random walk,
any time step is independent of the others. In our code this will manifest
itself as the normal distribution. Specifically, the difference between
any two steps in the one dimensional is a normal distribution of the form:
    $$W_{t_2 - t_1} = W_{t_2} - W_{t_1} \sim N(0, t_2 - t_1)$$

Okay, that's the randomness in the system accounted for, what's the remainder?
What's the order to our chaos?

## Mean Reversion

We expect interest rates to sit at some value across the longer term.
For example:

- The [UK's interest rates](https://www.bankofengland.co.uk/boeapps/database/Bank-Rate.asp)
sat in the 0%-1% range from 2009 to 2022, (although higher now to combat inflation).
- The [US's interest rates](https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_real_long_term)
have a long term averge of about 4.5%

But, our above walk has a mean of zero. How do we move this up to the long term expected
value? The most basic approach would be to add the long term mean to our walk. This would
give an expected value of the long term mean, however, the walk could easily divert from
this value in quite an extreme manner. In reality, it's likely the result would sit around
this long-term average, with some deviation in either direction.

So we use some form of mean reversion. The idea being, if we're above the mean, the term will
drive us lower, and if we're below the mean, the term will drive us up.

That's what this $\theta (\mu - x_t)$ term corresponds to: 

 - If the prior term is greater than the mean then $\mu - x_t$ is negative, pushing us down.
 - The converse pulls us up.

So what does $\theta$ correspond to? This is the mean reversion rate. It corresponds to how
fast we want teh mean reversion to occour, higer numbers tie the interest rate closer to the 
long-term mean, lower numbers give more weight to the random walk.

# Stochastic

So, the randomness in the walk gives us different 'paths' for our interest rate on a given
simulation. Why do we want this, can't we just have a central set of assumptions? 
The benefit to using stocastic processes is the universe of results they create. This is
the concept of a funnel chart, at time $t=0$, everything is determined. We know exactly 
what the interest rate is, at the next timestep, it's pretty determined that there will
be minor movement, at time $t=100$ the universe of values will be a lot greater.

By having a stochastic process, we implicitly embed our certainty of events based on this
'funnel'.

# Ornstein-Uhlenbeck

An [Ornstein–Uhlenbeck process](https://en.wikipedia.org/wiki/Ornstein%E2%80%93Uhlenbeck_process)
describes exactly what we were talking about above, it is defined as:
    $$\mathrm{d}x = \theta (\mu - x_t) \mathrm{d}t + \sigma \mathrm{d}W$$

Let's write our fist code to describe this process, as it seems like a natural abstraction
we'd want to take advantage of.

First, let's create a `data` for the O-U process, from our above formula we can see how to 
dissect our variables.

<iframe frameborder="0" scrolling="no" style="width:100%; height:184px;" allow="clipboard-write" src="https://emgithub.com/iframe.html?target=https%3A%2F%2Fgithub.com%2FJamesTadleyGreen%2Fquant-haskell%2Fblob%2F54f7030dc274c4a94fd77acb39e9e045fce6c1c3%2Fsrc%2FMyLib.hs%23L10-L14&style=tomorrow-night-blue&type=code&showBorder=on&showLineNumbers=on&showFileMeta=on&showFullPath=on&showCopy=on"></iframe>

Now let's define a function that given a 'step' function, creates random variables and
sequentially applies the 'step' function to create each $x_t$.

<iframe frameborder="0" scrolling="no" style="width:100%; height:163px;" allow="clipboard-write" src="https://emgithub.com/iframe.html?target=https%3A%2F%2Fgithub.com%2FJamesTadleyGreen%2Fquant-haskell%2Fblob%2F54f7030dc274c4a94fd77acb39e9e045fce6c1c3%2Fsrc%2FMyLib.hs%23L17-L20&style=tomorrow-night-blue&type=code&showBorder=on&showLineNumbers=on&showFileMeta=on&showFullPath=on&showCopy=on"></iframe>

Let's discuss some types quickly, the initial `x` is a float, this is simple. The 'step'
function takes two floats and returns a float. This corresponds to the last step,
the random variable $N(0,1)$, and outputting the next step. The number of steps
to generate is obviously an `Int`, and the most complex part is the return variable
`RVar [Float]`. This represents a list of random vairables each of type `Float`, we won't
cover any more in detail what this means here.

Line `19` tells us we're creating `number_of_steps` random $N(0,1)$ variables.

We'll  treat line `20` in two parts. Firstly the `pure`, this lifts our result to the
required monad, in our case `RVar`, this function has the type signature 
`[Float] -> RVar [Float]` in our case. The second part is the repeated application of
our function `f` to our prior value. Here we use `scanl` to calculate the intermidiate
values.

# Vascik

Let's create a specific model, starting off simply let's write the [Vascik](https://en.wikipedia.org/wiki/Vasicek_model)
model;
    $$\mathrm{d}x = \theta (\mu - x_t) \mathrm{d}t + \sigma \mathrm{d}W$$

This is exactly the example we had at the beginning.

Again, let's split this into two parts;

 - A meta-function for applying the O-U function.
 - A step function which tells us how to get to $x_{t+1}$ from $x_t$.

## Meta function

<iframe frameborder="0" scrolling="no" style="width:100%; height:121px;" allow="clipboard-write" src="https://emgithub.com/iframe.html?target=https%3A%2F%2Fgithub.com%2FJamesTadleyGreen%2Fquant-haskell%2Fblob%2F54f7030dc274c4a94fd77acb39e9e045fce6c1c3%2Fsrc%2FMyLib.hs%23L29-L30&style=tomorrow-night-blue&type=code&showBorder=on&showLineNumbers=on&showFileMeta=on&showFullPath=on&showCopy=on"></iframe>

Super simple, just an application of `ornsteinUhlenbeck`, applying our `vasicekStep`
function.

## Step function

<iframe frameborder="0" scrolling="no" style="width:100%; height:184px;" allow="clipboard-write" src="https://emgithub.com/iframe.html?target=https%3A%2F%2Fgithub.com%2FJamesTadleyGreen%2Fquant-haskell%2Fblob%2F54f7030dc274c4a94fd77acb39e9e045fce6c1c3%2Fsrc%2FMyLib.hs%23L22-L26&style=tomorrow-night-blue&type=code&showBorder=on&showLineNumbers=on&showFileMeta=on&showFullPath=on&showCopy=on"></iframe>

This marries exactly up to the function above, with a bit more of a breakdown
into what the consitiuant parts represent. For clarity, let's break the above
equation down in the same way as the code.

$$
\begin{aligned}
    x_{n+1} &= x_n &+& \mathrm{d}x\\
    \implies x_{n+1} &= x_n &+& \theta (\mu - x_t) \mathrm{d}t &+& \sigma \mathrm{d}W\\
    \implies x_{n+1} &= x_n &+& \text{drift} &+& \text{noise}\\
\end{aligned}
$$


# CIR
## Negative interest rates
The 'issue' with the [Vascik model](#Vascik) is that it allows negative interest rates.
The general consensus is that interest rates rarely go negative, although 
[recent history](https://en.wikipedia.org/wiki/Negative_interest_on_excess_reserves) says otherwise.

So how can we floor our model so that we don't have to think about this situation.
The most basic iteration of this would be to not allow $x$ to move negative.
But, this would create some off, flatlining charts, so how can we do better?
Let's manipulate the noise, by using the square root function.

If we have the function become 'less noisy' around 0, then we won't have negative
interest rates. So, how can we reduce the noise as our $x$s approach 0, we can 
multiply by a factor that shrinks as we get closer to 0. But, what function do
we choose? A few come to mind, $\frac{1}{x}$, $\frac{1}{x^2}$, $\sqrt{x}$.
I haven't fully understood the motivation for using the factor, $\sigma\sqrt{r_t}$.
I know it's related to the standard deviation, but for now we'll take this as
gospel truth. 

The idea of the above is, once we get close to nil, the randomness of the Weiner
process is dampened by this factor.



## Code
<iframe frameborder="0" scrolling="no" style="width:100%; height:184px;" allow="clipboard-write" src="https://emgithub.com/iframe.html?target=https%3A%2F%2Fgithub.com%2FJamesTadleyGreen%2Fquant-haskell%2Fblob%2F54f7030dc274c4a94fd77acb39e9e045fce6c1c3%2Fsrc%2FMyLib.hs%23L32-L36&style=tomorrow-night-blue&type=code&showBorder=on&showLineNumbers=on&showFileMeta=on&showFullPath=on&showCopy=on"></iframe>

# Chen
A brief recap, we've introduced a faily simplistic model in [Vascik](#Vascik) and then
built upon this with factors effecting the Weiner process. There are several
other models we could create in a similar vein by multiplying the drift or
randomness by either constants or dependent factors. Alternatively, we
could look at making the variables $a$ and $b$ in the Vascik model dependent
on time, this results in the [Hull-White](https://en.wikipedia.org/wiki/Hull%E2%80%93White_model) model

Instead, we'll go one step further, let's make each of our variables, $\theta$
and $\sigma$ [Ornstein-Uhlenbeck](#ornstein-uhlenbeck) processes.

What does this look like?

We have our overall function, in a similar style to what we've seen before.
    $$\mathrm{d}r = \kappa (\theta_t - r_t) \mathrm{d}t + \sqrt{\sigma_t} \sqrt{r_t} \mathrm{d}W_1$$

> Note: We have the $\sqrt{r_t}$ factor, mirroring [CIR](#cir).

Now, what do $\theta_t$ and $\sigma_t$ correspond to?
    $$
    \begin{align*}
        \mathrm{d}\theta &= \nu (\zeta - \theta_t) \mathrm{d}t + \alpha \sqrt{\theta_t} \mathrm{d}W_2\\
        \mathrm{d}\sigma &= \mu (\beta - \sigma_t) \mathrm{d}t + \eta \sqrt{\sigma_t} \mathrm{d}W_3\\
    \end{align*}
    $$

As we expected they're a sudo-[CIR](#cir) type of equation.

Putting this into code, we again have the step function.

<iframe frameborder="0" scrolling="no" style="width:100%; height:184px;" allow="clipboard-write" src="https://emgithub.com/iframe.html?target=https%3A%2F%2Fgithub.com%2FJamesTadleyGreen%2Fquant-haskell%2Fblob%2F54f7030dc274c4a94fd77acb39e9e045fce6c1c3%2Fsrc%2FMyLib.hs%23L42-L46&style=tomorrow-night-blue&type=code&showBorder=on&showLineNumbers=on&showFileMeta=on&showFullPath=on&showCopy=on"></iframe>

The above is standard, but the below is new.

<iframe frameborder="0" scrolling="no" style="width:100%; height:205px;" allow="clipboard-write" src="https://emgithub.com/iframe.html?target=https%3A%2F%2Fgithub.com%2FJamesTadleyGreen%2Fquant-haskell%2Fblob%2F54f7030dc274c4a94fd77acb39e9e045fce6c1c3%2Fsrc%2FMyLib.hs%23L49-L54&style=tomorrow-night-blue&type=code&showBorder=on&showLineNumbers=on&showFileMeta=on&showFullPath=on&showCopy=on"></iframe>

By `zip`ing together the three random values we can 



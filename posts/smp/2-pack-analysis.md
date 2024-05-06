---
title: Pack Analysis
tags: work, smp, maths
---

# The problem
Working out if the packs used were the most efficient was a very hard task, before the
introduction of the below process, it was really 'finger-in-the-air' as to not only
whether a pack was efficient, but as to how long picking a set of packs for a given job
actually took. Our solution entailed; each 'picker' scanning each picklist at the start
and the end of the pick. 

Due to audit reasons, we had to label who completed each pack, this meant that there 
was already a large amount of 'admin' happening for each pick within a pack. So we were
confident that our solution wouldn't add too much overhead.

Additionally, the information gained by this process was invaluable. To see the progress
of a 'picking line' historically, the wharehouse manager had to physically poke his head
out the door and see the progress first hand. Stage 1 of delivery, completed in this 
order to get people onside, was a dashboard showing number of packs picked, a 
distribution of pack sizes, and most importantly an expected time of completion for each
job.

How did we calculate this expected time to completion for a job? Initially, a time-in-motion 
study was completed and used as a baseline estimate. This was my bosses' 
suggestion, and I think a good first stab, but I had grander plans envisioned for phase
II. This provided a rough estimate for when a job would be finished but was defintely
quite rough around the edges. All of the usual issues with monitored working were 
exemplified by this exercise:

- Workers packed faster due to the knowledge that they were being monitored in a study.
- Shorter runs were over-estimated due to limits in measuring with stopwatches.
- Due to limitations in time we could only measure a set number of 'element types', so the data captured wasn't as compelete as it could be.

All of the above aside we had some good data to work with to provide a tool to the 
business that it was serverly lacking, we could estimate and show when a job is likely
to be completed. The feedback was overwhelmingly positive, with everyone saying it gave them more insight,
adn the ability to plan work more effectivly. They said the 'estimated job completion'
was good albeit similar to a computer loading bar where it'd hang for a while before 
completing suddenly.

I had already 'primed' the managers we were deploying to, telling them the estimated
time to complete a job would be improving over time with the more data we gathered.
So people were more forgiving with errenous timings, excited to see how it'd change, 
and most surprisingly, were telling me they could see that the timings were improving
even before we rolled out the new timings module. This latter fact was an interesting
psychological effect I didn't foresee. I also wasn't concerened about this feedback,
since we were measuring scanning time for each pack, I had an objective way to measure
if our estimates were accurate and wasn't relying on human feedback.

How do we improve these estimates, we can know how long a pack takes to pick on a given
job, but there's no guarentee this pack will ever come up again. The combination of a 
flyer, notepad and cap might be unique to this campaign, whilst each of those items is
very common across the client's campaign's.

Instead we measured each item's individual pick time, and used a weighted average 
(favouring the more recent picks), to estimate time taken to pick. For a new item we 
chose an average of similar sized items for a baseline and then used this moving weighted
average to constantly update our estimate.

# Row-Echelon Form

Now hang on a minute, I hear you cry, how are you working out the individual item pick
times when you said you only had pack timings. This is correct, we only capture the pack
timings not the item timings. To get the individual pack times we have to hop into a GCSE 
maths lesson.

Consider a system of two simultaneous equations of two unknowns, e.g.:
$$
\begin{align*}
6x + 5y &= 16\\
x + y &= 3
\end{align*}
$$

This has a valid solution with $x=1$, $y=2$. Now, you could definitely solve this by 
hand (or even eye). But given a more complex system you would need an algorithm. This 
is where row operations, and reduced-row Echelon form come into play. See [here](https://www.ucl.ac.uk/~ucahmto/0007/_book/2-3-linear-equations-and-row-operations.html)
for more details. We can see that the system above would be represented as:
$$
\begin{pmatrix}
6 & 5 & 16\\
1 & 1 & 3\\
\end{pmatrix}
$$
and after reducing to row-echelon form we get:
$$
\begin{pmatrix}
1 & 0 & 1\\
0 & 1 & 2\\
\end{pmatrix}
$$
which gives exactly our result from above.

# Applying the Maths
How does the above help us? Given a system of simultaneous equations we can calculate
the individual 'weights'. If we consider a given pack with items within, we have the 
overall time to pick the pack, so can estimate the individual pack timings with the 
above methodology.

Then given a prior estimate, we can use [Bayesian Inference](https://en.wikipedia.org/wiki/Bayesian_inference)
to estimate the next picking times.

# Future Steps
Given more time I would want to take this project in another direction, currently the 
estimates update for each job, influenced most heavily by the most recent job. This 
doesn't account for someone having a 'bad day', seasonal changes, and other unmeasured
characteristics. Ideally, we could do some form of outlier estimation to discard erroneous
information on-the-fly.

We settled on using the assumption that picking times are normally distributed, but this
is unlikely to be the case. I imagine that they're more likely to be [chi-squared](https://en.wikipedia.org/wiki/Chi-squared_distribution).

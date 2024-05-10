---
title: API Archetect
tags: work, smp, api, python
---

# The Problem
Before I joined SMP, there were a scattering of technology projects across the business
mainly in individual python scripts, excel workbooks, or jupyter notebooks.
Whilst there was a lot of good work in these files, the disjoint nature of their 
locations resulted in many lost efficiencies.

Having a centralised policy solves many of the above issues.

# The Solution

Whilst there were some very bright people working on several projects, none had any 
experience with git or other version control system. Once I introduced this, we had to
go through several rounds of training to get people comfortable with using it in a 
daily capacity.

Unsurprisingly, due to the disjoint nature mentioned above, there was a lot of 
repetition or solving the same problem in different ways. Together with the team, we 
settled on using python as our main language, due to the existing knowledge within the 
business. Additionally, due to they myriad of systems we have to interact with, an API
seemed like the natural choice for interaction.

I chose [Flask](https://flask.palletsprojects.com/en/3.0.x/) as the framework, due to 
personal experience. I think if I were to repeat the experience I might use 
[Fast API](https://fastapi.tiangolo.com/) due to its more expansive boilerplating.
Very quickly we were up-and-running, with a live and dev server within our VPN network.

# Queuing Framework

Very quickly we had endpoints that very not very performant (through no fault of our
own), one example would be label generation. Creating labels might take c. 1 second via
API, and for some of our larger jobs we had to generate over 6000 labels! Having a job 
run on the API for an hour and a half isn't tenable. Firstly, we only have a certain 
amount of workers, and this will hold one of them for an extended period. Secondly, 
we have auto refresh of workers that spend too long on a task, setting this timeout to
longer than an hour means any workers who 'get stuck', have to wait a very long time to
have their resources freed up again.

The solution I settled on was to use [Celery](https://docs.celeryq.dev/en/stable/).
This allowed us to 'offload' long running tasks to celery workers, which could take as
long as needed to complete their tasks without impacting our flask server. Most of the 
API's function was to send data between systems, so, we didn't need to query a result 
of a task often. We simply needed to be confident that a task would complete at some 
point.

# Reporting

There are a couple of simple solutions we used to ensure our API was running smoothly,
we utilised [Flask Monitoring Dashboard](https://flask-monitoringdashboard.readthedocs.io/en/latest/)
to see a whole heap of information. The part I found most useful was the automatic 
profiling spread across differing commits, this allowed us to empirically show that as 
we added more commits, the speed of our API improved.

To achieve similar functionality within Celery, we used
[Flower](https://flower.readthedocs.io/en/latest/), unfortunately this doesn't have the 
same built in profiling so we had to create our own simple solution.

For a heartbeat check, we set up a raspberry pi in a corner of our office, and had it 
ping a simple endpoint to ensure that the server is up and running. This was the 
simplest solution we could think of that had the best time to payoff ratio. There was a 
risk of double failure as the raspberry pi is on the same network as the API, but this
was a trade-off we were willing to accept. At time of writing, we've never had this 
process fail.

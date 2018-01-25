---
layout: post
title: 'What about that pesky "month" feature?'
---

Ways to handle the day and month features in a timeseries dataset:

- Treat all months as constant (ie: "all months are 30 days") -- good but we loose information and February becomes an awkward month (also, leap years are not treated in a special fashion which might be important). Computationally efficient, semantically incorrect, losses information.

I think I see this one the most "in the wild" and it _always_ looks like a hack.

- Ignore the months feature.

This is a totally valid option, depending on your circumstance. Years only differ by plus-or-minus one, so you could just treat everything as a delta off of that. Usually you don't want to do this, though, since you would rely on a higher dimensionality of days to cover causal behaviors like month-associated holidays or "No Shave November." Perhaps prematurely bucketing days to months is unnecessary, but I don't think it's a bad option to encode things that we already know. I'm sure people make decisions based on extremes of a month, kind of like the story-internet-fact that cops are always trying to fill speeding ticket quotas at the end of each month. Computationally efficient(?), semantically correct, losses information.

- Treat each day the percent completion of the month.

This seems like the right answer, but it also takes more time to do the data processing. Not the most simple thing to whip up in a notebook, so perhaps this is why I haven't seen too much of this.



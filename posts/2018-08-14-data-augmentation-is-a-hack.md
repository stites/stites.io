---
layout: post
title: Data augmentation is a hack (albeit nessecary)
---

Ideally, models are invariant to translation, rotation, and scaling. You would hope that your learning model identifies you both in close-up photos, as well as in a group shots (even if one is just a photoshopped version of the other).

That said, convolutional layers are only translation-invariant -- and only somewhat translation-invariant at that! This is where data augmentation comes in -- making sure that our model is trained on data which would make models robust to translation, rotation, and scaling (hopefully resulting in invariance). In the end it's really just a hack that we all live with, everything works well enough with our current hack in place, but there are attempts like Capsule Networks which attempt to chip away at this hack.

Questions arise:
- What other attempts go about solving this problem?
- Can we train on manifolds, or simple dimension reduction, to look at higher-level representations to eliminate these?
- If we succeed in solving this, can we just 0-pad to infinity in all input dimensions and run training in each direction up to a stopping criteria?
- If the idea here is that it is easier to work problems manually, then automate, then formalize -- can we automate augmentation for NLP? 

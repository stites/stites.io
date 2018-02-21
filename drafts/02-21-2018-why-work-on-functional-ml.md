---
layout: post
title: Why work on functional ML?
---

Fine-tuning this comment with some of my thoughts as a RL/DRL practitioner who also migrates models to production, which is in Haskell. Primarily I'm addressing this comment because it's a little more down-to-earth than the blogpost : )

<!-- Parallelization at the language level can be really nice! While CV isn't really in my wheelhouse, there are certain tasks you can't always delegate to the GPU with your favorite tensor library (specifically image processing comes to mind). You're right that types don't just solve everything, but they sure can help. That said it's not _terribly_ important unless you are dealing with a production system or you haven't heard of your language's GPU-accelerated or multiprocessing-based image library. -->

What is 

---

I normally think of the unicorn "statically-typed nerual network library" as a bit of a relabelling from some better goal of Functional ML which includes being able to ship research models without thinking twice about the engineering quality. It just so happens that when a language offers dependent types, people jump on that idea and forget about how crafting your own neural network architecture doesn't always happen with linear composition. The closest w, this tool is torch in a friendlier language or pytorch in a faster one -- but the choice of language is very important since research usually includes a lot of throwaway code, which I still want to be able to write... I just also want to be able to ship it to production without too much hassel. A compiler can force you to check certain boxes as you work through examples.

So Yes, it can be nice to know your matrix dimensions ahead of time (and lean more on the compiler than intuition) and yes, it can nice to

---

> I promise you, people will find ways to write awful and confusing code in any language :)

Unless the language forces everyone to be confusing in the same way *cough* *haskell* *cough*

More seriously though, I think the dream of the "statically-typed nerual network library," for me, is more about the last point than anything else. I would love to be able to prototype something out and ship it to engineers without being concerned about code quality. It would be even cooler if those prototypes were built on zero-cost abstractions and satisfied some minimal requirement for high-performance code, even if doing pre- or post-processing. That's what FP (sort of) offers for engineering and, perhaps with a lot of hair pulling, what it might offer ML (or at least my day job).

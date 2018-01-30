---
layout: post
title: GeForce, Quadro and Tesla
---

I've been thinking of expanding my deep learning rig, repurposing one of my 1080Ti cards for mining (on a PCIe 8x lane), while bumping up that 16-lane slot to a more "professional" deep learning card. In the GeForce series, from what I understand, a [1080 Ti is already a good enough bang-for-your-buck][compare] and a Titan is only an [incremental][in1] [improvement][in2]. My coworker has tauted having a Quadro series (which might be less expensive than a Tesla), but ultimately [is better suited for 3d-acceleration than for deep learning][so]. Finally, there are Teslas, which are the top of the line scientific computing GPUs (and [all you will ever find in the cloud][cloud]).

While Teslas do seem to be marketed as the highest-end card, it looks like the GeForce line is just as competitive when you only look at floating-point operations. Teslas have optimized double-precision and will be impossible to beat there, if you can [sidestep the defaults of your programming language][nvidia] you can reap huge performance gains:

> **NOTE:** Be careful what platform you are working on and what the default precision is in it. For example, here in the CUDA forums (August 2016), one developer owns two Titan X's (GeForce series) and doesn't see a performance gain in any of their R or Python scripts. This is diagnosed as a result of R being defaulted to double precision, and has a worse performance on new GPU than their CPU (a Xeon processor). Tesla GPUs are cited as the best performance for double precision. In this case, converting all numbers to float32 increases performance from 12.437s with nvBLAS 0.324s with gmatrix+float32s on one TITAN X (see first benchmark).

Another thing to consider is that the Quadro has a huge amount of memory, which might be beneficial for future learning models.

[compare]: http://timdettmers.com/2017/04/09/which-gpu-for-deep-learning/
[in1]: http://forums.fast.ai/t/1080ti-announced-beats-titan-x/1663
[in2]: https://www.pugetsystems.com/labs/hpc/TitanXp-vs-GTX1080Ti-for-Machine-Learning-937/
[so]: https://stackoverflow.com/questions/34715055/choosing-between-geforce-or-quadro-gpus-to-do-machine-learning-via-tensorflow/34715397#34715397
[cloud]: https://www.pcgamesn.com/nvidia-geforce-server
[nvidia]: https://devtalk.nvidia.com/default/topic/959375/cuda-programming-and-performance/performance-issues-with-cuda-and-python-r/1

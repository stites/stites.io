---
layout: post
title: Minkowski Error
---

{{ page.title }}
================

Normally to handle outliers we use Turkey's method to detect extreme values.
Multivariate methods also exist to detect and remove outliers.

Minkowski error, however, allows us to keep our outliers and simply reduce the
impact they have on our data. Instead of measuring our model using the squared
error, we raise error to a power less than two: say 1.5. In this way, the
contribution that an outlier gives is lessened and we can keep the data. To put
this in concrete terms, an error of 10 raised to the 1.5 will contribute a sum
of only 31.62 as opposed to 100 (by sum of squares). [See][see].

[see]: http://www.kdnuggets.com/2017/01/3-methods-deal-outliers.html

---
title: Explaining Away in Bayes Nets
---

Here's a scenario from [knowitvideos](https://www.youtube.com/watch?v=EZpzEZPy0Wk) which is pretty bad, so a better derivation is the following:

```
S   R
 \ /
  H

P(S) = 0.7
P(R) = 0.01

P(H| S, R) = 1.0
P(H|-S, R) = 0.9
P(H| S,-R) = 0.7
P(H|-S,-R) = 0.1

P(R|S) = P(R) since there is no way for the two to co-occur

P(R|HS) = P(HS|R)*P(R) / P(HS)
        = P(HSR) / P(HS)
        = P(HSR) / (P(H|S) * P(S))
        = (P(H|RS) * P(RS)) / (P(H|S) * P(S))
        = (P(H|RS) * P(R|S) * P(S)) / (P(H|S) * P(S))
        = (P(H|RS) * P(R|S)) / P(H|S)
        = (P(H|RS) * P(R|S)) / (P(H|SR)*P(R) + P(H|S-R)*P(-R))
        = (1       * 0.01  ) / (1      *0.01 + 0.7     *0.99 )

P(R|H-S) = (P(H|R-S) * P(R|-S)) / (P(H|-SR)*P(R) + P(H|-S-R)*P(-R))
         = (    0.9  * 0.01   ) / (0.9     *0.01 +  0.1 *0.99)
         = 0.0833

P(R|H) = P(H|R)*P(R) / P(H)
       = P(H|R)*P(R) / (P(H|SR)*P(SR)+P(H|-SR)*P(-SR)+P(H|S-R)*P(S-R) +P(H|-S-R)*P(-S-R))
       = P(H|R)*P(R) / (P(H|SR)*P(S)P(R)+P(H|-SR)*P(-S)P(R)+P(H|S-R)*P(S)P(-R) +P(H|-S-R)*P(-S)P(-R))
       = (P(H|RS)*P(S) + P(H|R-S)*P(-S))*P(R) / (P(H|SR)*P(S)P(R)+P(H|-SR)*P(-S)P(R)+P(H|S-R)*P(S)P(-R) +P(H|-S-R)*P(-S)P(-R))
       = (0.7 + 0.9*0.3)*0.01 / (0.7*0.01+0.9*0.3*0.01+0.7*0.7*0.99+0.1*0.3*0.99)
       = 0.0185

```

[Here's another scenario to explain away things](http://www.eecs.qmul.ac.uk/~norman/BBNs/The_notion_of__explaining_away__evidence.htm)

```
A   B
 \ / \
  C   D
```



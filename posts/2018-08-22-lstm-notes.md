---
layout: post
title: Reviewing LSTMs with ascii art
---

Scenario: Say we have a network that watches TV and classifies if an object is a wolf, dog, bear, goldfish, or an unknown thing. With a simple feed forward, convolutional network we would look at snapshots of frames and make a prediction for each animal. This is pretty simplisitic.

Because we are working with an ordered sequence over time, though, we can feed in hints of what the network has seen before. For instance we know that there is a higher correlation between wolves and bears, as well as dogs and goldfish and we could use an RNN to take advantage of this so that, let's say it watches the Nature Channel, if it sees a bear then the next canine it sees will more likely be a wolf (and not a dog). In contrast, if it's watching The Dog Office, a cubical goldfish will suggest that this caninie is a dog.

In the wild, you won't see this happen because the world is full of objects that a network will see between the goldfish and the dog. Many objects will be classified under the "unknowns" that it will break the desired correlation -- this is where we would really like to have some notion of short-term memory. State, however, already exists in an RNN and it is a kind of "short-term memory" -- so we come up with Long Short-Term Memory units.

LSTMs are comprised of two states: short-term memory, and long-short term memory (aka "long term memory" to simplify). You can think of this as an RNN + an extra memory buffer. Each time a new state is observed, it goes through the following transformation:

```
                                   ,--------.
                                   | Output |
                                   `--------'
                                        ,
                                       / \
                                        |
,-------------------.     ,------------------------------.     ,-------------------.
| Long term memory  |---->| forget gate -> remember gate |---->| Long term memory  |
`-------------------'     |          \  __,              |     `-------------------'
                          |           \  /|              |
                          |            \/                |
                          |            /\                |
                          |           /  \|              |
,-------------------.     |          /  --'              |     ,-------------------.
| Short term memory |---->| learn gate  -> use gate      |---->| Short term memory |
`-------------------'     `------------------------------'     `-------------------'
                                        ,
                                       / \
                                        |
                                   ,--------.
                                   | Event  |
                                   `--------'
```

When an event is observed an LSTM unit will run the long term memory buffer through the forget gate, trying to discard irrelivant information, and will run the short-term memory buffer through the learn gate which is much like the original RNN structure where we concat the observation to the current window. Everything which is saved from the forget and learn gates are then concatenated together and passed in to the remember and use gate, which dictates what will be saved as the next state of the LSTM unit.

Chained together, it composes:
```
            pred_1               pred_2               pred_3
              |                    |                    |
,-------.  ,------.  ,-------.  ,------.  ,-------.  ,------.  ,-------.
| LTM_1 |->|      |->| LTM_2 |->|      |->| LTM_3 |->|      |->| LTM_4 |
`-------'  | LSTM |  `-------'  | LSTM |  `-------'  | LSTM |  `-------'
| STM_1 |->|      |->| STM_2 |->|      |->| STM_3 |->|      |->| STM_4 |
`-------'  `------'  `-------'  `------'  `-------'  `------'  `-------'
              |                    |                    |
            obs_1                obs_2                obs_3
```

<br/>

## The Learn Gate

Steps:

- given: $STM_{t-1}$ as the memory buffer at time $t-1$ and $E_t$ as the event at time $t$
- combine: $N_t' = tanh(W_n[STM_{t-1}, E_t] + b_n)$
- forget:  $N_t  = N_t' x sigmoid(W_i[STM_{t-1}, E_t]+ b_i)$ -- notice that sigmoid turns this into a linear combination (ie: how much to keep and how much to forget of each weight).

<br/>

## The Forget Gate

Steps:

- given: $LTM_{t-1}$ as the memory buffer at time $t-1$
- use the learn gate from below with sigmoid to find out how much to keep: $f_t = sigmoid(W_f[STM_{t-1}, E_t] + b_f)$
- and apply that to the $LTM_{t-1}$: $F_t  = LTM_{t-1} \dot f_t$

<br/>

## The Remember Gate

Steps:

- given: $LTM_{t-1}f_t$, the output of the forget gate, and $N_ti_t$, the output of the learn gate
- add them together: $LTM_t = LTM_{t-1}f_t + N_ti_t$

<br/>

## The Use Gate (or output gate)

Steps:

- given: $LTM_{t-1}$, $STM_{t-1}$, $E_t$
- find out how much to keep from the forget gate: $U_t = tanh(W_u LTM_{t-1} \dot f_t + b_u$
- find out how much to keep from short term memory and the event:
    $V_t = sigmoid(W_v [STM_{t-1}, E_t] + b_v$
- add them together $STM_t=U_t \dot V_t$

<br/>

## Other architectures

- Gated Recurrent Units: merge the $LTM$ and $STM$ buffers. In this situation "Learn" and "Forget" are combined into an "Update" gate, and "Remember" and "Use" are merged into a "Combine" gate.
[Also](http://www.cs.toronto.edu/~guerzhoy/321/lec/W09/rnn_gated.pdf), [also](http://despicableme.wikia.com/wiki/Felonius_Gru).

- Peephole connections: We never use the long-term memory in many of the gates above. If we do by concatenating all of $[LTM_{t-1},STM_{t-1},E_t]$, we arrive at LSTM with peephole connections.

That's it!


---

[Also](https://skymind.ai/wiki/lstm), [also](https://www.youtube.com/watch?v=iX5V1WpxxkY), [also](https://blog.echen.me/2017/05/30/exploring-lstms/), [also](https://colah.github.io/posts/2015-08-Understanding-LSTMs/).

And later, check out: [Attention mechanisms and Memory Networks](https://skymind.ai/wiki/attention-mechanism-memory-network)

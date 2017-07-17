policy + value is best, but policy is preferred over value function (most of the time). Of course this it dependent on your use case.

there are advantages like "policies can be more compact" - ie: "see X, move Left", whereas a value function says "this state is value X, let's calculate what is reasonable according to our policy (usually maxing)"

but the maxing is the interesting part: in a value function, you have to find out value of states (or maybe state-actions) but you, ultimately, wind up argmax-ing your solution. this is your policy. well, we can get that directly simply by solving the policy gradient instaed.

also, you wind up with better convergence properties. in value fucntions you solve for estimated value and advantage, but sometimes you get chatter (oscillation) of hyperparameters for non-linear approxmiators. this makes your function approximator (ie neural net) fall apart. This is also the reason for dueling networks.

can learn stochastic policies (deterministic is "predictable", but think about what that means for roc-paper-sisscors)

BUT

the garuntee is only on a local maximum, and ps it also can be ineefficient to caluclate (also, it might have high variance). there is no silver bullet.

BUT

if we combine the two, and use things like double-learning, fixed Q-targets, etc (those are value-function things only, i think), we get better stability

  simplex method for random forest optimisation?

actor-critic works because we are using policy gradients, which are slow, but then we are speeding up the process not by looking at current value, but by looking at _future_ values, estimated by the critic.

BUT

we need to use "compatible function approximation (which have no bias)" for value approzimation (ideally for both). for the garuntees around our value function converging on the true value function. This turns out to always be true if the _features_ to the value function are the _score function_

Eligibility Traces: build up history, then we want to train on scores that are largest, most frequent, and most recent. is an eligibility over our scores (not our states). for a policy gradient it is analgous to the value function (search-replace "gradient" with "score").



PG: sometimes mean, sometimes noise - very noisy. Taking expectations of gradients over noise is hard to estimate. accuracy is high >>= variance is high.
Instead, start off with deterministic policies, then adjust to make this more stochastic.
This is the _natural policy gradient_ finds the deterministic policy. (only works for continuous actions).
Just start with the grad of our own Q-function, critic gives grad to policy and says "if you just adjusted to the better gradient, you win" - because the critic has the "answer" (really critique). See Natural Actor Critic.
IE: update actor parameters in direction of critic parameters. (how does this work, b/c then it seems we are just training PG on expectation(????))


build out different types of ACs:
        Q Actor-Critic
Advantage Actor-Critic (A2C)
       TD Actor-Critic
 TD(lmda) Actor-Critic
  Natural Actor-Critic (NAC)

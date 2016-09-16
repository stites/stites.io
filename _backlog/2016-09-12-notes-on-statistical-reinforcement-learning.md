# Statiscial Reinforcement Learning

What is 
- correlating generic rewards with signals from past actions.
- working in unknown enviroments (no "all-knowing oracle")

usually, these are modeled as MDPs
requires: an unknown environment, a computer agent, an action, a control policy
action is taken, rewards are assigned, state is updated.
non-greedy algorithm: this allows us to optimize in the long-term and not the short-term
states, actions, transitions, rewards, policies, values

---

### Deterministic Example: Maze problem

we have a start and an end, we want to get from point A to point B:

    A  _     _ B
    _  _     _ _
    _  _  _  _ _
    _  _  _  _ _
    _  _     _ _

If we know the entire map, we can give B a reward of 1 and discount each
neighbor by 0.9, repeating until we make it to A. Starting at A, we then
recursively pick the neighbor with the highest value.

We can construct a deterministic state machine from this map with positions as
nodes, transitions as edges, and node-weights (think "variant of edgeweights")
as rewards.

### Markov Decision Processes: The Math
time-step: $t$
observed state $s_{t}$ contained in all possible states $S$
action $a_{t}$ contained in all possible actions $A$

reward, $r \in \mathbb{R}$ _reward space_:

    $r_t = r(\mathit{\boldsymbol{s}}_t, \mathit{a_t}, \mathit{\boldsymbol{s}}_{t+1}) \in \mathbb{R}$

The initial position of the agent is either drawn from the probability mass function if discrete if the state space is discrete, and from the probability density function if the state space is continuous. Sane stuff.

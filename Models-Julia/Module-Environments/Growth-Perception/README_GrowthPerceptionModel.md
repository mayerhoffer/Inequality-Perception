# Perception of growth in income-homophilic networks

Extension of the network model of inequality perception by Schulz, Mayerhoffer and Gebhard (2022).

In the original model, agents observe incomes only within their homophilic ego network, and their perception of inequality is biased because that network is not representative of the population. This project asks the same question for economic growth: can an agent who reasons as an ideal, unbiased statistician, but only sees her ego network, tell whether incomes grow linearly or multiplicatively? We test how the answer depends on income homophily, on who grows fastest (the growth regime), on noise, on network size and on what the agent observes. Perceived inequality is recorded alongside.

## Quick start (RUNME)

Run everything from the folder that contains the files. The Julia environment needs CSV, DataFrames and SpecialFunctions (Random and Statistics are standard libraries).

```powershell
# 1. Pilot, all blocks, 5 repetitions (roughly 40 min on 8 threads)
julia -t 8 --project="<environment>" GrowthPerceptionModel.jl 5

# 2. Full run, all blocks, 100 repetitions (roughly 20 times the pilot)
julia -t 8 --project="<environment>" GrowthPerceptionModel.jl 100

# 3. Selected blocks only
julia -t 8 --project="<environment>" GrowthPerceptionModel.jl 100 growth-regimes equal-weight-check
```

With fewer than 100 repetitions the output is written to `output/growth-perception-pilot/<block>/`, otherwise to `output/growth-perception-full/<block>/`.

## Repository layout

```
GrowthPerceptionModel.jl   the model: growth_perceptions_paired!, the time-resolved analysis
                           (time_resolved_perceptions!, squared_weights!, verdict_entropy!,
                           homophily_correlation!), summaries and CSV output; all parameters,
                           the seed and the four blocks are constants in the Parameters section
                           at the top; RUN THE MODEL at the bottom runs the selected blocks
InputIncomes.jl            starting income distributions and real_gini!
GrowthNetworks.jl          homophilic_linkage! (the network) and egosizes!
GrowthWorlds.jl            growth rates per regime, the paired linear and multiplicative worlds, noise
GrowthPerceptions.jl       observation rules, perceived Gini, the ideal-statistician classifier,
                           the signal of the signal-to-noise ratio, comparison classifiers
output/                    generated output (not in the repository)
```

## The model

Discrete time t = 0, 1, …, T with T = 100, and N = 1,000 agents.

| Symbol | Meaning |
|---|---|
| y_i(0) | starting income of agent i: exponential, rescaled to mean 1, sorted from poor to rich; the same population in every repetition |
| ψ | strength of the Matthew effect (0.005, 0.01, 0.02) |
| g_i | growth rate of agent i per period |
| σ | noise (volatility of the multiplicative shocks: 0, 0.01, 0.025, 0.05, 0.10) |
| ρ | homophily (0, 4, 14, ∞) |
| k | contacts chosen per agent (5; 3 to 20 in the contacts block) |
| N_i | ego network of agent i: herself and everyone linked to her |
| h | horizon: the number of periods observed so far (10, 20, …, 100) |

Growth rates. The rule of the original study's `MatthewGrowth.jl`: g_i = [(y_i(0) + 1)^(1+ψ) − 1 − y_i(0)] / y_i(0), so richer agents grow faster. The same set of rates is assigned in three regimes with the same mean rate:

| Regime | Assignment |
|---|---|
| `matthew` | each agent gets her own Matthew rate (the rich grow faster) |
| `uniform` | every agent gets the mean Matthew rate |
| `convergent` | the Matthew rates in reverse income order (the poor grow faster) |

Paired worlds. Every agent has an income history in two worlds with the same start and the same end:

- multiplicative: y_i(t) = y_i(0) (1 + g_i)^t
- linear: y_i(t) = y_i(0) + (t / T) [y_i(0) (1 + g_i)^T − y_i(0)]

The worlds differ only in the shape of growth, so a correct verdict cannot come from the start or end level.

Noise. Every agent carries one cumulative, mean-one factor F_i(t) = F_i(t − 1) exp(−σ²/2 + σ ε_i(t)), with F_i(0) = 1 and ε_i(t) ~ N(0, 1). Incomes in both worlds are multiplied by the same F_i(t), so the two worlds have the same realised shocks.

Network. One fixed, undirected network per repetition, built from starting incomes as in the original study. Every agent chooses k others without replacement, with probability proportional to exp(−ρ |y_i(0) − y_j(0)|); a chosen link is known to both. ρ = 0 gives a random network and ρ = ∞ links each agent to the k closest incomes. If fewer than k weights are positive (in the sparse rich tail), the agent links to the next-poorer agents in the income order, as in the original fallback.

Observation. Agent i observes one series x_i(t), set by the observation rule:

| Rule | x_i(t) |
|---|---|
| `localmean` | mean income of N_i |
| `localmedian` | median income of N_i |
| `growthrates` | geometric mean income of N_i, whose log-growth is the mean log-growth rate of the members (equal weight per member, as the average growth rate of the original `MatthewGrowth.jl`) |
| `own` | own income y_i(t) (no network) |

Verdict. The agent is an ideal statistician who knows the noise model. She uses the log-increments d_t = log x_i(t) − log x_i(t − 1), t = 1, …, h, which are independent under multiplicative noise, and fits two shapes by maximum likelihood:

- multiplicative: d_t = r + e_t
- linear: d_t = log[(1 + c t) / (1 + c (t − 1))] − v/2 + e_t

with e_t ~ N(0, v). Each shape has one trend parameter (r or c) plus the variance v. With ΔAIC = AIC_multiplicative − AIC_linear, the verdict is `linear` if ΔAIC > 2, `multiplicative` if ΔAIC < −2 and `indistinguishable` otherwise. A verdict is correct if it names the agent's world.

Perceived inequality. As in the original study: the mean absolute income difference over all pairs in N_i, divided by twice the mean income of N_i. The population Gini uses `real_gini!` of the original study.

## Definitions

These are the quantities reported in the output. They are our choices; the notes say where another choice is possible.

| Quantity | Definition |
|---|---|
| Share correct | share of agents (in a decile or condition) whose verdict names their world |
| Rich–poor gap | share correct in the richest income decile minus the poorest, per repetition, then averaged; deciles are by starting income |
| Growth-level error | (x_i(h) / x_i(0)) / (Ȳ(h) / Ȳ(0)) − 1, with Ȳ the population mean income of the agent's world |
| Time to statistical identification | the first horizon from which the verdict is correct at every later horizon |
| Verdict entropy | entropy (bits) of the distribution of verdicts within a decile: 0 if all agents agree, log2(3) = 1.58 if the three verdicts are equally common |
| Signal-to-noise ratio (SNR) | separation signal × √(effective sample size) / σ, per agent and horizon (below) |

Notes on these choices:

- Expanding windows: at horizon h the agent uses all periods 0 to h. She has no memory limit.
- Time to identification is averaged over the agents who are identified by period 100; the output also reports the share identified. Agents who are never identified are not included in the mean.
- Verdict entropy measures disagreement among agents in a decile. It is not the confidence of an individual agent.
- The ±2 band on ΔAIC is the usual threshold for a meaningful difference in AIC. A wider band moves agents from a correct or wrong verdict to `indistinguishable`.

## Signal-to-noise ratio

Definition. For agent i at horizon h:

- Separation signal (`separation_signal!`): the distance between the agent's noise-free observed history and the wrong growth shape, measured in log-increments, the data the classifier uses. For a multiplicative history it is the misfit of the best linear shape; for a linear history the misfit of the best constant growth rate; in both cases the square root of the summed squared misfit. SNR² is then roughly the ΔAIC the agent can expect.
- Effective sample size (`squared_weights!`): the observation rule averages the independent shocks of the members of N_i with weights w_j. The noise of a weighted mean scales with √(Σ w_j²), so the effective number of members averaged is 1 / Σ w_j². For `localmean`, w_j = y_j / Σ y, so a network with one rich contact counts as fewer members than its size. For `growthrates` it is the size of N_i, for `localmedian` the size × 2/π, for `own` 1. It is averaged over periods 0 to h and reported as `meaneffectivesize`.

Why this definition. An SNR explains accuracy if share correct is the same function of SNR in every condition, that is, if the curves of share correct against SNR overlap. We report the spread: the largest difference in share correct between conditions within one SNR bin (bins of 0.25 in log10 SNR). Each definition below replaced the previous one because its curves did not overlap in one dimension.

| Step | Definition | Regime and contacts | Horizon | ρ |
|---|---|---|---|---|
| 1 | curvature of the population path / noise (`curvaturenoiseratio`, one value per condition) | not agent-level | – | – |
| 2 | curvature of the agent's noise-free path × √(size of N_i) / (σ √(h/2)) | overlap (contacts 3 to 20 within 0.05) | no overlap: at log10 SNR 0, share correct 0.75 / 0.60 / 0.42 for horizons 10–30 / 40–60 / 70–100 | – |
| 3 | separation signal × √(size of N_i) / σ | regime: spread 0.017 | overlap: 0.63 / 0.64 / 0.63 at log10 SNR 0.25 (sandbox); spread 0.052 (full run) | no overlap: at log10 SNR 0, 0.29 in random networks and 0.43 under homophily; spread 0.148 (full run) |
| 4 | separation signal × √(effective sample size) / σ (current) | regime: overlap | overlap | overlap: spread 0.008 (pilot, 5 repetitions) |

The step-3 failure has a direct cause. In random networks the income-weighted mean averages about 5.8 effective members out of about 11, because the richest contacts dominate the mean. Under homophily the incomes in N_i are similar and the effective size is close to the actual size. With the effective sample size, ρ affects accuracy only through the SNR.

Falsification check (block `equal-weight-check`). If the rich–poor gap came from the income weights of the mean, it would disappear for the `growthrates` observer, who weights all members equally. The SNR curves of this observer also overlap across ρ (spread 0.014), and the gap remains (see Results).

Caveats:

- The effective sample size assumes small noise (the log of a weighted mean is close to the weighted mean of the log-shocks). The factor 2/π for the median holds for large networks; ego networks here are small (at least k + 1 members).
- Agents at equal SNR are a decomposition, not a separate effect: homophily acts partly through the SNR, so conditioning on SNR removes part of its effect.
- `curvaturenoiseratio` in the global and perception tables is the step-1 descriptor at population level, not the agent SNR.

## Design

All blocks share N = 1,000, T = 100, exponential start, ψ ∈ {0.005, 0.01, 0.02}, σ ∈ {0, 0.01, 0.025, 0.05, 0.10}, ρ ∈ {0, 4, 14, ∞}, both worlds and the ΔAIC band ±2.

| Block | Varies | Question | Time-resolved |
|---|---|---|---|
| `growth-regimes` | regime: matthew, uniform, convergent | is the rich–poor gap caused by who grows fastest? | yes |
| `contacts` | k: 3, 5, 10, 20 (matthew) | is it network size rather than homophily? | yes |
| `observers` | localmean, localmedian, growthrates, own (matthew) | does it depend on what agents observe? | no |
| `equal-weight-check` | regime, observer growthrates | does the gap depend on the income weights of the mean? | yes |

Seeds. The starting population is drawn once with seed 20261030. Repetition j draws its networks with seed 20261030 + 1000 + j (the same seed for every ρ and k) and one set of shocks with seed 20261030 + 2000 + j, shared by all conditions of that repetition (common random numbers). Results therefore do not depend on the number of threads.

## Edge cases

| Situation | What is reported |
|---|---|
| σ = 0 | the SNR is infinite; these conditions are left out of the SNR table |
| fewer than k positive network weights | fallback to the next-poorer agents in the income order |
| ρ = ∞ | each agent links to the k closest starting incomes; ties broken by income order |
| agent never identified by period 100 | counted in the share not identified, left out of the mean time to identification |
| linear fit | c is searched on z = log(1 + c T) in [min(z0, 0) − 3, max(z0, 0) + 3], with z0 the observed total log-growth (31-point grid, then 25 golden-section steps) |
| `own` observer | independent of the network; identical across ρ and k |

## Checks

The code was checked with a smoke test of 17 checks that is not part of this repository. They include: row lengths match headers in all eight tables; verdict shares sum to 1; joint growth-and-inequality shares sum to the share with rising perceived inequality; the `own` observer is the same in every network; noise-free population paths are classified correctly; the final horizon of the time table reproduces the full-window verdicts; share identified equals share correct at the final horizon; SNR bins add up to all agents; the horizon-0 homophily correlation is the same in both worlds and at every noise level; reruns give identical tables; and results are identical on 1 and 4 threads.

## Results

Rich–poor gap in share correct (richest minus poorest decile), pooled over ψ, σ > 0 and both worlds, with 95% intervals over repetitions.

| Observer, run | ρ | matthew | uniform | convergent |
|---|---|---|---|---|
| localmean, full (100) | 0 | +0.028 ± 0.002 | +0.004 ± 0.001 | −0.013 ± 0.002 |
| localmean, full (100) | 14 | +0.241 ± 0.002 | +0.029 ± 0.003 | −0.176 ± 0.003 |
| growthrates, pilot (5) | 0 | +0.020 ± 0.003 | −0.003 ± 0.005 | −0.023 ± 0.011 |
| growthrates, pilot (5) | 14 | +0.204 ± 0.019 | −0.017 ± 0.018 | −0.232 ± 0.012 |

Under homophily the better-informed group is the one whose income grows fastest, and the sign of the gap follows the growth regime. Without homophily the gap is close to zero. Income weighting adds a tilt towards the rich of +0.03 to +0.08 in every regime; this is the small uniform-regime gap of the `localmean` observer.

Notes for interpretation:

- With σ > 0 the uniform regime does not keep the population Gini constant, because the shocks spread incomes. Perceived inequality rises over time only when σ > 0.
- The `growthrates` observer perceives a growth level of about exp(−σ²T/2) times the population growth in every network (volatility drag: the geometric mean grows more slowly than the arithmetic mean; −11% at σ = 0.05 and −37% at σ = 0.10 over 100 periods). Growth-level errors are therefore compared with the `localmean` observer.

## Differences from the original code

- Contacts are drawn with log(u)/weight keys (Efraimidis–Spirakis) instead of `StatsBase.sample`. The probabilities are the same; the random draws differ.
- `homophilic_linkage!` has the same name as in the original code but takes k as an argument and returns only the ego networks. `real_gini!` is the same formula. Loading both sets of modules in one session gives name clashes.
- The time-resolved summaries are averaged inside the model (with a lock) and rounded to 10 significant digits, so reruns give identical files.

## References

Schulz, J., D. M. Mayerhoffer, and A. Gebhard. 2022. A network-based explanation of inequality perceptions. Social Networks 70: 306–324. arXiv:2203.14254.

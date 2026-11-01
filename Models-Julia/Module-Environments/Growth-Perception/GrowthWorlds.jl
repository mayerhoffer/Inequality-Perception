module GrowthWorlds

using Statistics

export generate_matthew_growth!, regime_growth_rates!, paired_growth_histories!, apply_stochasticity!


## --------------------------------------------------------------------- Growth rates ---------------------------------------------------------------------

## The Matthew-growth rule of the original study (MatthewGrowth.jl): the one-step counterfactual (Y + 1)^(1 + psi) - 1 as a proportional growth rate of Y.
## Richer people grow faster when psi > 0.
function generate_matthew_growth!(inputdistribution, psi)
    newvalues = (inputdistribution .+ 1) .^ (1 + psi) .- 1
    growthrate = (newvalues .- inputdistribution) ./ inputdistribution
    return growthrate
end

## The same set of Matthew rates, assigned in three ways ("regimes"). All three have the same mean growth rate.
##   "matthew":    richer people grow faster
##   "uniform":    everybody grows at the mean Matthew rate
##   "convergent": the Matthew rates in reverse income order, so poorer people grow faster
function regime_growth_rates!(Y, psi, regime)
    matthew = generate_matthew_growth!(Y, psi)
    if regime == "matthew"
        return matthew
    elseif regime == "uniform"
        return fill(mean(matthew), length(Y))
    else
        order = sortperm(Y)                       ## poorest first
        growthrate = zeros(length(Y))
        growthrate[order] = matthew[reverse(order)]
        return growthrate
    end
end


## --------------------------------------------------------------------- Paired worlds ---------------------------------------------------------------------

## Two worlds with the same start and the same end for every person; they differ only in the shape of growth.
## Multiplicative: every person compounds her growth rate. Linear: a straight line to the same final income.
function paired_growth_histories!(Y, growthrates, periods)
    population = length(Y)
    linear = zeros(population, periods + 1)
    multiplicative = zeros(population, periods + 1)
    finalincomes = Y .* (1 .+ growthrates) .^ periods
    for t in 0:periods
        multiplicative[:, t + 1] = Y .* (1 .+ growthrates) .^ t
        linear[:, t + 1] = Y .+ (t / periods) .* (finalincomes .- Y)
    end
    return linear, multiplicative
end

## Applies one cumulative mean-one GBM factor per person to both worlds (the same realised shocks in both worlds).
## innovations is population x periods of standard-normal draws.
function apply_stochasticity!(linear, multiplicative, noise, innovations)
    population, timepoints = size(linear)
    factors = ones(population, timepoints)
    for t in 1:(timepoints - 1)
        factors[:, t + 1] = factors[:, t] .* exp.(-0.5 * noise^2 .+ noise .* innovations[:, t])
    end
    return linear .* factors, multiplicative .* factors
end

end ## module

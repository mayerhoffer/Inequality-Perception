module GrowthPerceptions

using Statistics

export observed_histories!, gini_perceptions_at!, growth_distance!, classify_shape!, midpoint_convexity!,
       increments_deltaaic!, separation_signal!, nls_deltaaic!, logols_deltaaic!


## --------------------------------------------------------------------- What the observer sees ---------------------------------------------------------------------

## Observed history of every person (population x time) under one observation rule:
##   "localmean":   mean income of the ego network (self included); income-weighted, so the richest contacts dominate
##   "localmedian": median income of the ego network; robust to one rich contact
##   "growthrates": average growth rate of the ego network, as in the original MatthewGrowth.jl. Computed as the geometric
##                  mean income, whose log-growth is exactly the mean log-growth rate of the members (equal weight per person)
##   "own":         the person's own income only (no network; benchmark)
function observed_histories!(history, linksPerNodeList, observer)
    if observer == "own"
        return copy(history)
    end
    population, timepoints = size(history)
    source = history
    if observer == "growthrates"
        source = log.(history)
    end
    observed = zeros(population, timepoints)
    for t in 1:timepoints
        for i in 1:population
            myvisiblenodes = source[linksPerNodeList[i], t]
            if observer == "localmedian"
                observed[i, t] = median(myvisiblenodes)
            else
                observed[i, t] = mean(myvisiblenodes)
            end
        end
    end
    if observer == "growthrates"
        observed = exp.(observed)
    end
    return observed
end

## Perceived Gini of every person at period index t, with the formula of the original study (gini_perceptions!):
## the mean absolute difference over all pairs in the ego network, divided by twice the ego-network mean.
function gini_perceptions_at!(history, linksPerNodeList, t)
    population = length(linksPerNodeList)
    perceptionlist_gini = zeros(population)
    for i in 1:population
        myvisiblenodes = history[linksPerNodeList[i], t]
        n = length(myvisiblenodes)
        differencesum = 0.0
        for k in 1:(n - 1)
            for l in (k + 1):n
                differencesum += abs(myvisiblenodes[k] - myvisiblenodes[l])
            end
        end
        perceptionlist_gini[i] = (differencesum / (n * (n - 1) / 2)) / (2 * mean(myvisiblenodes))
    end
    return perceptionlist_gini
end

## Root-mean-square distance between two growth-index histories of every person (each row divided by its first value)
function growth_distance!(firsthistory, secondhistory)
    firstindex = firsthistory ./ firsthistory[:, 1]
    secondindex = secondhistory ./ secondhistory[:, 1]
    return vec(sqrt.(mean((firstindex .- secondindex) .^ 2, dims=2)))
end


## --------------------------------------------------------------------- Reading the growth shape ---------------------------------------------------------------------

## Turns a delta-AIC (multiplicative minus linear) into the verdict: "linear" above the threshold, "multiplicative" below
## minus the threshold, "indistinguishable" in between
function classify_shape!(deltaaic, threshold)
    if deltaaic > threshold
        return "linear"
    elseif deltaaic < -threshold
        return "multiplicative"
    else
        return "indistinguishable"
    end
end

## Model-free curvature: gap between the chord from start to end and the normalised history at the midpoint.
## Positive values mean the history bends upwards (convex), as multiplicative growth does.
function midpoint_convexity!(values)
    n = length(values)
    midpoint = cld(n, 2)
    chord = 1 + (midpoint - 1) / (n - 1) * (values[end] / values[1] - 1)
    return chord - values[midpoint] / values[1]
end

## THE IDEAL STATISTICIAN: delta-AIC (multiplicative minus linear) from the likelihood of the log-increments
## d_t = log(y_t) - log(y_(t-1)). The observer knows that incomes carry mean-one multiplicative (GBM) noise, under which
## the log-increments are independent:
##   multiplicative shape:  d_t = g - v/2 + e_t
##   linear shape:          d_t = log(1 + c t) - log(1 + c (t - 1)) - v/2 + e_t,      e_t ~ N(0, v)
## Each shape has one trend parameter plus the noise variance v, all estimated by maximum likelihood.
function increments_deltaaic!(values)
    T = length(values) - 1
    d = zeros(T)
    for t in 1:T
        d[t] = log(values[t + 1]) - log(values[t])
    end
    ## Multiplicative shape: with r = g - v/2 the estimates are the mean and the (ML) variance of the increments
    vexp = max(mean((d .- mean(d)) .^ 2), 1e-300)
    loglikexp = -T / 2 * (log(2 * pi * vexp) + 1)
    ## Linear shape: one-dimensional search over z = log(1 + c T), the log of the implied total growth factor
    zstart = log(values[end] / values[1])
    bestz, negloglin = grid_golden_minimise!(z -> linear_negloglik!(d, z), min(zstart, 0.0) - 3.0, max(zstart, 0.0) + 3.0, 31, 25)
    return 2 * (-negloglin - loglikexp)
end

## Negative log-likelihood of the log-increments under the linear shape, with v at its optimum for this z.
## One pass without temporary arrays, because it is evaluated about 55 times for every person.
function linear_negloglik!(d, z)
    T = length(d)
    c = (exp(z) - 1) / T
    s1 = 0.0
    s2 = 0.0
    for t in 1:T
        u = d[t] - log((1 + c * t) / (1 + c * (t - 1)))
        s1 += u
        s2 += u * u
    end
    m2 = s2 / T
    v = max(2 * m2 / (1 + sqrt(1 + m2)), 1e-300)           ## root of v^2 + 4v - 4 m2 = 0, written to stay exact
    return T / 2 * log(2 * pi * v) + (s2 + v * s1 + T * v^2 / 4) / (2 * v)
end

## SIGNAL of the signal-to-noise ratio: how far a noise-free observed history is from the WRONG growth shape, measured in the
## data the ideal statistician uses (the log-increments). For a multiplicative history: the misfit of the best linear shape;
## for a linear history: the misfit of the best constant growth rate. Returned as the square root of the summed squared misfit,
## so that (signal * sqrt(effective sample size) / noise)^2 is roughly the delta-AIC the statistician can expect
## (effective sample size: see squared_weights! in GrowthPerceptionModel.jl).
function separation_signal!(noiselesspath, world)
    T = length(noiselesspath) - 1
    d = zeros(T)
    for t in 1:T
        d[t] = log(noiselesspath[t + 1]) - log(noiselesspath[t])
    end
    if world == "linear"
        return sqrt(sum((d .- mean(d)) .^ 2))
    else
        zstart = log(noiselesspath[end] / noiselesspath[1])
        bestz, misfit = grid_golden_minimise!(z -> linear_increment_misfit!(d, z), min(zstart, 0.0) - 3.0, max(zstart, 0.0) + 3.0, 31, 25)
        return sqrt(misfit)
    end
end

## Summed squared distance between log-increments d and the increments of the linear shape with z = log(1 + c T)
function linear_increment_misfit!(d, z)
    T = length(d)
    c = (exp(z) - 1) / T
    misfit = 0.0
    for t in 1:T
        misfit += (d[t] - log((1 + c * t) / (1 + c * (t - 1))))^2
    end
    return misfit
end

## COMPARISON (levels, least squares for both shapes): treats the residuals as independent, so it is overconfident under GBM noise
function nls_deltaaic!(values)
    y = values ./ values[1]
    n = length(y)
    time = collect(0.0:(n - 1))
    intercept, slope = ols_line!(time, y)
    linearsse = sum((y .- intercept .- slope .* time) .^ 2)
    logintercept, startrate = ols_line!(time, log.(y))
    function exponentialsse!(r)
        e = exp.(r .* time)
        a = sum(y .* e) / sum(e .^ 2)
        return sum((y .- a .* e) .^ 2)
    end
    width = max(0.05, 2 * abs(startrate))
    bestrate, exponentialsse = grid_golden_minimise!(exponentialsse!, startrate - width, startrate + width, 41, 40)
    return n * log(max(exponentialsse / n, eps())) - n * log(max(linearsse / n, eps()))
end

## COMPARISON: multiplicative shape fitted by OLS on log(y) but scored in levels; biased towards "linear"
function logols_deltaaic!(values)
    y = values ./ values[1]
    n = length(y)
    time = collect(0.0:(n - 1))
    intercept, slope = ols_line!(time, y)
    linearsse = sum((y .- intercept .- slope .* time) .^ 2)
    logintercept, logslope = ols_line!(time, log.(y))
    exponentialsse = sum((y .- exp.(logintercept .+ logslope .* time)) .^ 2)
    return n * log(max(exponentialsse / n, eps())) - n * log(max(linearsse / n, eps()))
end


## --------------------------------------------------------------------- Helper Functions ---------------------------------------------------------------------

## Ordinary least squares line y = intercept + slope * x
function ols_line!(x, y)
    meanx = mean(x)
    meany = mean(y)
    slope = sum((x .- meanx) .* (y .- meany)) / sum((x .- meanx) .^ 2)
    return meany - slope * meanx, slope
end

## Golden-section search for the minimum of a one-dimensional function f on [a, b]
function golden_minimise!(f, a, b, iterations)
    ratio = (sqrt(5) - 1) / 2
    c = b - ratio * (b - a)
    d = a + ratio * (b - a)
    fc = f(c)
    fd = f(d)
    for i in 1:iterations
        if fc < fd
            b = d
            d = c
            fd = fc
            c = b - ratio * (b - a)
            fc = f(c)
        else
            a = c
            c = d
            fc = fd
            d = a + ratio * (b - a)
            fd = f(d)
        end
    end
    if fc < fd
        return c, fc
    else
        return d, fd
    end
end

## Coarse grid followed by a golden-section refinement between the neighbours of the best grid point
function grid_golden_minimise!(f, lower, upper, gridpoints, iterations)
    grid = collect(range(lower, upper, length=gridpoints))
    values = [f(x) for x in grid]
    best = argmin(values)
    x, fx = golden_minimise!(f, grid[max(1, best - 1)], grid[min(gridpoints, best + 1)], iterations)
    if fx < values[best]
        return x, fx
    else
        return grid[best], values[best]
    end
end

end ## module

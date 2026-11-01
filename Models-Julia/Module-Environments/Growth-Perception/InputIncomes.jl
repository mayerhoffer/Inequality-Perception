module InputIncomes

using Random, Statistics

export income_onegroup_exponential!, income_onegroup_lognormal!, real_gini!


## Exponential income distribution with mean 1, sorted from poor to rich (Gini of about 0.5)
function income_onegroup_exponential!(population)
    y = randexp(population)
    y = y / mean(y)
    y = sort(y)
    return y
end

## Lognormal income distribution with mean 1, sorted from poor to rich. For a target Gini use sigma = 2 * erfinv(gini).
function income_onegroup_lognormal!(population, sigma)
    y = exp.(sigma .* randn(population))
    y = y / mean(y)
    y = sort(y)
    return y
end

## Calculates the real gini of an income distribution (the formula of the original perception-of-inequality study)
function real_gini!(Y)
    n = length(Y)
    xx = sort(Y)
    return 2 * (sum(collect(1:n) .* xx)) / (n * sum(xx)) - 1
end

end ## module

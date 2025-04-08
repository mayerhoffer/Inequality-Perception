module InputDistributions

using StatsBase, Combinatorics, Distributions

export income_onegroup_exponential!, income_onegroup_lognormal!, income_pareto_tail!, real_gini!

## Basic one-group functions

## Exponential
function income_onegroup_exponential!(population)
    exb = Exponential(1)
    y = rand(exb,population)
    y = y/mean(y)
    y  = sort(y)
    return y
end

## LogNormal
function income_onegroup_lognormal!(population,sigma)
    lognorm = LogNormal(1,sigma)
    y = rand(lognorm,population)
    y = y/mean(y)
    y  = sort(y)
    return y
end

## Beta Distribution

# alpha = beta = 0.5


## Multigroup functions

## Draws an income distribution with mean 1 and consisting of "population" individual values where the proportion "p" is in the Pareto tail and the rest comes from the exponential distribution and "m" is the markup defining the distribution in the tail.
function income_pareto_tail!(p,m,population)
    exb = Exponential(1)
    y = rand(exb,population)
    y  = sort(y)
    if p > 0
        paredo = Pareto(m*(p-1)/(p*harmonic((1-p)*population)+m*p-m),  
        harmonic((1-p)*population) / ((1-p)*(1+m)) )
        y = y[1:find_workerPopulation!(p,population)]
        y = y/(mean(y)*(1+m)*(1-p))
        ycap = rand(paredo,Int(round(p*population)))
        ycap = (ycap/mean(ycap))*(m/((1+m)*p))
        ycap - sort(ycap)
        yall = vcat(y,ycap)
        yall = sort(yall)
        y  = sort(y)
        return y,yall,ycap
    else
        y = y/mean(y)
        return y
    end
end

function find_workerPopulation!(p,population)
    return Int(round((1-p)*population))
end


## Calculates the real gini of an income distribution
function real_gini!(Y)
    n = length(Y)
    xx = sort(Y)
  2*(sum(collect(1:n).*xx))/(n*sum(xx))-1
end

end



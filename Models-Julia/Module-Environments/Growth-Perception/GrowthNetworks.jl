module GrowthNetworks

using Random

export homophilic_linkage!, egosizes!


## --------------------------------------------------------------------- Network Generation ---------------------------------------------------------------------

## Builds one fixed, undirected, income-homophilic network and returns the ego network of every person (self included).
## Every person chooses `contacts` others with weights exp(-Rho * |income difference|); a chosen link is known to both.
## Rho = 0 gives a random network, Rho = Inf the perfect homophily network (the `contacts` closest incomes).
## Y must be sorted from poor to rich (as returned by InputIncomes), because the fallback uses the income order.
function homophilic_linkage!(Y, Rho, contacts)
    population = length(Y)
    linkspernode = [Set([i]) for i in 1:population]          ## nodes always know about themselves
    for i in 1:population
        candidates = [j for j in 1:population if j != i]
        distances = abs.(Y[candidates] .- Y[i])
        if isinf(Rho)
            chosen = candidates[sortperm(distances)[1:contacts]]
        else
            weights = exp.(-Rho .* distances)
            if count(>(0), weights) >= contacts
                chosen = weighted_sample!(candidates, weights, contacts)
            else
                chosen = lower_neighbours!(i, contacts, population)
            end
        end
        for j in chosen
            push!(linkspernode[i], j)
            push!(linkspernode[j], i)
        end
    end
    linksPerNodeList = []
    for i in 1:population
        push!(linksPerNodeList, sort(collect(linkspernode[i])))
    end
    return linksPerNodeList
end


## --------------------------------------------------------------------- Helper Functions ---------------------------------------------------------------------

## Draws `contacts` candidates without replacement, with probabilities proportional to the weights
## (Efraimidis-Spirakis: every candidate gets the key log(u) / weight and the largest keys are kept).
function weighted_sample!(candidates, weights, contacts)
    keys = zeros(length(candidates))
    for k in 1:length(candidates)
        if weights[k] > 0
            keys[k] = log(rand()) / weights[k]
        else
            keys[k] = -Inf
        end
    end
    return candidates[sortperm(keys, rev=true)[1:contacts]]
end

## Fallback of the original study (samps!): if too few weights are positive (typically in a sparse rich tail), link to the
## next-poorer people in the sorted income list. For the very poorest, the next-richer people complete the list.
function lower_neighbours!(i, contacts, population)
    neighbours = []
    for j in (i - contacts):(i - 1)
        if j >= 1
            push!(neighbours, j)
        end
    end
    j = i + 1
    while length(neighbours) < contacts && j <= population
        push!(neighbours, j)
        j = j + 1
    end
    return Int.(neighbours)
end

## Size of every ego network (self included)
function egosizes!(linksPerNodeList)
    return [length(links) for links in linksPerNodeList]
end

end ## module

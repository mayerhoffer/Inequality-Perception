using StatsBase, Random

using .InputDistributions, .HomophilicNetworks, .IndividualPerceptions


## Define function
function calculate_utility!(ownincomes, minperceptions, maxperceptions, alpha, beta)
    utilitysum = 0
    for i in 1:length(ownincomes)
        #currentutility = ownincomes[i] ^ (beta) * (alpha * maxperceptions[i] + (1 - alpha) * minperceptions[i]) ^ (1 - beta)
        currentutility = log(ownincomes[i]) * (beta) + log( maxperceptions[i] ^ alpha * minperceptions[i] ^ (1 - alpha) ) * (1 - beta)
        utilitysum = utilitysum + currentutility
    end
    return utilitysum
end

function calculate_utilities!(ownincomes, minperceptions, maxperceptions, alphalist, betalist)
    utilitiesperbetaperalpha = []
    for i in 1:length(betalist)
         betasperalpha = []
        for j in 1:length(alphalist)
            utilnow = calculate_utility!(ownincomes, minperceptions, maxperceptions, alphalist[j], betalist[i])
            push!(betasperalpha,utilnow)
        end
        push!(utilitiesperbetaperalpha, betasperalpha)
    end
    return utilitiesperbetaperalpha
end


function tunnel_perceptions!(repetitions,population,Rho,sigmaList,alphalist,betalist)
    u = ReentrantLock()
    alphalength = length(alphalist)
    betalength = length(betalist)
    simulationresults = [[] for i=1:(length(sigmaList) * repetitions * alphalength * betalength + 1)]
    simulationresults[1]=["sigma", "seed", "Rho", "alpha", "beta", "utilitysum"]
    for i in 1:length(sigmaList)
        thissigma = sigmaList[i]
         ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
        #Threads.@threads for j in 1:repetitions
        for j in 1:repetitions 
            Random.seed!(j)
            Y = income_onegroup_lognormal!(population,thissigma)
            linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(Y,Rho)
            perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty = distri_perceptions!(find_visible_nodes!(Y,linkspernodeTHISRUN))
            for k in 1:betalength
                for l in 1:alphalength
                    thissum = calculate_utility!(Y, perceptionlist_minproperty, perceptionlist_maxproperty, alphalist[l], betalist[k])
                    thisrun = []
                    push!(thisrun, thissigma, j, Rho, alphalist[l], betalist[k], thissum)
                    simulationresults[1 + (i - 1) * repetitions * betalength * alphalength + (k-1) * repetitions * alphalength + (l-1) * repetitions + j] = thisrun
                end
            end
        end
    end
    # simulationresultsnobrackets = []
    # for i in simulationresults
    #     push!(simulationresultsnobrackets,i...)
    # end
    # return simulationresultsnobrackets
    return simulationresults
    #income = income_onegroup_lognormal!(population,sigma)
end


function testblubb!(ownincomes, minperceptions, maxperceptions, alpha, beta)
    return alpha + beta
end

## Run the simulations
ie = income_onegroup_lognormal!(1000,0.6)

real_gini!(ie)

links, linkspernode = homophilic_linkage!(ie,8)


perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty = distri_perceptions!(find_visible_nodes!(ie,linkspernode))


calculate_utility!(ie, perceptionlist_minproperty, perceptionlist_maxproperty, 0.9, 0)


utilities = calculate_utilities!(ie, perceptionlist_minproperty, perceptionlist_maxproperty, [0, 0.5, 1], [0, 0.5, 1])


## repetitions,population,Rho,sigmaList,alphalist,betalist
test = tunnel_perceptions!(2,1000,1,[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2],alphas,betas)

alphas = []
push!(alphas,0)
for i in 1:50
    push!(alphas,i/50)
end
betas = alphas

ouputdf_min = outputdf

## Transform into df for vosualisation
using DataFrames, Plots, Statistics

header = test[1]                    # Extract header
rows = test[2:end]                  # Extract rows
outputdf = DataFrame([Symbol(h) => [row[i] for row in rows] for (i, h) in enumerate(header)])


function create_overviewplot!(alphaofinterest, betaofinterest,df)  
    selected_outputdf = filter(row -> row.alpha == alphaofinterest && row.beta == betaofinterest, df)
    
    scatter(
    selected_outputdf.sigma,              # x-axis
    selected_outputdf.utilitysum,         # y-axis
    xlabel = "sigma",
    ylabel = "utilitysum",
    title = "utilitysum vs sigma (α = $(alphaofinterest), β = $(betaofinterest))",
    legend = false,
    markerstrokewidth = 0.5,
    markersize = 6
    )

    grouped_df = combine(groupby(selected_outputdf, :sigma), :utilitysum => mean => :mean_utilitysum)

    plot!(
    grouped_df.sigma,
    grouped_df.mean_utilitysum,
    seriestype = :line,
    lw = 2,
    marker = :circle,
    markersize = 7,
    label = "mean utilitysum"
    )
end

create_overviewplot!(0.0, 0.5, ouputdf_min)


function create_heatmap_by_beta!(betaofinterest, df)
    # Filter by beta
    selected_df = filter(row -> row.beta == betaofinterest, df)

    # Group by (alpha, sigma) and compute mean utilitysum
    grouped_df = combine(groupby(selected_df, [:alpha, :sigma]), 
                         :utilitysum => mean => :mean_utilitysum)

    # Get unique sorted values for heatmap axes
    alphas = sort(unique(grouped_df.alpha))
    sigmas = sort(unique(grouped_df.sigma))

    # Create a matrix for the heatmap: rows = alpha, columns = sigma
    heat_values = [ 
        grouped_df[(grouped_df.alpha .== a) .& (grouped_df.sigma .== s), :mean_utilitysum][1] 
        for a in alphas, s in sigmas
    ]

    # Plot the heatmap
    heatmap(
        sigmas,          # x-axis (sigma)
        alphas,          # y-axis (alpha)
        heat_values,     # color (mean utilitysum)
        xlabel = "sigma",
        ylabel = "alpha",
        title = "Mean utilitysum by sigma and alpha (β = $(betaofinterest))",
        colorbar_title = "mean utilitysum"
    )
end


create_heatmap_by_beta!(0.9, ouputdf_min)





function find_nonmonotonic_alpha_beta_combinations(df)
    results = []

    # Get unique (alpha, beta) pairs
    unique_pairs = unique(select(df, [:alpha, :beta]))

    for row in eachrow(unique_pairs)
        a = row.alpha
        b = row.beta

        # Filter for this (alpha, beta)
        subset = filter(r -> r.alpha == a && r.beta == b, df)

        # Group by sigma and compute mean utilitysum
        grouped = combine(groupby(subset, :sigma), :utilitysum => mean => :mean_utilitysum)
        sort!(grouped, :sigma)

        utilities = grouped.mean_utilitysum

        if length(utilities) < 3
            continue  # Not enough points to check for monotonicity
        end

        # Check if strictly increasing or decreasing
        diffs = diff(utilities)
        is_increasing = all(>=(0), diffs)
        is_decreasing = all(<=(0), diffs)

        if !(is_increasing || is_decreasing)
            push!(results, (alpha = a, beta = b))
        end
    end

    return DataFrame(results)
end

find_nonmonotonic_alpha_beta_combinations(outputdf)

## Für rho geg.
## Sigma variieren => NW
## Für jedes NW => alle alpha und beta

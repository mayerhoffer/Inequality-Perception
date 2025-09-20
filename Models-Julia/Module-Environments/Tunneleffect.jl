using StatsBase, Random, SpecialFunctions

using .InputDistributions, .HomophilicNetworks, .IndividualPerceptions


## Define function
function calculate_utility!(ownincomes, minperceptions, maxperceptions, meanperceptions, alpha, beta, gamma)
    utilitysum = 0
    for i in 1:length(ownincomes)
        currentutility = ownincomes[i] + gamma * (alpha * maxperceptions[i] + beta * meanperceptions[i] + (1 - alpha - beta) * minperceptions[i])
        #currentutility = log(ownincomes[i]) * (beta) + log( maxperceptions[i] ^ alpha * minperceptions[i] ^ (1 - alpha) ) * (1 - beta)
        utilitysum = utilitysum + currentutility
    end
    return utilitysum
end

function calculate_utilities!(ownincomes, minperceptions, maxperceptions, meanperceptions, alphalist, betalist, gamma)
    utilitiesperbetaperalpha = []
    for i in 1:length(betalist)
         betasperalpha = []
        for j in 1:length(alphalist)
            utilnow = calculate_utility!(ownincomes, minperceptions, maxperceptions, meanperceptions, alphalist[j], betalist[i], gamma)
            push!(betasperalpha,utilnow)
        end
        push!(utilitiesperbetaperalpha, betasperalpha)
    end
    return utilitiesperbetaperalpha
end


function tunnel_perceptions!(repetitions,population,Rho,sigmaList,alphalist,betalist,gamma)
    u = ReentrantLock()
    alphalength = length(alphalist)
    betalength = length(betalist)
    simulationresults = [[] for i=1:(length(sigmaList) * repetitions * alphalength * betalength + 1)]
    simulationresults[1]=["sigma", "seed", "Rho", "alpha", "beta", "gamma", "utilitysum"]
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
                    thisrun = []
                    if alphalist[l] + betalist[k] <= 1
                        thissum = calculate_utility!(Y, perceptionlist_minproperty, perceptionlist_maxproperty, perceptionlist_meanproperty, alphalist[l], betalist[k],gamma)
                        push!(thisrun, thissigma, j, Rho, alphalist[l], betalist[k], gamma, thissum)
                    else
                        push!(thisrun, thissigma, j, Rho, alphalist[l], betalist[k], gamma, "NA")
                    end
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



function tunnel_perceptions_gini_perceptionsave!(repetitions,population,Rho,giniList,alphalist,betalist,gamma)
    #u = ReentrantLock()
    alphalength = length(alphalist)
    betalength = length(betalist)
    simulationresults = [[] for i=1:(length(giniList) * repetitions * alphalength * betalength + 1)]
    simulationresults[1]=["gini", "seed", "Rho", "alpha", "beta", "gamma", "utilitysum"]
    listperceptions = [[] for i=1:(length(giniList) * repetitions + 1)]
    listperceptions[1]=["gini", "seed", "Rho", "Y", "perceptionsOfMEAN", "perceptionsOfMIN", "perceptionsOfMAX"]
    for i in 1:length(giniList)
        thisgini = giniList[i]
         ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
        #Threads.@threads for j in 1:repetitions
        for j in 1:repetitions 
            Random.seed!(j)
            Y = income_onegroup_lognormal!(population,2 * erfinv(thisgini))
            linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(Y,Rho)
            perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty = distri_perceptions!(find_visible_nodes!(Y,linkspernodeTHISRUN))
            thisrunperceptions = [ thisgini, j, Rho, Y, perceptionlist_meanproperty, perceptionlist_minproperty, perceptionlist_maxproperty ]
            listperceptions[1 + (i - 1) * repetitions + j] = thisrunperceptions
            for k in 1:betalength
                for l in 1:alphalength
                    thisrun = []
                    if alphalist[l] + betalist[k] <= 1
                        thissum = calculate_utility!(Y, perceptionlist_minproperty, perceptionlist_maxproperty, perceptionlist_meanproperty, alphalist[l], betalist[k],gamma)
                        push!(thisrun, thisgini, j, Rho, alphalist[l], betalist[k], gamma, thissum)
                    else
                        push!(thisrun, thisgini, j, Rho, alphalist[l], betalist[k], gamma, "NA")
                    end
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
    return simulationresults, listperceptions
    #income = income_onegroup_lognormal!(population,sigma)
end

## Define lists for lognorm sigma and weights

ginis = []
push!(ginis,0)
for i in 41:55
    push!(ginis,i/100)
end


alphas = []
push!(alphas,0)
for i in 1:100
    push!(alphas,i/100)
end
betas = alphas


## repetitions,population,Rho,sigmaList,alphalist,betalist,gamma
Rho0 = tunnel_perceptions!(100,1000,0,sigmas,alphas,betas, 50)


Rho0SPLIT3, Rho0perceptionsSPLIT3 = tunnel_perceptions_gini_perceptionsave!(100,1000,0,ginis,alphas,betas,50)




## Transform into df for vosualisation and storage
using DataFrames, Plots, Statistics, CSV

header = Rho0SPLIT3[1]                    # Extract header
rows = Rho0SPLIT3[2:end]                  # Extract rows
outputdfRho0gini = DataFrame([Symbol(h) => [row[i] for row in rows] for (i, h) in enumerate(header)])

dfofinterest = outputdfRho0gini
outputdfRho0gini = dfofinterest[.!((dfofinterest.alpha .+ dfofinterest.beta) .> 1), :]

const working_directory = "C:/Users/dmayerh/Onedrive - Personal/OneDrive/DATIpilot/Inhaltliches/Papers/Hirschman/SimOutput"

CSV.write(joinpath(working_directory, "Rho0gini.csv"), outputdfRho0gini)



## Create Perception df for visualisation and storage
header = Rho0perceptionsSPLIT3[1]                    # Extract header
rows = Rho0perceptionsSPLIT3[2:end]                  # Extract rows
outputdfRho0perceptionsGini = DataFrame([Symbol(h) => [row[i] for row in rows] for (i, h) in enumerate(header)])

CSV.write(joinpath(working_directory, "Rho0perceptionsGini.csv"), outputdfRho0perceptionsGini)





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

create_overviewplot!(0, 1, ouputdfRho1test)


function create_heatmap_by_beta!(betaofinterest, df)
    # Filter by beta and ensure alpha + beta ≤ 1
    selected_df = filter(row -> row.beta == betaofinterest &&
                                  row.alpha + row.beta <= 1, df)

    # If no data remains, skip
    if isempty(selected_df)
        @info "No valid data for β=$(betaofinterest) after filtering α+β ≤ 1"
        return nothing
    end

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



create_heatmap_by_beta!(0.86, outputdf)



function create_heatmap_by_alpha!(alphaofinterest, df)
    # Filter by alpha and ensure alpha + beta ≤ 1
    selected_df = filter(row -> row.alpha == alphaofinterest &&
                                  row.alpha + row.beta <= 1, df)

    # If no data remains, skip
    if isempty(selected_df)
        @info "No valid data for α=$(alphaofinterest) after filtering α+β ≤ 1"
        return nothing
    end

    # Group by (beta, sigma) and compute mean utilitysum
    grouped_df = combine(groupby(selected_df, [:beta, :sigma]),
                         :utilitysum => mean => :mean_utilitysum)

    # Get unique sorted values for heatmap axes
    betas = sort(unique(grouped_df.beta))
    sigmas = sort(unique(grouped_df.sigma))

    # Create a matrix for the heatmap: rows = beta, columns = sigma
    heat_values = [
        grouped_df[(grouped_df.beta .== b) .& (grouped_df.sigma .== s), :mean_utilitysum][1]
        for b in betas, s in sigmas
    ]

    # Plot the heatmap
    heatmap(
        sigmas,          # x-axis (sigma)
        betas,           # y-axis (beta)
        heat_values,     # color (mean utilitysum)
        xlabel = "sigma",
        ylabel = "beta",
        title = "Mean utilitysum by sigma and beta (α = $(alphaofinterest))",
        colorbar_title = "mean utilitysum"
    )
end

create_heatmap_by_alpha!(0.04, outputdf)



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

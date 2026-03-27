## Import modules and packages

using StatsBase, Random, SpecialFunctions

include("InputDistributions.jl")
using .InputDistributions

include("HomophilicNetworks.jl")
using .HomophilicNetworks

include("IndividualPerceptions.jl")
using .IndividualPerceptions


## UTILITY CALCULATION
function calculate_aggregate_utility!(ownincomes, minperceptions, maxperceptions, meanperceptions, alpha, beta, gamma)
    utilitysum = 0
    for i in 1:length(ownincomes)
        currentutility = ownincomes[i] + gamma * (alpha * maxperceptions[i] + beta * meanperceptions[i] + (1 - alpha - beta) * minperceptions[i])
        #currentutility = log(ownincomes[i]) * (beta) + log( maxperceptions[i] ^ alpha * minperceptions[i] ^ (1 - alpha) ) * (1 - beta)
        utilitysum = utilitysum + currentutility
    end
    return utilitysum
end


function calculate_individual_utilities!(ownincomes, minperceptions, maxperceptions, meanperceptions, alpha, beta, gamma)
    utilitylist = []
    for i in 1:length(ownincomes)
        currentutility = ownincomes[i] + gamma * (alpha * maxperceptions[i] + beta * meanperceptions[i] + (1 - alpha - beta) * minperceptions[i])
        #currentutility = log(ownincomes[i]) * (beta) + log( maxperceptions[i] ^ alpha * minperceptions[i] ^ (1 - alpha) ) * (1 - beta)
        push!(utilitylist, currentutility)
    end
    return utilitylist
end

## STANDAALONE FUNCTION TO CALCULATE UTILITIES OF MULTIPLE RUNS- NOT USED CURRENTLY
# function calculate_utilities!(ownincomes, minperceptions, maxperceptions, meanperceptions, alphalist, betalist, gamma)
#     utilitiesperbetaperalpha = []
#     for i in 1:length(betalist)
#          betasperalpha = []
#         for j in 1:length(alphalist)
#             utilnow = calculate_utility!(ownincomes, minperceptions, maxperceptions, meanperceptions, alphalist[j], betalist[i], gamma)
#             push!(betasperalpha,utilnow)
#         end
#         push!(utilitiesperbetaperalpha, betasperalpha)
#     end
#     return utilitiesperbetaperalpha
# end




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
                        thissum = calculate_aggregate_utility!(Y, perceptionlist_minproperty, perceptionlist_maxproperty, perceptionlist_meanproperty, alphalist[l], betalist[k],gamma)
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


## RUN THE MODEL
## Adjust parameters as you see fit
# e.g. Rho = [0 8], ...

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

## Sometimes, splitting the simulation is faviurable if running out of RAM (and swap space) is a concern. In this case, the results of multiple runs can be merged in the end. The following code is an example for how to do this for the Rho0 runs. Adjust file names and variable names as needed for other runs.
Rho0SPLIT3, Rho0perceptionsSPLIT3 = tunnel_perceptions_gini_perceptionsave!(100,1000,0,ginis,alphas,betas,50)



## TRANSFORM to DF and SAVE
const working_directory = pwd()  # set to your desired output folder "" 

## Function to create a perception df for visualisation and storage
function create_df!(inputdataname)
    header = inputdataname[1]                    # Extract header
    rows = inputdataname[2:end]                  # Extract rows
    outputdf = DataFrame([Symbol(h) => [row[i] for row in rows] for (i, h) in enumerate(header)])
    return outputdf[.!((dfofinterest.alpha .+ dfofinterest.beta) .> 1), :]
end

## Transform into df for visualisation and storage
using DataFrames, Plots, Statistics, CSV, StatsPlots


outputdfRho0SPLIT3 = create_df!(Rho0SPLIT3)
CSV.write(joinpath(working_directory, "Rho0perceptionsGini.csv"), outputdfRho0perceptionsGini)






## Makeshift overview plots if batch plotting is not desired.
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


## adjust name here for the desired df to plot
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




## Stuff to play around with
## REGRESSIVE REDISTRIBUTION
function regressive_redistribution!(sortedinputlist,percentage, percbeneficiaries)
    sortedincomelist = copy(sortedinputlist)
    population = length(sortedincomelist)
    numberbeneficiaries = round(Int, population * percbeneficiaries)
    numberpayers = population - numberbeneficiaries
    totalamountcollected = 0
    for i in 1:numberpayers
        amountcollected = sortedincomelist[i] * percentage
        sortedincomelist[i] = sortedincomelist[i] - amountcollected
        totalamountcollected = totalamountcollected + amountcollected
    end
    ## Distribute collected amount equally among beneficiaries
    # amountperbeneficiary = totalamountcollected / numberbeneficiaries
    # for j in (numberpayers + 1):population
    #     sortedincomelist[j] = sortedincomelist[j] + amountperbeneficiary
    # end

    ## Distribute collected amount proportionally among beneficiaries
    totalincomebeneficiaries = sum(sortedincomelist[(numberpayers + 1):population])
    for j in (numberpayers + 1):population
        proportionthisbeneficiary = sortedincomelist[j] / totalincomebeneficiaries
        sortedincomelist[j] = sortedincomelist[j] + (totalamountcollected * proportionthisbeneficiary)
    end
    return sortedincomelist
end










function regressive_redistribution_perceptions!(repetitions,population,Rho,gini,alpha,beta,gamma,percentage, percbeneficiaries)
    simulationresults = [[] for i=1:(repetitions + 1)]
    simulationresults[1]=["seed", "Rho", "gini", "alpha", "beta", "gamma", "percentage", "percbeneficiaries", "deltautilities"]
     ## The network generation is the most computationally intense task here. It can be split split in multiple Threads if supported by the Julia environment. (Check in Settings!)
    for j in 1:repetitions 
        Random.seed!(j)
        Y = income_onegroup_lognormal!(population,2 * erfinv(gini))
        linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(Y,Rho)
        ## Utilities before redistribution
        perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty = distri_perceptions!(find_visible_nodes!(Y,linkspernodeTHISRUN))
        utilitiesbeforeredistribution = calculate_individual_utilities!(Y, perceptionlist_minproperty, perceptionlist_maxproperty, perceptionlist_meanproperty, alpha, beta, gamma)
        ## Apply redistribution
        Yredistributed = regressive_redistribution!(Y,percentage, percbeneficiaries)
        ## Re-link after redistribution
    #    linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(Yredistributed,Rho)
        perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty = distri_perceptions!(find_visible_nodes!(Yredistributed,linkspernodeTHISRUN))
        utilitiesafterredistribution = calculate_individual_utilities!(Yredistributed, perceptionlist_minproperty, perceptionlist_maxproperty, perceptionlist_meanproperty, alpha, beta, gamma)
        deltautilities = utilitiesafterredistribution - utilitiesbeforeredistribution
        thisrun = [ j, Rho, gini, alpha, beta, gamma, percentage, percbeneficiaries, deltautilities]
        simulationresults[1 + j] = thisrun
    end
    return simulationresults
end

blubb = regressive_redistribution_perceptions!(10,1000,4,0.5,0.03,0.97,50,0.1,0.05)



test0 = regressive_redistribution_perceptions!(10,1000,0,0.2,0.1,0.9,50,0.1,0.05)
testdf0 = create_df!(test0)
testdf = testdf0

#test1point5 = regressive_redistribution_perceptions!(10,1000,1.5,0.5,0.03,0.97,50,0.1,0.05)
#testdf1point5=create_df!(test1point5)
#testdf = vcat(testdf, testdf1point5; cols=:union)

test4 = regressive_redistribution_perceptions!(10,1000,4,0.2,0.1,0.9,50,0.1,0.05)
testdf4=create_df!(test4)
testdf = vcat(testdf, testdf4; cols=:union)

#test8 = regressive_redistribution_perceptions!(10,1000,8,0.5,0.2,0.4,50,0.1,0.05)
#testdf8=create_df!(test8)
#testdf = vcat(testdf, testdf8; cols=:union)

#test14 = regressive_redistribution_perceptions!(10,1000,14,0.5,0.2,0.4,50,0.1,0.05)
#testdf14=create_df!(test14)
#testdf = vcat(testdf, testdf14; cols=:union)

testlow = regressive_redistribution_perceptions!(10,1000,4,0.5,0.03,0.97,50,0.1,0.05)
testdflow = create_df!(testlow)
testdf = testdflow

testhigh = regressive_redistribution_perceptions!(10,1000,4,0.51,0.03,0.97,50,0.1,0.05)
testdfhigh = create_df!(testhigh)
testdf = vcat(testdf, testdfhigh; cols=:union)



## Add column with mean delate utilitues
testdf = transform(testdf, :deltautilities => (x -> mean.(x)) => :mean_deltautilty)

## Add column counting calues below zero
thresholdforrevolt = 0
testdf = transform(testdf,
    :deltautilities => ByRow(v -> isempty(v) ? missing : count(<(thresholdforrevolt), v) / length(v)) =>
    :prop_below_threshold
)

## Violin plots of mean_deltautilty of rho = 0
#rhos = sort(unique(testdf.Rho))                 # [0, 1.5, 4, 8, 14]
#rho2i = Dict(ρ => i for (i, ρ) in enumerate(rhos))

## Violin plots for different ginis
ginis = sort(unique(testdf.gini))                 # [0, 1.5, 4, 8, 14]
ginis2i = Dict(ρ => i for (i, ρ) in enumerate(ginis))

x = getindex.(Ref(ginis2i), testdf.gini)
#y = testdf.mean_deltautilty
y = testdf.prop_below_threshold

p = violin(x, y;
    xticks=(1:length(ginis), string.(ginis)),
    xlabel="ρ",
    ylabel="Δ utility below 0",
    title="gini = 0.2, α=0.5, β=0.1, γ=50, 10% redistributed, 5% beneficiaries",
    legend=false)

scatter!(p, x, y; ms=3, alpha=0.7, legend=false)

display(p)


long = reduce(vcat, (
    DataFrame(Rho=testdf.Rho[r],
              i=collect(eachindex(testdf.deltautilities[r])),
              du=testdf.deltautilities[r])
    for r in 1:nrow(testdf)
))


@df long scatter(:i, :du, group=:Rho,marker=:x, 
                 ms=2, alpha=0.7,
                 xlabel="income rank", ylabel="Δ utility",
                 title="gini = 0.2, α=0.1, β=0.1, γ=50, 10% redistributed, 5% beneficiaries")
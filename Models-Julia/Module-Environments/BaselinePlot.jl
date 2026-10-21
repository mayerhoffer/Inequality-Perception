## Import modules and packages

#cd("Models-Julia/Module-Environments")

include("InputDistributions.jl")
using .InputDistributions

include("HomophilicNetworks.jl")
using .HomophilicNetworks

include("IndividualPerceptions.jl")
using .IndividualPerceptions

using SpecialFunctions, StatsBase, Combinatorics, Distributions, LinearAlgebra, DataFrames, CSV, Readables, BenchmarkTools, Random, ColorSchemes, Colors, DelimitedFiles, Graphs, Plots, NetworkLayout

# Keep Makie's plotting functions qualified so that they do not conflict with
# Plots.jl functions such as `scatter` used later in this file.
import CairoMakie
import GraphMakie

CairoMakie.activate!()


## Create adjacency matrix
Random.seed!(1)
#incomedistri = income_onegroup_exponential!(1000)
incomedistri = InputDistributions.income_onegroup_lognormal!(1000,0.25)
#incomedistri = income_onegroup_normal!(1000)
#incomedistri = income_onegroup_uniform!(1000)


linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.perfect_homophilic_linkage(incomedistri)
linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.perfect_rank_homophilic_linkage(incomedistri,6)
#linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.homophilic_linkage!(incomedistri,8)
adjmatr = HomophilicNetworks.create_adjacencymatrix!(linksTHISRUN,1000)

#CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\adjmatrix_norm-rho10.csv",Tables.table(adjmatr))
#CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\attributedistri_norm.csv",Tables.table(incomedistri))



# Convert the DataFrame to an adjacency matrix
adjmatrix_array = Matrix{Int}(adjmatr)

# Create a graph from the adjacency matrix
g = Graphs.SimpleGraph(adjmatrix_array)

n = Graphs.nv(g)

income_ranks = collect(1:n)

fig, ax, p = GraphMakie.graphplot(
    g;
    layout = NetworkLayout.Spring(
    C = 8.0,
    iterations = 300,
    initialtemp = 3.0,
    seed = 1,
    ),
    # layout = NetworkLayout.SFDP(
    #     C = 0.8,
    #     K = 1.0,
    #     iterations = 250,
    #     tol = 0.01,
    #     seed = 1,
    #     Ptype = Float32,
    # ),
    node_size = 8,
    node_color = income_ranks,
    node_attr = (; colormap = :viridis, colorrange = extrema(income_ranks)),
    edge_color = Gray(0.4),
    edge_width = 0.5,
)
CairoMakie.hidedecorations!(ax)
CairoMakie.hidespines!(ax)
CairoMakie.Colorbar(
    fig[1, 2],
    GraphMakie.get_node_plot(p);
    label = "Income Rank",
)
fig




# Reuse the first layout and color nodes by their actual income.
income_fig, income_ax, income_plot = GraphMakie.graphplot(
    g;
    layout = p[:node_pos][],
    node_size = 8,
    node_color = incomedistri,
    node_attr = (; colormap = :viridis, colorrange = extrema(incomedistri)),
    edge_color = Gray(0.4),
    edge_width = 0.5,
)
CairoMakie.hidedecorations!(income_ax)
CairoMakie.hidespines!(income_ax)
CairoMakie.Colorbar(
    income_fig[1, 2],
    GraphMakie.get_node_plot(income_plot);
    label = "Income",
)
income_fig

##Colour by income of nodes



## Plot rank differences to neighbors
function rankdifferences(repetitions,population,Rho,gini)
    u = ReentrantLock()
    higherdiffs = [Int[] for i=1:population]
    lowerdiffs = [Int[] for i=1:population]
    alldiffs = [Int[] for i=1:population]
    minseenranks = [Int[] for i=1:population]
    maxseenranks = [Int[] for i=1:population]
    minseenincome = [Float64[] for i=1:population]
    maxseenincome = [Float64[] for i=1:population]
    meanseenincome = [Float64[] for i=1:population]
    stdseenincome = [Float64[] for i=1:population]
         ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
        #Threads.@threads for j in 1:repetitions
        for j in 1:repetitions 
            Random.seed!(j)
            if isnan(gini)
                Y = InputDistributions.income_onegroup_exponential!(population)
            else
                Y = InputDistributions.income_onegroup_lognormal!(population, 2 * erfinv(gini))
            end
            ## Toggle between rho-based (including for a random network for rho = 0) and perfect homophily here
            #linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.homophilic_linkage!(Y, Rho)
            Y = InputDistributions.expectedvalue_onegroup_lognormal(population,2 * erfinv(gini))
            linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.perfect_rank_homophilic_linkage(Y,6)

            perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty = IndividualPerceptions.distri_perceptions!(IndividualPerceptions.find_visible_nodes!(Y,linkspernodeTHISRUN))
            for entry in linkspernodeTHISRUN
                me = entry[1]
                neighbours = entry[2:end]
                myhigherdiffs = [thisneighbour - me for thisneighbour in neighbours if thisneighbour > me]
                mylower_diffs  = [me - thisneighbour for thisneighbour in neighbours if thisneighbour < me]
                myalldiffs = vcat(myhigherdiffs, mylower_diffs)
                lock(u) do
                    append!(higherdiffs[me], myhigherdiffs)
                    append!(lowerdiffs[me], mylower_diffs)
                    append!(alldiffs[me], myalldiffs)
                end
                lock(u) do
                    if !isempty(myhigherdiffs)
                        push!(maxseenranks[me], maximum(myhigherdiffs))
                    end
                    if !isempty(mylower_diffs)
                        push!(minseenranks[me], maximum(mylower_diffs))
                    end
                end
                lock(u) do
                    push!(minseenincome[me],perceptionlist_minproperty[me])
                    push!(maxseenincome[me],perceptionlist_maxproperty[me])
                    push!(meanseenincome[me],perceptionlist_meanproperty[me])
                    push!(stdseenincome[me],perceptionlist_stdproperty[me])
                end
            end
        end
    return higherdiffs, lowerdiffs, alldiffs, minseenranks, maxseenranks, minseenincome, maxseenincome, meanseenincome, stdseenincome
end

higherdiffs, lowerdiffs, alldiffs, minseenranks, maxseenranks, minseenincome, maxseenincome, meanseenincome, stdseenincome = rankdifferences(200,1000,0,0.5)
higherdiffs8, lowerdiffs8, alldiffs8, minseenranks8, maxseenranks8, minseenincome8, maxseenincome8, meanseenincome8, stdseenincome8 = rankdifferences(200,1000,8,0.5)
higherdiffsP, lowerdiffsP, alldiffsP, minseenranksP, maxseenranksP, minseenincomeP, maxseenincomeP, meanseenincomeP, stdseenincomeP = rankdifferences(1,1000,0,0.5)


ranks=collect(1:1000)


## Plots
## Adjust input variables based on desired rho value

## Rank differences to link neighbors
## Individual runs pooled -> means of entire lists
scatter(
    ranks, mean.(alldiffs),
    xlabel = "Income rank",
    ylabel = "neighbours' mean absolute rank difference",
    ms = 2,
    mc = :black,
    markershape = :cross,
    legend = false
)

scatter!(
    ranks, mean.(higherdiffs),
    xlabel = "Income rank",
    ylabel = "richer neighbours' mean rank difference",
    ms = 2,
    mc = :purple,
    markershape = :cross,
    legend = false
)

scatter!(
    ranks, mean.(lowerdiffs),
    xlabel = "Income rank",
    ylabel = "poorer neighbours' mean rank difference",
    ms = 2,
    mc = :orange,
    markershape = :cross,
    legend = false
)


## Min and max of each run -> means of run mins/maxes
scatter(
    ranks, mean.(minseenranks),
    xlabel = "Income rank",
    ylabel = "mean rank difference to poorest neighbour",
    ms = 2,
    mc = :orange,
    markershape = :cross,
    legend = false
)

scatter(
    ranks, mean.(maxseenranks),
    xlabel = "Income rank",
    ylabel = "mean rank difference to richest neighbour",
    ms = 2,
    mc = :purple,
    markershape = :cross,
    legend = false
)

## ...in one plot
scatter!(
    ranks, mean.(minseenranksP),
    label = "... Poorest Link-Neighbour",
    xlabel = "Income Rank",
    ylabel = "Mean Rank Difference to...",
    ms = 2,
    mc = :orange,
    markershape = :cross,
)

scatter(
    ranks, mean.(maxseenranksP),
    label = "... Richest Link-Neighbour",
    xlabel = "Income Rank",
    ylabel = "Mean Rank Difference to...",
    ms = 2,
    mc = :purple,
    markershape = :cross,
)


## Min mean and max income seen by each income rank
scatter(
    ranks, mean.(maxseenincomeP),
    label = "Highest",
    xlabel = "Income Rank",
    ylabel = "Seen Incomes",
    ms = 1,
    mc = :purple,
    markershape = :cross
)
scatter!(
    ranks, mean.(meanseenincomeP),
    label = "Mean",
    xlabel = "Income Rank",
    ylabel = "seen Incomes",
    ms = 1,
    mc = :black,
    markershape = :cross
)
scatter!(
    ranks, mean.(minseenincomeP),
    label = "Lowest",
    xlabel = "Income Rank",
    ylabel = "Seen Incomes",
    ms = 1,
    mc = :orange,
    markershape = :cross
)


# std of observed incomes
scatter(
    ranks, mean.(stdseenincome),
    label = "Std",
    xlabel = "Income rank",
    ylabel = "std of seen incomes",
    ms = 1,
    mc = :blue,
    markershape = :cross
)

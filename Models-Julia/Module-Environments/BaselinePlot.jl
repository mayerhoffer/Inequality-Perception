using StatsBase, Combinatorics, Distributions, LinearAlgebra, DataFrames, CSV, Readables, BenchmarkTools, Random,ColorSchemes,Colors, DelimitedFiles, Graphs, Plots, GraphRecipes, NetworkLayout

using .InputDistributions, .HomophilicNetworks


## Create adjacency matrix
Random.seed!(1)
incomedistri = income_onegroup_exponential!(1000)
#incomedistri = income_onegroup_lognormal!(1000,1)
linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(incomedistri,4)
adjmatr = create_adjacencymatrix!(linksTHISRUN,1000)

CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\adjmatrix_exp-rho4.csv",Tables.table(adjmatr))
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\attributedistri.csv",Tables.table(incomedistri))



# Convert the DataFrame to an adjacency matrix
adjmatrix_array = Matrix{Int}(adjmatr)

# Create a graph from the adjacency matrix
g = SimpleGraph(adjmatrix_array)

# Generate a spring layout for the graph
layout = spring_layout(g)


nodefillc = incomedistri
gplot(g, nodefillc=nodefillc)


graphplot(g,
           node_weights = 1:1000,
           markercolor = :darkgray,
           dim = 3,
           markersize = 5,
           linecolor = :darkgrey,
           linealpha = 0.5
       )
using StatsBase, Combinatorics, Distributions, LinearAlgebra, DataFrames, CSV, Readables, BenchmarkTools, Random,ColorSchemes,Colors, DelimitedFiles, Graphs, Plots, GraphRecipes, NetworkLayout, Random
using GraphMakie, CairoMakie, Colors, GraphMakie.NetworkLayout


using .InputDistributions, .HomophilicNetworks


## Create adjacency matrix
Random.seed!(1)
incomedistri = income_onegroup_exponential!(1000)
#incomedistri = income_onegroup_lognormal!(1000,0.25)
#incomedistri = income_onegroup_normal!(1000)
#incomedistri = income_onegroup_uniform!(1000)

linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(incomedistri,0)
adjmatr = create_adjacencymatrix!(linksTHISRUN,1000)

CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\adjmatrix_norm-rho10.csv",Tables.table(adjmatr))
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\attributedistri_norm.csv",Tables.table(incomedistri))



# Convert the DataFrame to an adjacency matrix
adjmatrix_array = Matrix{Int}(adjmatr)

# Create a graph from the adjacency matrix
g = Graphs.SimpleGraph(adjmatrix_array)

n = Graphs.nv(g)

vals = collect(1:n)

fig, ax, p = graphplot(
    g;
    #layout = Shell(),
    layout = Stress(),
    # ayout = Spring(;
    #     iterations = 100,   # increase for nicer convergence (slower)
    #     seed = 1            # deterministic layout
    #     # dim = 2           # default
    # ),
    node_size = 8,
    node_color = vals,
    colormap = :grays,
    edge_color = Gray(0.4),
    edge_width = 0.5, 
)
hidedecorations!(ax); hidespines!(ax)
            # (often optional now, but fine)
Colorbar(fig[1, 2], p; label="Income Rank")
fig




nodefillc = incomedistri
gplot(g, nodefillc=nodefillc)

## Unicoloured nodes
graphplot(g,
           node_weights = 1:1000,
           markercolor = :darkgray,
           dim = 3,
           markersize = 5,
           linecolor = :darkgrey,
           linealpha = 0.5
       )

##Colour by income of nodes
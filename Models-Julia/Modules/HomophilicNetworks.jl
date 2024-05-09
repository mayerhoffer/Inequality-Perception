module HomophilicNetworks

using StatsBase, LightGraphs

export homophilic_linkage!, create_adjacencymatrix!, calculate_average_path_length!, calculate_clustering_coefficient!, perfect_homophilic_linkage!

## --------------------------------------------------------------------- Network Generation ---------------------------------------------------------------------

## Basic function. Yields links between nodes (identified by their number in the income distribution only) and links per node (ordered by their number in the income distribution)
function homophilic_linkage!(Distri,Rho)
    populationsize = length(Distri)
    ## Builds weights matrix for linkage. 
    weights = zeros(populationsize,populationsize)
        ## Given homophilic links, the matrix is symmetric and one can avoid half of the work.
    for i in 1:length(Distri)
        for j in 1:(i-1)
            weights[i,j]=weights[j,i]
        end
        for j in (i+1):length(Distri)
            weights[i,j] = exp(-Rho*abs(Distri[i] - Distri[j])) # ArbReal(exp(-Rho*abs(Distri[i] - Distri[j])))
        end
    end
    ## Does the actual sampling via the samps function to determine which links exist
    items = range(1,populationsize)
    linklist = []
    for i in 1:length(Distri)
        sampsnow = samps!(i,weights,items)
        for j in 1:length(sampsnow)
            pairnow = [i,sampsnow[j]]
            push!(linklist,sort(pairnow))
        end
    end
    ## Cleans up the list of existing links and indicates the link-neighbours for each node
    linklist = unique(linklist)
    linksPerNodeList = []
    for i in 1:populationsize
        mylinks = [i]                                ## nodes always know about themselves/have a link with themselves
        for j in 1:length(linklist)                  ## cycles through all other nodes and checks whether there is a link
            if i in linklist[j]
                if linklist[j][1] == i               ## makes sure the other node of the pair, not the one searching, is added
                    push!(mylinks,linklist[j][2])
                else
                    push!(mylinks,linklist[j][1])
                end
            end
        end
        push!(linksPerNodeList,mylinks)
    end

    return linklist, linksPerNodeList
end

function perfect_homophilic_linkage!(Distri)
    populationsize = length(Distri)
    linklist = []
    for i in 1:length(Distri)
        myincome = Distri[i]
        comparisons = []
        for j in Distri
            push!(comparisons,abs(myincome - j))
        end
        sorted_indices = sortperm(comparisons)
        for l in 1:6
            pairnow = [i,sorted_indices[l]]
            push!(linklist,sort(pairnow))
        end
    end
    ## Cleans up the list of existing links and indicates the link-neighbours for each node
    linklist = unique(linklist)
    linksPerNodeList = []
    for i in 1:populationsize
        mylinks = []
        for j in 1:length(linklist)                  ## cycles through all other nodes and checks whether there is a link
            if i in linklist[j]
                if linklist[j][1] == i               ## makes sure the other node of the pari, not the one searching, is added
                    push!(mylinks,linklist[j][2])
                else
                    push!(mylinks,linklist[j][1])
                end
            end
        end
        push!(linksPerNodeList,mylinks)
    end

    return linklist, linksPerNodeList
end


## --------------------------------------------------------------------- Helper Functions ---------------------------------------------------------------------
## Draws link-neighbours using a weights matrix
## If dividing the weights by their sum and working with Float32, this would be a great candidate for GPU computing
function samps!(i,weights,items)
    if count(i->(i>0), Weights(weights[:,i])) > (5-1)
        return sample(items, Weights(weights[:,i]), 5, replace=false)
    else
        ## In case there are not enough links with non-zero weights (typically in a Pareto tail), just link to the five closest values.
        ## Can be adjusted 
        return [i-5,i-4,i-3,i-2,i-1]
    end
end



## --------------------------------------------------------------------- Network Evaluation ---------------------------------------------------------------------
function create_adjacencymatrix!(linklist,populationsize)
    ## Creates an adjacencymatrix from the linklist
    adjacencymatrix = zeros(Int, populationsize, populationsize)
    for pair in linklist
        a, b = pair
        adjacencymatrix[a, b] = 1
        adjacencymatrix[b, a] = 1
    end
    return adjacencymatrix
end

function calculate_average_path_length!(adjacencymatrix)
    networkgraph = Graph(adjacencymatrix) ## creates a network graph from the input adjacencymatrix
    avgpathlengthssum = 0
    for i in nv(networkgraph)
        mypaths = dijkstra_shortest_paths(networkgraph, i)
        avgpathlengthssum += mean(mypaths.dists)
    end
    return avgpathlengthssum
end

## calculates the average clustering coefficient   
function calculate_clustering_coefficient!(linklist, linksPerNodeList)
    clustering_coefficients = []
    for node_links in linksPerNodeList
        num_neighbors = length(node_links) - 1
        num_actual_edges = 0
        for i in 2:length(node_links)
            for j in (i+1):length(node_links)
                if [node_links[i], node_links[j]] in linklist || [node_links[j], node_links[i]] in linklist
                    num_actual_edges += 1
                end
            end
        end
        num_possible_edges = num_neighbors * (num_neighbors - 1) / 2
        if num_possible_edges > 0
            clustering_coefficient = num_actual_edges / num_possible_edges
        else
            clustering_coefficient = 0.0
        end
        push!(clustering_coefficients, clustering_coefficient)
    end
    
    avgclustering = sum(clustering_coefficients) / length(linklist)
    
    return avgclustering
end

end
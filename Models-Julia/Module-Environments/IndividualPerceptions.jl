module IndividualPerceptions

using StatsBase, Combinatorics

export find_visible_nodes!, distri_perceptions!, gini_perceptions!, self_perceptions!

function find_visible_nodes!(sorted_nodePropertyList,linksPerNodeList)
    visiblenodelist = []
    for i in 1:length(sorted_nodePropertyList)
        myvisiblenodes = []
        for j in 1:length(linksPerNodeList[i])       ## find all link-neighbours' property
            push!(myvisiblenodes,sorted_nodePropertyList[linksPerNodeList[i][j]] )
        end
        push!(visiblenodelist,myvisiblenodes)
    end
    return visiblenodelist
end

function distri_perceptions!(visiblenodelist)
    perceptionlist_meanproperty = []
    perceptionlist_maxproperty = []
    perceptionlist_minproperty = []
    perceptionlist_stdproperty = []
    for i in 1:length(visiblenodelist)
        myvisiblenodes = visiblenodelist[i]
        push!(perceptionlist_meanproperty,mean(myvisiblenodes))
        push!(perceptionlist_maxproperty,maximum(myvisiblenodes))
        push!(perceptionlist_minproperty,minimum(myvisiblenodes))
        push!(perceptionlist_stdproperty,std(myvisiblenodes))
    end
    return perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty
end

function gini_perceptions!(visiblenodelist)
    perceptionlist_gini = []
    for i in 1:length(visiblenodelist)
        myvisiblenodes = visiblenodelist[i]
        mypairs = collect(combinations(myvisiblenodes,2))
        myginilist = []
        foreach(x -> push!(myginilist,abs(x[1]-x[2])), mypairs)    ## do all possible pairwise combinations
        mygini = mean(myginilist)
        mygini = mygini / (mean(myvisiblenodes)*2)
        push!(perceptionlist_gini,mygini)
    end
    return perceptionlist_gini
end

function self_perceptions!(visiblenodelist,sorted_nodePropertyList)
    n = length(visiblenodelist)
    perceptionlist_self = Vector{Float64}(undef, n)
    for i in 1:n
        myegonetwork = sort(visiblenodelist[i])
        myproperty = sorted_nodePropertyList[i]
        ranks = findall(==(myproperty), myegonetwork)
        if length(ranks) == 0
            perceptionlist_self[i] = missing
        else
            perceptionlist_self[i] = mean(ranks)
        end
    end
    return perceptionlist_self
end

end
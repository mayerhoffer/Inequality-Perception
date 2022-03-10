#using GraphPlot, SimpleWeightedGraphs, LinearAlgebra, DataFrames, Random, CSV, Colors
#import Cairo, Fontconfig
using StatsBase, Combinatorics, Distributions, LinearAlgebra, PlotlyJS, DataFrames, CSV, Readables, BenchmarkTools, Random
#using ArbNumerics,DoubleFloats

## Draws an income distribution with mean 1 and consisting of "population" individual values where the proportion "p" is in the Pareto tail and the rest comes from the exponential distribution and "m" is the markup defining the distribution in the tail.
function combined_incomedistribution!(p,m,population)
    exb = Exponential(1)
    y = rand(exb,population)
    y  = sort(y)
    if p > 0
 #       paredo = Pareto((m*(p-1)/(-m+m*p-p*log(p))),-log(p))
        y = y[1:find_workerPopulation!(p,population)]
        y = y/(mean(y)*(1+m)*(1-p))
 #       y = vcat(y,rand(paredo,Int(round(p*population))))
        y  = sort(y)
    else
        y = y/mean(y)
    end
    return y
end

function find_workerPopulation!(p,population)
    return Int(round((1-p)*population))
end

## Calculates the real gini of an income distribution
function real_gini!(Distri)
    n = length(Distri)
    xx = sort(Distri)
  2*(sum(collect(1:n).*xx))/(n*sum(xx))-1
end


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

## Creates a list of links for every node (income) given an income distribution (defined above) and a homophily Strength Rho
function homophilic_linkage!(Distri,Rho,population)
# The higher working precision is still not helpful and can thus be ignored
#    if p > 0
#        setworkingprecision(ArbReal, digits=population)
#    else
#        setworkingprecision(ArbReal, digits=64)
#    end
## Builds weights matrix for linkage. 
    weights = zeros(population,population)
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
    items = range(1,population)
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
    for i in 1:population
        mylinks = [i]                                ## nodes always know about themselves/have a link with themselves
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
    return linksPerNodeList
end

## Lets each node estimate the gini
function gini_estimation!(Y,linksPerNodeList)
    ginilist = []
    for i in 1:length(Y)
        myvisibleincomes = []
        for j in 1:length(linksPerNodeList[i])       ## find all link-neighbours' incomes
            push!(myvisibleincomes,Y[linksPerNodeList[i][j]] )
        end
        mypairs = collect(combinations(myvisibleincomes,2))
        myginilist = []
        foreach(x -> push!(myginilist,abs(x[1]-x[2])), mypairs)    ## do all possible pairwise combinations
        mygini = mean(myginilist)
        mygini = mygini / (mean(myvisibleincomes)*2)               ## divide to calculate the actual gini estimate
        push!(ginilist,mygini)
    end
    return ginilist     ## result: A list of individual gini estimates
end

## Computes the consumption given an income distribution, network and consumption parameters
function consumption_calculation!(population,Y,bwork,bcap,c,w,linksPerNodeList)
    C = []
## First calculates the indiosyncratic consumption (as always sorted by income rank)
    for i in 1:population                  ## workers
        push!(C,Y[i] * bwork * w)
    end
    # for i in (Int(round((1-p)*population))+1):population       ## capitalists
    #     push!(C,Y[i] * bcap * w)
    # end
## Computes status consumption, starting with the highest income

## richest workers' consumption - connected to the capitalist representative consumption
    myidioconsumption = C[workerPopulation]
    C = replace(C, myidioconsumption =>  myidioconsumption + ((1-w) * c * ((1/p)*(m/(1+m))*bcap*w) - myidioconsumption))

## all other workers' consumptions
    for i in 1:(population - 1)
        iDesc = population - i
        myvisibleconsumptions = []
        for j in 1:length(linksPerNodeList[iDesc])              ## collects link neighbours' consumptions
            push!(myvisibleconsumptions, C[linksPerNodeList[iDesc][j]])
        end
        orientation = maximum(myvisibleconsumptions)
        myidioconsumption = C[iDesc]
        if orientation > myidioconsumption
            myconsumption = (myidioconsumption + (1-w) * c * (orientation - myidioconsumption))
            C = replace(C, myidioconsumption =>myconsumption)
        end
    end
    return C    ## result: A list of workers'sonsumptions
end


function behaviorspace_consumption_multiple!(repetitions,p,m,population,RhoRange,bwork,bcapRange,cRange,w)
    if p > 0 
        relevantPopulation = find_workerPopulation!(p,population)
    else
        relevantPopulation = population
    end
    u = ReentrantLock()
    # creates income distribution and lists of edges per node for later usages
    ylists = []         # nested list with one sublist for each Rho containing the results (Vector of length population) for each MC run using this Rho: [[Rho1-Seed1,...,Rho1-Seedn],...,[Rhon-Seed1,...,Rhon-Seedn]]
    linklists = []
    realginiLists = []
    for i in 1:length(RhoRange)
        thisrhorealginiList = collect(Float64,1:repetitions)
        thisrhoylists = []#  collect(Float64,1:repetitions)
        thisrholinklists = []#collect(Float64,1:repetitions)
        for j in 1:repetitions
            push!(thisrhoylists,[])
            push!(thisrholinklists,[])
        end
        Threads.@threads for j in 1:repetitions   ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
            Random.seed!(j)
            y = combined_incomedistribution!(p,m,population)
            thisRG = real_gini!(y)
            thisNW = homophilic_linkage!(y,RhoRange[i],relevantPopulation)
            Threads.lock(u) do                    ## Lock to avoid race conditions. Maybe not necessary after all...
                thisrhorealginiList[j] = thisRG
            end
            Threads.lock(u) do
                thisrhoylists[j] = y
            end
            Threads.lock(u) do
                thisrholinklists[j] = thisNW
            end
        end
        push!(realginiLists,thisrhorealginiList)
        push!(ylists, thisrhoylists)
        push!(linklists,thisrholinklists)
    end
    Worlddescription = [ylists,linklists,realginiLists]
    Output =[]
    for i in 1:length(RhoRange)
        for j in 1:length(bcapRange)
            for k in 1:length(cRange)
                APCsList = []
                aggAPCList = []
                CList = []
                for j in 1:repetitions
                    push!(APCsList,[])
                    push!(aggAPCList,[])
                    push!(CList,[])
                end
                for l in 1:repetitions
                    Random.seed!(l)
                    y=ylists[i][l]
                    C = consumption_calculation!(relevantPopulation,y,bwork,bcapRange[j],cRange[k],w,linklists[i][l])
                    APCs = C./y
                    aggAPC = (sum(C) + (w*bcap*population*(m/(1+m)))) /population
                    Threads.lock(u) do
                        APCsList[l] = APCs
                    end
                    Threads.lock(u) do
                        aggAPCList[l] = aggAPC
                    end
                    Threads.lock(u) do
                        CList[l] = C
                    end
                end
                meanAPCsByPos = []
                meanCByPos = []
                for q in 1:population
                    thisincomeAPCs = []
                    thisincomeCs = []
                    for r in 1:repetitions
                        push!(thisincomeAPCs,APCsList[r][q])
                        push!(thisincomeCs,CList[r][q])
                    end
                    push!(meanAPCsByPos,mean(thisincomeAPCs))
                    push!(meanCByPos,mean(thisincomeCs))
                end
                push!(Output,[RhoRange[i],bcapRange[j],cRange[k],meanCByPos,meanAPCsByPos,aggAPCList])
            end
        end
    end
    return Output, Worlddescription
end


resulstpls = behaviorspace_consumption_multiple!(5,0.01,0.3,1000,[1,4],1,[0.1,1],[0.5,1],0.5)

## Parameters: ( repetitions, p , m , population , RhoRange , bwork , bcapRange , cRange ,w )


## Visualisation
# scatterplotPerception = Layout(title="Gini Perceptions by Income Rank",
#     xaxis_title="Income Rank",
#     yaxis_title="Gini Perception",
# )
# scatterplotAPCs = Layout(title="APCs by Income by Income Rank",
#     xaxis_title="Income Rank",
#     yaxis_title="APC",
# )

# plot(scatter(x=1:1000, y=consumptionlist, marker_color="purple",mode="markers"),scatterplotAPCs)

# df = dataset(DataFrame, "tips")
# plot(df, x=:total_bill, kind="histogram")

# plot(histogram(x=log.(consumptionlist[3]), nbinsx=50))


##runtime evaluation
#@btime behaviorspace_consumption_multiple!(10,0,0,1000,[1,4],1,[0.1,1],[1],0.5)


## Deprescated code for baseline
# function run_gini!(p,m,population,Rho)
#     y = combined_incomedistribution!(p,m,population)
#     ginilist = gini_estimation!(y,homophilic_linkage!(p,y,Rho,population))
#     real_gini!(y)
#     return ginilist
# end



## Deprescated code for consumption
# function consume!(p,m,population,Rho,bwork,bcap,c,w)
#     y = combined_incomedistribution!(p,m,population)
#     C = consumption_calculation!(p,population,y,bwork,bcap,c,w,homophilic_linkage!(p,y,Rho,population))
#     APCs = C./y
#     meanAPC = sum(C)/sum(y)
#     real_gini!(y)
#     return y, meanAPC, APCs, C
# end

# function consume_repeated!(p,m,population,Rho,bwork,bcap,c,w,APCsList,aggAPCList,CList,realginiList)
#     y = combined_incomedistribution!(p,m,population)
#     C = consumption_calculation!(p,population,y,bwork,bcap,c,w,homophilic_linkage!(p,y,Rho,population))
#     APCs = C./y
#     aggAPC = sum(C)/sum(y)
#     push!(APCsList, APCs)
#     push!(aggAPCList,aggAPC)
#     push!(CList,C)
#     push!(realginiList,real_gini!(y))
# end

# function behaviorspace_consumption!(repetitions,p,m,population,Rho,bwork,bcap,c,w)
#     APCsList = []
#     aggAPCList = []
#     CList = []
#     realginiList = []
#     for i in 1:repetitions
#         Random.seed!(i)
#         consume_repeated!(p,m,population,Rho,bwork,bcap,c,w,APCsList,aggAPCList,CList,realginiList)
#     end
#     meanAPCsByPos = []
#     for i in 1:population
#         thisincome = []
#         for j in 1:repetitions
#             push!(thisincome,APCsList[j][i])
#         end
#         push!(meanAPCsByPos,mean(thisincome))
#     end
#     meanCByPos = []
#     for i in 1:population
#         thisincome = []
#         for j in 1:repetitions
#             push!(thisincome,CList[j][i])
#         end
#         push!(meanCByPos,mean(thisincome))
#     end
#     return meanCByPos, meanAPCsByPos, aggAPCList #,CList,realginiList
# end

# function behaviorspace_consumption_multiple_old!(repetitions,p,m,population,RhoRange,bwork,bcapRange,cRange,w)
#     Output = []
#     foreach(x -> foreach(z -> foreach(a -> push!(Output,[string("Rho=",x,", bcap=",z,", c=",a),behaviorspace_consumption!(repetitions,p,m,population,x,bwork,z,a,w)]), cRange), bcapRange),RhoRange)
#     return Output
# end
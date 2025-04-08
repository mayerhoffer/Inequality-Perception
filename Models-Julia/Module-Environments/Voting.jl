using StatsBase, Combinatorics, Distributions, LinearAlgebra, DataFrames, CSV, Readables, BenchmarkTools, Random,ColorSchemes,Colors, DelimitedFiles, Graphs, Plots, GraphRecipes

using .InputDistributions, .HomophilicNetworks


function individual_perceptions!(Y,linksPerNodeList)
    maxincomelist = []
    meanincomelist = []
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
        push!(meanincomelist,mean(myvisibleincomes))
        push!(maxincomelist,maximum(myvisibleincomes))
    end
     return ginilist, meanincomelist, maxincomelist ## result: A list of individual mean incomes in ego network    
 #   return ginilist ## result: A list of individual gini estimates
end


function voting_perceptions!(repetitions,population,Rho,sigmaList)
    u = ReentrantLock()
    simulationresults = [[] for i=1:(length(sigmaList) * repetitions + 1)]
    simulationresults[1]=["sigma", "seed", "Rho", "Y", "ginilist", "meanincomelist", "maxincomelist"]
    for i in 1:length(sigmaList)
        thissigma = sigmaList[i]
         ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
        Threads.@threads for j in 1:repetitions
        #for j in 1:repetitions 
            Random.seed!(j)
            Y = income_onegroup_lognormal!(population,thissigma)
            linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(Y,Rho)
            ginilist, meanincomelist, maxincomelist = individual_perceptions!(Y,linkspernodeTHISRUN)
            thisrun = []
            push!(thisrun, thissigma, j, Rho, Y..., ginilist..., meanincomelist..., maxincomelist...)
            simulationresults[1 + (i - 1) * repetitions + j] = thisrun
        end
    end
    simulationresultsnobrackets = []
    for i in simulationresults
        push!(simulationresultsnobrackets,i...)
    end
    return simulationresultsnobrackets
    #income = income_onegroup_lognormal!(population,sigma)
end

sigmas = []
for i in 71:90
    push!(sigmas,i/100)
end

## voting_perceptions!(repetitions,population,Rho,sigmaList)
Rho8par = voting_perceptions!(100,1000,8,sigmas)

## For partial simulation of sigmas:
Rho8par1 = filter!(e->e∉["sigma", "seed", "Rho", "Y", "ginilist", "meanincomelist", "maxincomelist"],Rho8par1)
push!(Rho8par,Rho8parlast...)


CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Gini\\Voting\\Simulationresults LogNormal\\Rho8-lognorm.csv",Tables.table(Rho8par))


## Create adjacency matrix
Random.seed!(1)
incomedistri = income_onegroup_exponential!(1000)
#incomedistri = income_onegroup_lognormal!(1000,1)
linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(incomedistri,8)
adjmatr = create_adjacencymatrix!(linksTHISRUN,1000)

CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Gini\\Voting\\Simulationresults LogNormal\\adjmatrix_exp-perfect.csv",Tables.table(adjmatr))

linksTHISRUN, linkspernodeTHISRUN = perfect_homophilic_linkage!(incomedistri)

networkgraph=Graph(adjmatr)

h = watts_strogatz(50, 6, 0.3)


g = graphfamous("karate")

graphplot(adjmatr)

gplot(networkgraph)
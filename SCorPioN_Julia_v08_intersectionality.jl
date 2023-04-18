#using GraphPlot, SimpleWeightedGraphs, LinearAlgebra, DataFrames, Random, CSV, Colors
#import Cairo, Fontconfig
using StatsBase, Combinatorics, Distributions, LinearAlgebra, PlotlyJS, DataFrames, CSV, Readables, BenchmarkTools, Random,ColorSchemes,Colors, DelimitedFiles, Polylogarithms
#using ArbNumerics,DoubleFloats

testY = combined_incomedistribution!(0,0,[500,500],[1,0.5])
testY.links = homophilic_linkage!(testY.Y,14,1000)
testY.localmeany = gini_estimation!(testY.Y,testY.links)

## Creates subpopulations (from the list of population sizes)
## Draws an income distribution with mean 1 as baseline and multiplies by the relative wages for each sub-population
function combined_incomedistribution!(populations,relativewages)
    exb = Exponential(1)
    df = DataFrame(Y=Vector{Float64}(), priv=Vector{Float64}())
    for i in 1:length(populations)
        y = rand(exb,populations[i])
        relwagenow = relativewages[i]
        for j in 1:populations[i]
            datathisagent = [y[j] * relwagenow, relwagenow]    ##actual income and group belonging (defined by own relative wage)
            push!(df,datathisagent)
        end
    end
    df.Y = df.Y/mean(df.Y)
    return df  ## Sorted by privilege group, NOT income!
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
function homophilic_linkage!(individualDF,Rho, populations)
    incomelist = individualDF[1]
## Builds weights matrix for linkage. 
    weights = zeros(sum(populations),sum(populations))
    ## Given homophilic links, the matrix is symmetric and one can avoid half of the work.
    for i in 1:length(incomelist)
        for j in 1:(i-1)
            weights[i,j]=weights[j,i]
        end
        for j in (i+1):length(incomelist)
            weights[i,j] = exp(-Rho*abs(incomelist[i] - incomelist[j])) # ArbReal(exp(-Rho*abs(Distri[i] - Distri[j])))
        end
    end
## Does the actual sampling via the samps function to determine which links exist
    items = range(1,sum(populations))
    linklist = []
    for i in 1:length(incomelist)
        sampsnow = samps!(i,weights,items)
        for j in 1:length(sampsnow)
            pairnow = [i,sampsnow[j]]
            push!(linklist,sort(pairnow))
        end
    end
## Cleans up the list of existing links and indicates the link-neighbours for each node
    linklist = unique(linklist)
    linksPerNodeList = []
    for i in 1:sum(populations)
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
function gini_estimation!(individualDF)
    linksPerNodeList = individualDF[3]
    Y = individualDF[1]
    meanincomelist = []     ## result: A list of individual mean incomes in ego network
    ginilist = []           ## result: A list of individual gini estimates
    for i in 1:length(individualDF)
        myvisibleincomes = []
        for j in 1:length(linksPerNodeList[i])       ## find all link-neighbours' incomes
            push!(myvisibleincomes,Y[linksPerNodeList[i][j]] )
        end
        mypairs = collect(combinations(myvisibleincomes,2))
        mycomparisons = []
        foreach(x -> push!(mycomparisons,abs(x[1]-x[2])), mypairs)    ## do all possible pairwise combinations
        mygini = mean(mycomparisons)
        mygini = mygini / (mean(myvisibleincomes)*2)               ## divide to calculate the actual gini estimate
        push!(ginilist,mygini)
        push!(meanincomelist,[Y[i],mean(myvisibleincomes)])
    end
    return ginilist 
end

function testginiintersectional!(populations,relativewages,Rho)
    individualDF = combined_incomedistribution!(populations,relativewages)
    individualDF[3] = function homophilic_linkage!(individualDF,Rho,populations)
end
























## Computes the consumption given an income distribution, network and consumption parameters
function consumption_calculation!(p,m,population,Y,wwork,ycap,wcap,c,linksPerNodeList)
    Ccap = ycap * wcap
    C = []
## First calculates the indiosyncratic consumption (as always sorted by income rank)
    for i in 1:population                  ## workers
        push!(C,Y[i] * wwork)
    end
    # for i in (Int(round((1-p)*population))+1):population       ## capitalists
    #     push!(C,Y[i] * bcap * w)
    # end
## Computes status consumption, starting with the highest income
## richest workers' consumption - connected to the capitalist representative consumption
    myidioconsumption = C[population]
    orientation = ((1/p)*(m/(1+m))*wcap)
    ##MEDIAN   - (harmonic((1-p) * population)  *(0.5) ^ ((p*harmonic((1-p)*population) ) / ((1-p)*m) -1 ) / ((m+1)*(p-1)))*wcap
    ##MEAN ((1/p)*(m/(1+m))*wcap)
    if orientation > myidioconsumption
        C = replace(C, myidioconsumption =>  myidioconsumption + ((1-wwork) * c * (orientation - myidioconsumption)))
    end

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
            myconsumption = (myidioconsumption + (1-wwork) * c * (orientation - myidioconsumption))
            C = replace(C, myidioconsumption =>myconsumption)
        end
    end
    Call = vcat(C,Ccap)
    Call = sort(Call)
    return C,Call,Ccap   ## result: A list of workers' consumptions
end


function calculate_decileAPCs!(Call,yall)
    decilelength = trunc(Int,length(Call)/10)
    decileAPCs = []
    for i in 0:9
        startposition = (i * decilelength) + 1
        endposition = (i * decilelength) + decilelength
        thisCallDecile = Call[startposition:endposition]
        thisyallDecile = yall[startposition:endposition]
        thisdecileAPC = sum(thisCallDecile)/sum(thisyallDecile)
        push!(decileAPCs,thisdecileAPC)
    end
    return decileAPCs
end

function compare_variation!(Call,yall)
    cvCall = std(Call) / mean(Call)
    cvyall = std(yall) / mean(yall)
    rate = cvCall / cvyall
end


function behaviorspace_consumption_multiple!(repetitions,p,m,population,RhoRange,wwork,wcapRange,cRange)
    if p > 0 
        relevantPopulation = find_workerPopulation!(p,population)
    else
        relevantPopulation = population
    end
    u = ReentrantLock()
    # creates income distribution and lists of edges per node for later usages
    ylists = []         # nested list with one sublist for each Rho containing the results (Vector of length population) for each MC run using this Rho: [[Rho1-Seed1,...,Rho1-Seedn],...,[Rhon-Seed1,...,Rhon-Seedn]]
    yalllists = [] 
    ycaplists = [] 
    linklists = []
    realginiLists = []
    for i in 1:length(RhoRange)
        thisrhorealginiList = collect(Float64,1:repetitions)
        thisrhoylists = []#  collect(Float64,1:repetitions)
        thisrhoyalllists = []
        thisrhoycaplists = []
        thisrholinklists = []#collect(Float64,1:repetitions)
        for j in 1:repetitions
            push!(thisrhoylists,[])
            push!(thisrhoyalllists,[])
            push!(thisrhoycaplists,[])
            push!(thisrholinklists,[])
        end
        Threads.@threads for j in 1:repetitions   ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
            Random.seed!(j)
            combinedys = combined_incomedistribution!(p,m,population)
            y = combinedys[1]
            yall = combinedys[2]
            ycap = combinedys[3]
            thisRG = real_gini!(y)
            thisNW = homophilic_linkage!(y,RhoRange[i],relevantPopulation)
            Threads.lock(u) do                    ## Lock to avoid race conditions. Maybe not necessary after all...
                thisrhorealginiList[j] = thisRG
            end
            Threads.lock(u) do
                thisrhoylists[j] = y
            end
            Threads.lock(u) do
                thisrhoycaplists[j] = ycap
            end
            Threads.lock(u) do
                thisrhoyalllists[j] = yall
            end
            Threads.lock(u) do
                thisrholinklists[j] = thisNW
            end
        end
        push!(realginiLists,thisrhorealginiList)
        push!(ylists, thisrhoylists)
        push!(yalllists, thisrhoyalllists)
        push!(ycaplists,thisrhoycaplists)
        push!(linklists,thisrholinklists)
    end
    Worlddescription = [ylists,linklists,realginiLists]
    Output =[]
    gini_estimation!(Y,linksPerNodeList)
    for i in 1:length(RhoRange)
        for j in 1:length(wcapRange)
            for k in 1:length(cRange)
                APCsList = []
                aggAPCList = []
                DecileAPCsList =[]
                CList = []
                CVrateList = []
                for j in 1:repetitions
                    push!(APCsList,[])
                    push!(aggAPCList,[])
                    push!(CList,[])
                    push!(DecileAPCsList,[])
                    push!(CVrateList,[])
                end
                for l in 1:repetitions
                    Random.seed!(l)
                    y=ylists[i][l]
                    yall=yalllists[i][l]
                    ycap=ycaplists[i][l]
                    combinedCs = consumption_calculation!(p,m,relevantPopulation,y,wwork,ycap,wcapRange[j],cRange[k],linklists[i][l])
                    C = combinedCs[1]
                    Call = combinedCs[2]
                    Ccap = combinedCs[3] 
                    APCs = C./y
                    APCsall = Call./yall
                    DecileAPCs = calculate_decileAPCs!(Call,yall)
                    aggAPC = (sum(C) + (wcapRange[j]*population*(m/(1+m)))) /population
                    CVrate = compare_variation!(Call,yall)
                    Threads.lock(u) do
                        APCsList[l] = APCs
                    end
                    Threads.lock(u) do
                        aggAPCList[l] = aggAPC
                    end
                    Threads.lock(u) do
                        CList[l] = C
                    end
                    Threads.lock(u) do
                        DecileAPCsList[l] = DecileAPCs
                    end
                    Threads.lock(u) do
                        CVrateList[l] = CVrate
                    end
                end
                meanAPCsByPos = []
                meanCByPos = []
                for q in 1:relevantPopulation
                    thisincomeAPCs = []
                    thisincomeCs = []
                    for r in 1:repetitions
                        push!(thisincomeAPCs,APCsList[r][q])
                        push!(thisincomeCs,CList[r][q])
                    end
                    push!(meanAPCsByPos,mean(thisincomeAPCs))
                    push!(meanCByPos,mean(thisincomeCs))
                end
                push!(Output,[p,m,RhoRange[i],wcapRange[j],meanAPCsByPos,meanCByPos,aggAPCList,DecileAPCsList,CVrateList])
                #push!(Output,[RhoRange[i],wcapRange[j],cRange[k],meanCByPos,meanAPCsByPos,aggAPCList])
            end
        end
    end
    return Output#, Worlddescription
end


function profitshareConsumption_multiple_mp!(repetitions,pRange,mRange,population,RhoRange,wwork,wcapRange,cRange)
    SimulationResults = []
    for j in 1:length(pRange)
    for i in 1:length(mRange)
        thism = behaviorspace_consumption_multiple!(repetitions,pRange[j],mRange[i],population,RhoRange,wwork,wcapRange,cRange)
        push!(SimulationResults,thism)
    end
    end
    return SimulationResults
end


function behaviorspace_perception_multiple!(repetitions,p,m,population,RhoRange)
    if p > 0 
        relevantPopulation = find_workerPopulation!(p,population)
    else
        relevantPopulation = population
    end
    u = ReentrantLock()
    # creates income distribution and lists of edges per node for later usages
    ylists = []         # nested list with one sublist for each Rho containing the results (Vector of length population) for each MC run using this Rho: [[Rho1-Seed1,...,Rho1-Seedn],...,[Rhon-Seed1,...,Rhon-Seedn]]
    yalllists = [] 
    ycaplists = [] 
    linklists = []
    realginiLists = []
    for i in 1:length(RhoRange)
        thisrhorealginiList = collect(Float64,1:repetitions)
        thisrhoylists = []#  collect(Float64,1:repetitions)
        thisrhoyalllists = []
        thisrhoycaplists = []
        thisrholinklists = []#collect(Float64,1:repetitions)
        for j in 1:repetitions
            push!(thisrhoylists,[])
            push!(thisrhoyalllists,[])
            push!(thisrhoycaplists,[])
            push!(thisrholinklists,[])
        end
        Threads.@threads for j in 1:repetitions   ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
            Random.seed!(j)
            y = combined_incomedistribution!(p,m,population)
            ## UNCOMMENT & CHANGE TO INCLUDE PROFITSHARE!
            #   y = combinedys[1]
                #   yall = combinedys[2]
            #   ycap = combinedys[3]
            thisRG = real_gini!(y)
            thisNW = homophilic_linkage!(y,RhoRange[i],relevantPopulation)
            Threads.lock(u) do                    ## Lock to avoid race conditions. Maybe not necessary after all...
                thisrhorealginiList[j] = thisRG
            end
            Threads.lock(u) do
                thisrhoylists[j] = y
            end
            Threads.lock(u) do
           #     thisrhoycaplists[j] = ycap
            end
            Threads.lock(u) do
           #     thisrhoyalllists[j] = yall
            end
            Threads.lock(u) do
                thisrholinklists[j] = thisNW
            end
        end
        push!(realginiLists,thisrhorealginiList)
        push!(ylists, thisrhoylists)
        push!(yalllists, thisrhoyalllists)
        push!(ycaplists,thisrhoycaplists)
        push!(linklists,thisrholinklists)
    end
    Worlddescription = [ylists,linklists,realginiLists]
    Output =[]
    for i in 1:length(RhoRange)
        perceptionlist = []
        for j in 1:repetitions
            push!(perceptionlist,[])
        end
        Threads.@threads for l in 1:repetitions
            Random.seed!(l)
            thisylist=ylists[i][l]
            perceptions = gini_estimation!(thisylist,linklists[i][l])
            Threads.lock(u) do
                perceptionlist[l] = perceptions
            end
        end
        push!(Output,[RhoRange[i],perceptionlist])
        #push!(Output,[RhoRange[i],wcapRange[j],cRange[k],meanCByPos,meanAPCsByPos,aggAPCList])
    end
    return Output#, Worlddescription
end


resultspls = behaviorspace_consumption_multiple!(25,0.01,0.3,1000,4,1,1,[0.5,1],0.5)

resultspls = behaviorspace_consumption_multiple!(10,0.01,1,1000,4,0.5,[0.1,0.5],0.5)

allp1 = profitshareConsumption_multiple_mp!(1,[0.01 0.02 0.03 0.04 0.05],[0.5, 0.6, 0.7, 0.8,0.9,1],1000,[0,1,4,8,14],0.5,[0.1,0.2,0.3,0.4,0.499,0.5],0.5)

allp = profitshareConsumption_multiple_mp!(100,[0.01 0.02 0.03 0.04 0.05],[0.5, 0.6, 0.7, 0.8,0.9,1],1000,[0,1,4,8,14],0.5,[0.1,0.2,0.3,0.4,0.499,0.5],0.5)

wcap0499 = profitshareConsumption_multiple_mp!(100,[0.01 0.02 0.03 0.04 0.05],[0.5, 0.6, 0.7, 0.8,0.9,1],1000,[0,1,4,8,14],0.5,0.499,0.5)

test = behaviorspace_perception_multiple!(100,0,0,1000,[1,4,8,14])

p001 = profitshareConsumption_multiple_m!(100,0.01,[0.5, 0.6, 0.7, 0.8,0.9,1],1000,[0,1,4,8,14],0.5,[0.1,0.2,0.3,0.4,0.5],0.5)

testdf = DataFrame(ownIncome=test)

bla = test[1]

function prepare!(df,part)
    OwnIncome = []
    OtherIncome = []
    agentNR = []
for i in 1:100
    for j in 1:1000
        push!(agentNR,j)
        push!(OwnIncome,df[part][2][i][j][1])
        push!(OtherIncome,df[part][2][i][j][2])
    end
end
return agentNR, OwnIncome,OtherIncome
end

bla = prepare!(test,4)
rho14 = DataFrame(agentNR=bla[1],ownIncome=bla[2],meanIncomeEgpNetwork=bla[3])
CSV.write("C:\\Users\\dmayerh\\OneDrive - Hotmail\\OneDrive\\MODUS\\Projekte-Jan\\Gini\\Voting\\PerceivedMeanIncome\\Output\\rho14.csv",rho14)


outputdf = DataFrame(p=Vector{Float64}(), m=Vector{Float64}(),Rho=Vector{Float64}(),wcap=Vector{Float64}(),aggAPCs=Vector{Vector{Float64}}())
outputdf = DataFrame(p=Vector{Float64}(), m=Vector{Float64}(),Rho=Vector{Float64}(),wcap=Vector{Float64}(),meanAPCsByPos=Vector{Vector{Float64}}(),meanCByPos=Vector{Vector{Float64}}())

outputdf = DataFrame(p=Vector{Float64}(), m=Vector{Float64}(),Rho=Vector{Float64}(),wcap=Vector{Float64}(),meanAPCsByPos=Vector{Vector{Float64}}(),meanCByPos=Vector{Vector{Float64}}(),aggAPCs=Vector{Vector{Float64}}(),DecileAPCs=Vector{Vector{Vector{Float64}}}(),CVrateList=Vector{Vector{Float64}}())

length(allp)
push!(outputdf,[1,1,1,1,[2,3]])

outputdffilled = filldf!(outputdf,allp)

function filldf!(df,list)
    dffull = df
    for i in 1:length(list)
        for j in 1:length(list[i])
            push!(df,list[i][j])
        end
    end
    return dffull
end

DataFrame(blah=p001[1][1])

CSV.write("C:\\Users\\ba2mr5\\OneDrive\\MODUS\\Projekte-Jan\\Gini\\Consumption\\Profitshare\\Output\\Deciles-Firsttry.csv", outputdffilled)

CSV.write("C:\\Users\\ba2mr5\\OneDrive\\MODUS\\Projekte-Jan\\Gini\\Consumption\\Profitshare\\Output\\INDIVIDUAL-APC-C-05-1_p-001-005_wcap-01-05_rho.csv", outputdffilled)


datatoday = CSV.read("C:\\Users\\ba2mr5\\OneDrive\\MODUS\\Projekte-Jan\\Gini\\Consumption\\Profitshare\\Output\\aggAPCs_all_2MCruns.csv", DataFrame)

CSV.write("C:\\Users\\ba2mr5\\OneDrive\\MODUS\\Projekte-Jan\\Gini\\Consumption\\Profitshare\\Output\\p001-03052022.csv", Tables.table(p001), writeheader=false,bufsize=14792477517)

datatoday = readdlm("C:\\Users\\ba2mr5\\OneDrive\\MODUS\\Projekte-Jan\\Gini\\Consumption\\Profitshare\\Output\\p001-03052022.csv", ',', Float64) 


## Parameters: ( repetitions, p , mRange , population , RhoRange , wwork , wcapRange , cRange )
## Parameters: ( repetitions, p , m , population , RhoRange , bwork , bcapRange , cRange ,w )


## Output:
## m
## => 1. value: Final Output Values (for each combination of simulation inputs!)
## Final Output Values (Consumption)
##  1. value: Rho used
##  2. value: wcap used
##  3. value: c used
##  4. list (length: population):  consumptions by income rank (aggregate over all repetitions/MC-Runs)
##  5. list (length: population):  APCs by income rank (aggregate over all repetitions/MC-Runs)
##  6. list (length: repetitions): mean APC per MC-Run
##
## => 2. value: world description



function draw_plots!(simulationoutput)
    output = simulationoutput[1]
    names=[]
    rho=[]
    wcap=[]
    c=[]
    aggAPCs=[]
    theplot=[]
    thescatter=[]
    for i in 1:length(output)
        push!(theplot,violin(y=output[i][6], name=string("Rho=",output[i][1]," bcap=",output[i][2]," c=",output[i][3]),meanline_visible=true ))
        push!(thescatter,scatter(x=1:1000, y=output[i][4],mode="markers", name=string("Rho=",output[i][1]," bcap=",output[i][2]," c=",output[i][3]),meanline_visible=true ))
    end
 #   df = DataFrame(rho=rho,bcap=bcap,c=c,aggAPC=aggAPCs)
 #   theplot=plot(df, x=:c, y=:aggAPC,kind="box")
    return theplot
end

function draw_plotsByM!(simulationoutputs)
    wcap005=[]
    wcap01=[]
    wcap015=[]
    wcap02=[]
    wcap025=[]
    wcap03=[]
    wcap035=[]
    wcap04=[]
    for i in 1:length(simulationoutputs)
        output = simulationoutputs[i][1]
        push!(wcap005,violin(y=output[1][6], name="bcap = 0.2",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[1], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(wcap01,violin(y=output[2][6], name="bcap = 0.3",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[2], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(wcap015,violin(y=output[3][6], name="bcap = 0.4",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[3], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(wcap02,violin(y=output[4][6], name="bcap = 0.5",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[4], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(wcap025,violin(y=output[5][6], name="bcap = 0.6",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[5], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(wcap03,violin(y=output[6][6], name="bcap = 0.7",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[6], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(wcap035,violin(y=output[7][6], name="bcap = 0.8",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[7], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(wcap04,violin(y=output[8][6], name="bcap = 0.9",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[8], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
    end
    theplot=plot([
        bcap02[1],bcap03[1],bcap04[1],bcap05[1],bcap06[1],bcap07[1],bcap08[1],bcap09[1],bcap02[2],bcap03[2],bcap04[2],bcap05[2],bcap06[2],bcap07[2],bcap08[2],bcap09[2],
        bcap02[3],bcap03[3],bcap04[3],bcap05[3],bcap06[3],bcap07[3],bcap08[3],bcap09[3],
        bcap02[4],bcap03[4],bcap04[4],bcap05[4],bcap06[4],bcap07[4],bcap08[4],bcap09[4],
        bcap02[5],bcap03[5],bcap04[5],bcap05[5],bcap06[5],bcap07[5],bcap08[5],bcap09[5],
        bcap02[6],bcap03[6],bcap04[6],bcap05[6],bcap06[6],bcap07[6],bcap08[6],bcap09[6],
        ],
        Layout(title="p = 0.01 -- Rho = 4 -- m from 0.6 to 1.1",violinmode="group"))
    return theplot #bcap02, bcap03, bcap04,bcap05,bcap06,bcap07,bcap08,bcap09
end

plotsbym=draw_plotsByM!([m06,m07,m08,m09,m10,m11])

function draw_boxplotsByM!(simulationoutputs)
    bcap02=[]
    bcap03=[]
    bcap04=[]
    bcap05=[]
    bcap06=[]
    bcap07=[]
    bcap08=[]
    bcap09=[]
    for i in 1:length(simulationoutputs)
        output = simulationoutputs[i][1]
        push!(bcap02,box(y=output[1][6], name="bcap = 0.2",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[1], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(bcap03,box(y=output[2][6], name="bcap = 0.3",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[2], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(bcap04,box(y=output[3][6], name="bcap = 0.4",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[3], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(bcap05,box(y=output[4][6], name="bcap = 0.5",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[4], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(bcap06,box(y=output[5][6], name="bcap = 0.6",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[5], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(bcap07,box(y=output[6][6], name="bcap = 0.7",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[6], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(bcap08,box(y=output[7][6], name="bcap = 0.8",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[7], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
        push!(bcap09,box(y=output[8][6], name="bcap = 0.9",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[8], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
    end
    theplot=plot([
        bcap02[1],bcap03[1],bcap04[1],bcap05[1],bcap06[1],bcap07[1],bcap08[1],bcap09[1],bcap02[2],bcap03[2],bcap04[2],bcap05[2],bcap06[2],bcap07[2],bcap08[2],bcap09[2],
        bcap02[3],bcap03[3],bcap04[3],bcap05[3],bcap06[3],bcap07[3],bcap08[3],bcap09[3],
        bcap02[4],bcap03[4],bcap04[4],bcap05[4],bcap06[4],bcap07[4],bcap08[4],bcap09[4],
        bcap02[5],bcap03[5],bcap04[5],bcap05[5],bcap06[5],bcap07[5],bcap08[5],bcap09[5],
        bcap02[6],bcap03[6],bcap04[6],bcap05[6],bcap06[6],bcap07[6],bcap08[6],bcap09[6],
        ],
        Layout(title="p = 0.01 -- Rho = 4 -- m from 0.6 to 1.1",boxmode="group"))
    return theplot #bcap02, bcap03, bcap04,bcap05,bcap06,bcap07,bcap08,bcap09
end

plotsbym=draw_boxplotsByM!([m06,m07,m08,m09,m10,m11])

plotssofar = draw_plots!(m06)
plotsm06=draw_plots!(m06)
plot([plotssofar[1],plotssofar[2],plotssofar[3],plotssofar[4],plotssofar[5],plotssofar[6],plotssofar[7],plotssofar[8]],Layout(title="p = 0.01 -- m = 0.6") )

ColorSchemes.mk_8.colors[1]

plot(violin(y=m06[1][6], name="bcap = 0.2",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[1], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))
plot(scatter(x=1:1000, y=resultspls[1][1][4]))



test_CAPKONSUM!(0.05,1)

plot(violin(y=resultsplsm1[1][1][6], name="m = 1",marker=attr(symbol="line-ew", color=ColorSchemes.mk_8.colors[1], line=attr(color=ColorSchemes.mk_8.colors[1], width=2)),meanline_visible=true ))

resultspls[1][1][6]

## Output:
## Final Output Values (Consumption)
##  1. value: Rho used
##  2. value: bcap used
##  3. value: c used
##  4. list (length: population):  consumptions by income rank (aggregate over all repetitions/MC-Runs)
##  5. list (length: population):  APCs by income rank (aggregate over all repetitions/MC-Runs)
##  6. list (length: repetitions): mean APC per MC-Run



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
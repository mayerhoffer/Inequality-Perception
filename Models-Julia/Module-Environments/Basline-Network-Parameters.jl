include("InputDistributions.jl")
using .InputDistributions

include("HomophilicNetworks.jl")
using .HomophilicNetworks

using Random, CSV, StatFiles

## Beta distribution
function network_beta(repetitions,population, alpha, beta, Rho)
    u = ReentrantLock()
    simulationresults = [[] for i=1:(repetitions + 1)]
    simulationresults[1]=["seed", "Rho", "inputdistribution", "degreedistribution"]

    ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
    Threads.@threads for i in 1:repetitions
        Random.seed!(i)
        sorted_inputattitudes = InputDistributions.income_onegroup_beta(population, alpha, beta)
        linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.homophilic_linkage!(sorted_inputattitudes,Rho)
        degreedistri = HomophilicNetworks.calculate_degree_distribution!(linkspernodeTHISRUN)
        thisrun = []
        push!(thisrun, i, Rho, sorted_inputattitudes, degreedistri)
        simulationresults[1 + i] = thisrun
    end
    simulationresultsnobrackets = []
    for i in simulationresults
        push!(simulationresultsnobrackets,i...)
    end
    return simulationresultsnobrackets
end

beta_05_05_Rho_8 = network_beta(100, 1000, 0.5, 0.5, 8)
CSV.write("C:\\Users\\dmaye\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\Degrees\\beta_05_05_Rho_8.csv",Tables.table(beta_05_05_Rho_8))



function network_exponential(repetitions,population,Rho)
    u = ReentrantLock()
    simulationresults = [[] for i=1:(repetitions + 1)]
    simulationresults[1]=["seed", "Rho", "inputdistribution", "degreedistribution"]

    ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
    Threads.@threads for i in 1:repetitions
        Random.seed!(i)
        sorted_inputattitudes = InputDistributions.income_onegroup_exponential!(population)
        linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.homophilic_linkage!(sorted_inputattitudes,Rho)
        degreedistri = HomophilicNetworks.calculate_degree_distribution!(linkspernodeTHISRUN)
        thisrun = []
        push!(thisrun, i, Rho, sorted_inputattitudes, degreedistri)
        simulationresults[1 + i] = thisrun
    end
    simulationresultsnobrackets = []
    for i in simulationresults
        push!(simulationresultsnobrackets,i...)
    end
    return simulationresultsnobrackets
end

exponential_Rho_8 = network_exponential(100,1000,8)
CSV.write("C:\\Users\\dmaye\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\Degrees\\exponential_Rho_8.csv",Tables.table(exponential_Rho_8))   


function network_lognormal(repetitions,population,Rho,sigma)
    u = ReentrantLock()
    simulationresults = [[] for i=1:(repetitions + 1)]
    simulationresults[1]=["seed", "Rho", "inputdistribution", "degreedistribution"]

    ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
    Threads.@threads for i in 1:repetitions
        Random.seed!(i)
        sorted_inputattitudes = InputDistributions.income_onegroup_lognormal!(population,sigma)
        linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.homophilic_linkage!(sorted_inputattitudes,Rho)
        degreedistri = HomophilicNetworks.calculate_degree_distribution!(linkspernodeTHISRUN)
        thisrun = []
        push!(thisrun, i, Rho, sorted_inputattitudes, degreedistri)
        simulationresults[1 + i] = thisrun
    end
    simulationresultsnobrackets = []
    for i in simulationresults
        push!(simulationresultsnobrackets,i...)
    end
    return simulationresultsnobrackets
end

lognormal_Rho_8_sigma_1point5 = network_lognormal(100,1000,8,1.5)
CSV.write("C:\\Users\\dmaye\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\Degrees\\lognormal_Rho_8_sigma_1point5.csv",Tables.table(lognormal_Rho_8_sigma_1point5 ))  

lognormal_Rho_8_sigma_2 = network_lognormal(100,1000,8,2)
CSV.write("C:\\Users\\dmaye\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\Degrees\\lognormal_Rho_8_sigma_2.csv",Tables.table(lognormal_Rho_8_sigma_2))

lognormal_Rho_8_sigma_2point5 = network_lognormal(100,1000,8,2.5)
CSV.write("C:\\Users\\dmaye\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\Degrees\\lognormal_Rho_8_sigma_2point5.csv",Tables.table(lognormal_Rho_8_sigma_2point5)) 

lognormal_Rho_8_sigma_3 = network_lognormal(100,1000,8,3)
CSV.write("C:\\Users\\dmaye\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\Degrees\\lognormal_Rho_8_sigma_3.csv",Tables.table(lognormal_Rho_8_sigma_3))

lognormal_Rho_8_sigma_3point5 = network_lognormal(100,1000,8,3.5)
CSV.write("C:\\Users\\dmaye\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\Degrees\\lognormal_Rho_8_sigma_3point5.csv",Tables.table(lognormal_Rho_8_sigma_3point5)) 


function network_uniform(repetitions,population,Rho)
    u = ReentrantLock()
    simulationresults = [[] for i=1:(repetitions + 1)]
    simulationresults[1]=["seed", "Rho", "inputdistribution", "degreedistribution"]

    ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
    Threads.@threads for i in 1:repetitions
        Random.seed!(i)
        sorted_inputattitudes = InputDistributions.income_onegroup_uniform!(population)
        linksTHISRUN, linkspernodeTHISRUN = HomophilicNetworks.homophilic_linkage!(sorted_inputattitudes,Rho)
        degreedistri = HomophilicNetworks.calculate_degree_distribution!(linkspernodeTHISRUN)
        thisrun = []
        push!(thisrun, i, Rho, sorted_inputattitudes, degreedistri)
        simulationresults[1 + i] = thisrun
    end
    simulationresultsnobrackets = []
    for i in simulationresults
        push!(simulationresultsnobrackets,i...)
    end
    return simulationresultsnobrackets
end

uniform_Rho_8 = network_uniform(100,1000,8)
CSV.write("C:\\Users\\dmaye\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\Degrees\\uniform_Rho_8.csv",Tables.table(uniform_Rho_8))
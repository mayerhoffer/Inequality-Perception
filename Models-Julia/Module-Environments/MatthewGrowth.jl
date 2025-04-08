
using Random, Statistics, StatsPlots

using .InputDistributions, .HomophilicNetworks, .IndividualPerceptions


Random.seed!(1)
incomedistri = income_onegroup_exponential!(1000)
linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(incomedistri,4)


function generate_matthew_growth!(inputdistribution, psi)
    newvalues = (inputdistribution.+ 1) .^(1 + psi) .-1
    growthrate = (newvalues .- inputdistribution) ./ inputdistribution
    return growthrate
end

growthrates = generate_matthew_growth!(incomedistri, 0.02)

function growthperception!(Rho, psi)
    Random.seed!(100)
    incomedistri = income_onegroup_exponential!(1000)
    growthrates = generate_matthew_growth!(incomedistri, psi)
    linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(incomedistri,Rho)
    observedgrowthrates = find_visible_nodes!(growthrates,linkspernodeTHISRUN)
    perceivedgrowthMEAN, perceivedgrowthMAX, perceivedgrowthMIN, perceivedgrowthSTD = distri_perceptions!(observedgrowthrates)
    return incomedistri, growthrates, perceivedgrowthMEAN, perceivedgrowthMAX, perceivedgrowthMIN, perceivedgrowthSTD
end


incomedistriRho080, growthratesRho080, perceivedgrowthMEANRho080, perceivedgrowthMAXRho080, perceivedgrowthMINRho080, perceivedgrowthSTDRho080 = growthperception!(80, 0.02)
incomedistriRho040, growthratesRho040, perceivedgrowthMEANRho040, perceivedgrowthMAXRho040, perceivedgrowthMINRho040, perceivedgrowthSTDRho040 = growthperception!(4, 0.02)
incomedistriRho015, growthratesRho015, perceivedgrowthMEANRho015, perceivedgrowthMAXRho015, perceivedgrowthMINRho015, perceivedgrowthSTDRho015 = growthperception!(1.5, 0.02)

ranks = 1:length(growthrates)

scatter(ranks, perceivedgrowthMEANRho015, xlabel="Rank", ylabel="Perceived Growth", label="Rho = 1.5", title="Growth Perceptions (Mean of Egonetwork)")
scatter!(ranks, perceivedgrowthMEANRho040, label="Rho = 4")
scatter!(ranks, perceivedgrowthMEANRho080, label="Rho = 8")
hline!([mean(growthratesRho040)], label="Population Mean Growth", color=:gray)

scatter(ranks, perceivedgrowthSTDRho015, xlabel="Rank", ylabel="Perceived Growth Dispersion", label="Rho = 1.5", title="Perception of Growth Dispersion (SD)")
scatter!(ranks, perceivedgrowthSTDRho040, label="Rho = 4")
scatter!(ranks, perceivedgrowthSTDRho080, label="Rho = 8")
hline!([std(growthratesRho040)], label="Population Growth SD", color=:gray)


scatter(ranks, growthratesRho015 - perceivedgrowthMEANRho015, xlabel="Rank", ylabel="Perceived Own Relative Growth", label="Rho = 1.5", title="Own Growth vs Local Mean")
scatter!(ranks, growthratesRho040 - perceivedgrowthMEANRho040, label="Rho = 4")
scatter!(ranks, growthratesRho080 - perceivedgrowthMEANRho080, label="Rho = 8")
scatter!(ranks, growthratesRho040 .- (mean(growthratesRho040)), label="vs Global Mean", color=:gray)

scatter(ranks, growthratesRho040, xlabel="Rank", ylabel="Own Growth", legend=false, title="Growth Rates", color=:purple)
hline!([mean(growthratesRho040)], color=:gray)


median(perceovedgrowthMEAN)
mean(perceovedgrowthSTD)


mean(grownincomes)
std(grownincomes)

testvector = [1, 2, 3]


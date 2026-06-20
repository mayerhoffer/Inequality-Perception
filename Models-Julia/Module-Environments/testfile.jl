using Revise

include("InputDistributions.jl")
using .InputDistributions

include("HomophilicNetworks.jl")
using .HomophilicNetworks

using Plots

inputdistri = InputDistributions.income_onegroup_exponential!(1000)
inputdistri = InputDistributions.income_onegroup_beta(1000, 0.5, 0.5)
inputdistri = InputDistributions.income_onegroup_uniform!(1000)
inputdistri = InputDistributions.income_onegroup_lognormal!(1000, 2)

network = HomophilicNetworks.homophilic_linkage!(inputdistri, 8)



test = HomophilicNetworks.homophilic_linkage!(InputDistributions.income_onegroup_uniform!(1000), 20)

degreedistri = HomophilicNetworks.calculate_degree_distribution!(network[2])


## Visualise the distribution of degrees
histogram(degreedistri, bins=0:maximum(degreedistri), xticks=0:maximum(degreedistri), xlabel="Degree", ylabel="Frequency", title="Degree Distribution")

## Plot the degreedistri against the rank of the nodes (i.e. inex of the degree in degreedistri)
scatter(1:length(degreedistri), degreedistri, xlabel="Node Rank", ylabel="Degree", title="Degree vs Node Rank", legend=false)

## create a joint dataframe of inputdistri and degreedistri
using DataFrames
df = DataFrame(income=inputdistri, degree=degreedistri)
## Plot degree against income
scatter(df.income, df.degree, xlabel="Income", ylabel="Degree", title="Degree vs Income", legend=false)

## Plot degree against income on a log scale
scatter(df.income, df.degree, xlabel="Income", ylabel="Degree", title="Degree vs Income (Log Scale)", legend=false, yscale=:log10)



## Plot degree as boxplot for different income groups: <1 , 1-2, 2-3, 3-4, >4 without using cut function
df.income_group = ifelse.(df.income .< 1, "<1",
                    ifelse.(df.income .< 2, "1-2",
                    ifelse.(df.income .< 3, "2-3",
                    ifelse.(df.income .< 4, "3-4", ">4"))))
boxplot(df.income_group, df.degree, xlabel="Income Group", ylabel="Degree", title="Degree by Income Group", legend=false)


## Plot degree as boxplot for income quartiles
df.income_quartile = ifelse.(df.income .< quantile(df.income, 0.25), "Q1",
                        ifelse.(df.income .< quantile(df.income, 0.5), "Q2",
                        ifelse.(df.income .< quantile(df.income, 0.75), "Q3", "Q4")))
boxplot(df.income_quartile, df.degree, xlabel="Income Quartile", ylabel="Degree", title="Degree by Income Quartile", legend=false)

using Statistics
using StatsPlots

## Plot degree as boxplot for income deciles. First, define the decile midpoints
deciles = quantile(df.income, 0:0.1:1)

## Then, create a new column in the DataFrame for the decile groups
df.income_decile = ifelse.(df.income .< deciles[2], "D01",
                        ifelse.(df.income .< deciles[3], "D02",
                        ifelse.(df.income .< deciles[4], "D03",
                        ifelse.(df.income .< deciles[5], "D04",
                        ifelse.(df.income .< deciles[6], "D05",
                        ifelse.(df.income .< deciles[7], "D06",
                        ifelse.(df.income .< deciles[8], "D07",
                        ifelse.(df.income .< deciles[9], "D08",
                        ifelse.(df.income .< deciles[10], "D09", "D10")))))))))


## Finally, create the boxplot for the deciles
boxplot(df.income_decile, df.degree, xlabel="Income Decile", ylabel="Degree", title="Degree by Income Decile", legend=false)
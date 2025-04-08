using .HomophilicNetworks, .IndividualPerceptions


## Empirical Inputs
using StatFiles, DataFrames, CSV, Statistics, StatsPlots, Random


## USED ONLY ONCE TO SUBSET THE HUGE ALLBUS DATASET
# ALLBUS_df = DataFrame(load("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\ALLBUS-Study.dta"))

# CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\ALLBUS-Study.csv",ALLBUS_df)

# ALLBUS_mig_2021 = ALLBUS_df[ALLBUS_df.year .== 2021, [:mi05, :mi06, :mi07, :mi08, :mi09, :mi10, :mi11, :mp16, :mp17, :mp18, :mp19]]

# variables = [:mi05, :mi06, :mi07, :mi08, :mi09, :mi10, :mi11, :mp16, :mp17, :mp18, :mp19]
# ## Calculate frequencies for each variable
# for variable in variables
#     println("Frequencies for $variable:")
#     freqs = combine(groupby(ALLBUS_mig_2021, variable), nrow => :Count)
#     println(freqs)
#     println()
# end

# # Filter rows where no mp questions were asked
# ALLBUS_mig_2021 = ALLBUS_mig_2021[ALLBUS_mig_2021.mp16 .!= -11, :]

# # Recode negative values to NA
# foreach(col -> ALLBUS_mig_2021[!, col] .= ifelse.(ALLBUS_mig_2021[!, col] .< 0, missing, ALLBUS_mig_2021[!, col]), names(ALLBUS_mig_2021) )

# CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\ALLBUS_mig_2021.csv",ALLBUS_mig_2021)

# dropmissing!(ALLBUS_mig_2021)



## Prepare data first time.
# ALLBUS_mig_2021 = CSV.read("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\ALLBUS_mig_2021.csv", DataFrame)


# mp_cols = names(ALLBUS_mig_2021, r"^mp")

# for col in mp_cols
#     inverted_col = Symbol(string(col) * "-inv")  # Name for the inverted column
#     ALLBUS_mig_2021[!, inverted_col] = 6 .- ALLBUS_mig_2021[!, col]  # Invert the scale
# end

# ALLBUS_mig_2021 = dropmissing!(select!(ALLBUS_mig_2021, Not(mp_cols)))

# ALLBUS_mig_2021[!, :mean] = [mean(skipmissing(row)) for row in eachrow(ALLBUS_mig_2021)]

# #ALLBUS_mig_2021.mean = [isempty(skipmissing(row)) ? missing : mean(skipmissing(row)) for row in eachrow(ALLBUS_mig_2021)]

# histogram(ALLBUS_mig_2021.mean, bins=50, xlabel="Mean", ylabel="", title="Distribution of Mean Migration Attitudes", legend=false)

# sorted_migattitudes = sort(ALLBUS_mig_2021.mean)
# popsize = length(sorted_migattitudes)

ALLBUS_mig_2021 = CSV.read("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\sorted_migattitudes.csv", DataFrame)
sorted_migattitudes = ALLBUS_mig_2021[!,1]

migmean = mean(sorted_migattitudes)
normalised_sorted_migattitudes = sorted_migattitudes / migmean
#CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\normalised_sorted_migattitudes.csv",Tables.table(normalised_sorted_migattitudes))

## Create network
Random.seed!(1)
linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(sorted_migattitudes,8)

visiblenodes = find_visible_nodes!(sorted_migattitudes,linkspernodeTHISRUN)

perceivedmeanattitude,perceivedmaxattitude,perceivedminattitude, perceivedstdattitude = distri_perceptions!(visiblenodes)

perceivedattitudespan = perceivedmaxattitude .-perceivedminattitude
mean(perceivedattitudespan)

## Create adjacency matrix
adjmatr = create_adjacencymatrix!(linksTHISRUN,popsize)


#CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\adjmatrix_allbus.csv",Tables.table(adjmatr))
#CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\sorted_migattitudes.csv",Tables.table(sorted_migattitudes))

calculate_average_path_length!(adjmatr)
#calculate_clustering_coefficient!(linksTHISRUN, linkspernodeTHISRUN)


# Convert to DataFrame
data = DataFrame(X=sorted_migattitudes, Y=perceivedattitudespan)

# Bin X values into intervals of 0.5
bin_size = 0.5
data.X_binned = round.(data.X ./ bin_size) .* bin_size  # Round to nearest 0.5


# Create the boxplot
@df data violin(
    :X_binned, :Y,  # Grouped by binned categories
    xlabel="Own Attitude",
    ylabel="Perceived Attitude Span",
#    title="Box-Whisker Plot of Perceived Attitude Span by Binned Own Attitude",
    legend=false
)
@df data dotplot!(
    :X_binned, :Y,  # Grouped by binned categories
    xlabel="Own Attitude",
    ylabel="Perceived Attitude Span",
    legend=false
)


# Self-perceptions
selfperceptions = self_perceptions!(visiblenodes,sorted_migattitudes)
histogram(selfperceptions, bins=10, ylabel="", title="Selfperceptions", legend=false)
maximum(selfperceptions)


## Calculate bias in  segregation perception: Ashman's D
## Calculate perceived segregation
# mu1 mean of own ego network
# mu2 of random other ego network
# sigma1 = SD of own ego network
# sigma2 = SD of random other ego network
mu2subj = mean(sorted_migattitudes)

function calculate_perceivedAshmansDs!(perceivedmeanattitude, perceivedstdattitude, muglobal, sigmaglobal)
    n = length(perceivedmeanattitude)
    perceptionlist_localAshmansD = Vector{Float64}(undef, n)
    perceptionlist_globalAshmansD = Vector{Float64}(undef, n)
    for i in 1:n
        outgroup = rand(1:n)
        localAshmansD = abs(perceivedmeanattitude[i] - perceivedmeanattitude[outgroup]) / sqrt(2 * (perceivedstdattitude[i] + perceivedstdattitude[outgroup] + exp(-12)))
        perceptionlist_localAshmansD[i] = localAshmansD
        globalAshmansD = abs(perceivedmeanattitude[i] - muglobal) / sqrt(2 * (perceivedstdattitude[i] + sigmaglobal + exp(-12))) ## check!
        perceptionlist_globalAshmansD[i] = globalAshmansD
    end
    return perceptionlist_localAshmansD, perceptionlist_globalAshmansD
end

perceivedDs, globalDs = calculate_perceivedAshmansDs!(perceivedmeanattitude, perceivedstdattitude, mean(sorted_migattitudes), std(sorted_migattitudes))
subjAshmansD = mean(perceivedDs)

histogram(perceivedDs, bins=50, ylabel="", title="Perceived Segregation (Ashman's D)", legend=false)



## Pipeline
function segregation_perceptions!(repetitions,sorted_inputattitudes,Rho)
    u = ReentrantLock()
    simulationresults = [[] for i=1:(repetitions + 1)]
    simulationresults[1]=["seed", "Rho", "perceivedmeanattitudes", "perceivedmaxattitudes", "perceivedminattitudes", "perceivedstdattitudes", "selfperceptions", "perceivedlocalAshmansDs","perceivedglobalAshmansDs"]

    ## The network generation is the most computationally intense task here. It is split in multiple Threads if supported by the Julia environment. (Check in Settings!)
    Threads.@threads for i in 1:repetitions
        Random.seed!(i)
        linksTHISRUN, linkspernodeTHISRUN = homophilic_linkage!(sorted_inputattitudes,Rho)
        visiblenodes = find_visible_nodes!(sorted_inputattitudes,linkspernodeTHISRUN)
        perceivedmeanattitudes,perceivedmaxattitudes,perceivedminattitudes, perceivedstdattitudes = distri_perceptions!(visiblenodes)
        selfperceptions = self_perceptions!(visiblenodes,sorted_inputattitudes)
        perceivedlocalAshmansDs, perceivedglobalAshmansDs = calculate_perceivedAshmansDs!(perceivedmeanattitudes, perceivedstdattitudes, mean(sorted_inputattitudes), std(sorted_inputattitudes))
        thisrun = []
        push!(thisrun, i, Rho, perceivedmeanattitudes, perceivedmaxattitudes, perceivedminattitudes, perceivedstdattitudes, selfperceptions, perceivedlocalAshmansDs, perceivedglobalAshmansDs)
        simulationresults[1 + i] = thisrun
    end
    simulationresultsnobrackets = []
    for i in simulationresults
        push!(simulationresultsnobrackets,i...)
    end
    return simulationresultsnobrackets
    #income = income_onegroup_lognormal!(population,sigma)
end


Rho0 = segregation_perceptions!(100,normalised_sorted_migattitudes,0)
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\migattitudes_Rho0.csv",Tables.table(Rho0))
Rho4 = segregation_perceptions!(100,normalised_sorted_migattitudes,4)
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\migattitudes_Rho4.csv",Tables.table(Rho4))
Rho8 = segregation_perceptions!(100,normalised_sorted_migattitudes,8)
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\migattitudes_Rho8.csv",Tables.table(Rho8))
Rho14 = segregation_perceptions!(100,normalised_sorted_migattitudes,14)
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\migattitudes_Rho14.csv",Tables.table(Rho14))
Rho50 = segregation_perceptions!(100,normalised_sorted_migattitudes,14)
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\migattitudes_Rho50.csv",Tables.table(Rho50))



## Polarisation perceptions - visualisation
# Convert to DataFrame
data = DataFrame(X=sorted_migattitudes, Y=perceivedDs)

# Bin X values into intervals of 0.5
bin_size = 0.5
data.X_binned = round.(data.X ./ bin_size) .* bin_size  # Round to nearest 0.5

# Create the plot
@df data violin(
    :X_binned, :Y,  # Grouped by binned categories
    xlabel="Own Attitude",
    ylabel="Perceived Ashman's D",
#    title="Box-Whisker Plot of Perceived Attitude Span by Binned Own Attitude",
    color="red",
    legend=false
)
@df data dotplot!(
    :X_binned, :Y,  # Grouped by binned categories
    xlabel="Own Attitude",
    ylabel="Perceived Ashman's D",
    color="purple",
    legend=false
)


# Calculate actual segregation => change
# mu1 and sigma1 => lower half (by attitude) of the population
# mu2 and sigma2 => upper half (by attitude) of the population

populationsize = round(length(sorted_migattitudes))
mu1obj = mean(sorted_migattitudes[1:populationsize ÷ 2])
sigma1obj = std(sorted_migattitudes[1:populationsize ÷ 2])
mu2obj = mean(sorted_migattitudes[populationsize ÷ 2 + 1:populationsize])
sigma2obj = std(sorted_migattitudes[populationsize ÷ 2 + 1:populationsize])

objAshmansD = abs(mu1obj - mu2obj) / sqrt(2 * (sigma1obj + sigma2obj))

function calculate_objAshmansD_randomGroups!(repetitions, sorted_inputattitudes)
    populationsize = round(length(sorted_inputattitudes))
    AshmansDlist = []
    for i in 1:repetitions
        Random.seed!(i)
        shuffled_inputattitudes = shuffle(sorted_inputattitudes)
        sizegroup1 = rand(2:(populationsize - 2)) # alternative: populationsize ÷ 2 for equal group sizes
        mu1obj = mean(shuffled_inputattitudes[1:sizegroup1])
        sigma1obj = std(shuffled_inputattitudes[1:sizegroup1])
        mu2obj = mean(shuffled_inputattitudes[sizegroup1 + 1:populationsize])
        sigma2obj = std(shuffled_inputattitudes[sizegroup1 + 1:populationsize])
        objAshmansD = abs(mu1obj - mu2obj) / sqrt(2 * (sigma1obj + sigma2obj))
        push!(AshmansDlist, objAshmansD)
    end
    return AshmansDlist
end

objrandomAshmansDs = calculate_objAshmansD_randomGroups!(10000, normalised_sorted_migattitudes)
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\objAshmansDs_random_pop-splits.csv",Tables.table(objrandomAshmansDs))

function calculate_objAshmansD_attitudeGroups!(sorted_inputattitudes)
    populationsize = round(length(sorted_inputattitudes))
    AshmansDlist = []
    for i in 2:(populationsize - 2)
        Random.seed!(i)
        sizegroup1 = i
        mu1obj = mean(sorted_inputattitudes[1:sizegroup1])
        sigma1obj = std(sorted_inputattitudes[1:sizegroup1])
        mu2obj = mean(sorted_inputattitudes[sizegroup1 + 1:populationsize])
        sigma2obj = std(sorted_inputattitudes[sizegroup1 + 1:populationsize])
        objAshmansD = abs(mu1obj - mu2obj) / sqrt(2 * (sigma1obj + sigma2obj))
        push!(AshmansDlist, objAshmansD)
    end
    return AshmansDlist
end

objattitudeAshmansDs = calculate_objAshmansD_attitudeGroups!(normalised_sorted_migattitudes)
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\Projekte-Jan\\Homophilic-Attitudes\\Simulation-Output\\objAshmansDs_attitude_pop-splits.csv",Tables.table(objattitudeAshmansDs))

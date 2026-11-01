## Perception of growth in income-homophilic networks
## Can an agent who reasons as an ideal, unbiased statistician, but only sees her ego network, tell whether incomes grow
## linearly or multiplicatively? Paired linear and multiplicative worlds, fixed income-homophilic networks, three growth
## regimes, multiplicative noise and four observation rules. Perceived inequality is recorded alongside.
## Definitions of every reported quantity: README_GrowthPerceptionModel.md.
##
## Run from this folder (the number is the number of repetitions; -t 8 runs 8 repetitions at the same time):
##   julia -t 8 --project="C:\research\Inequality-Perception" GrowthPerceptionModel.jl 5      (pilot)
##   julia -t 8 --project="C:\research\Inequality-Perception" GrowthPerceptionModel.jl 100    (full run)
##   julia -t 8 --project="C:\research\Inequality-Perception" GrowthPerceptionModel.jl 100 growth-regimes equal-weight-check
##                                                                                              (selected blocks only)

## Import modules and packages

using Random, Statistics, SpecialFunctions, CSV, DataFrames

include("InputIncomes.jl")
using .InputIncomes

include("GrowthNetworks.jl")
using .GrowthNetworks

include("GrowthWorlds.jl")
using .GrowthWorlds

include("GrowthPerceptions.jl")
using .GrowthPerceptions


## --------------------------------------------------------------------- Parameters ---------------------------------------------------------------------
## Every parameter of the experiment and the seed. The blocks below use these values; nothing else is set elsewhere.

const POPULATION = 1000                              ## agents
const PERIODS = 100                                  ## T
const DISTRIBUTION = "exponential"                   ## starting incomes: "exponential" or "lognormal"
const INITIALGINI = 0.5                              ## target Gini of the lognormal start (not used for "exponential")
const FIXEDPOPULATION = true                         ## true: the same starting population in every repetition
const PSIS = [0.005, 0.010, 0.020]                   ## psi, strength of the Matthew effect
const NOISES = [0.0, 0.01, 0.025, 0.05, 0.10]        ## sigma, volatility of the multiplicative shocks
const RHOS = [0.0, 4.0, 14.0, Inf]                   ## Rho, homophily (Inf = perfect homophily)
const GINITIMES = collect(0:10:100)                  ## periods at which perceived and actual inequality are recorded
const HORIZONS = collect(10:10:100)                  ## h, expanding observation windows: periods 0-10, 0-20, ..., 0-100
const AICTHRESHOLD = 2.0                             ## verdict band: linear if delta-AIC > 2, multiplicative if < -2
const COMPARISONMETHODS = false                      ## true: also run the two comparison classifiers
const SEED = 20_261_030                              ## population: SEED; networks: SEED + 1000 + j; shocks: SEED + 2000 + j

## The four blocks: name, contacts k, regimes, observation rules, time-resolved analysis on or off
const BLOCKS = [
    ## Is the rich-poor gap caused by who grows fastest? (expected: gone under "uniform", reversed under "convergent")
    ("growth-regimes", [5], ["matthew", "uniform", "convergent"], ["localmean"], true),
    ## Is it network size rather than homophily? (perfect homophily gives smaller ego networks)
    ("contacts", [3, 5, 10, 20], ["matthew"], ["localmean"], true),
    ## Does it depend on what agents observe? Mean income, median income, average growth rate or own income
    ("observers", [5], ["matthew"], ["localmean", "localmedian", "growthrates", "own"], false),
    ## Falsification check of the effective-sample-size explanation: with equal weights (average growth rate) every contact
    ## counts the same, so a gap caused by the income weights of the mean would disappear
    ("equal-weight-check", [5], ["matthew", "uniform", "convergent"], ["growthrates"], true),
]


## --------------------------------------------------------------------- Helper Functions ---------------------------------------------------------------------

## Starting distribution (sorted, mean one): "exponential", or "lognormal" with the target gini
function draw_incomes!(population, distribution, gini)
    if distribution == "exponential"
        return income_onegroup_exponential!(population)
    else
        return income_onegroup_lognormal!(population, 2 * erfinv(gini))
    end
end

## Initial-income decile of every person (Y is sorted, so the position is the income rank)
function income_deciles!(population)
    return [min(10, cld(10 * i, population)) for i in 1:population]
end

## Share of `true` among the given people
function share_of!(flags, people)
    return mean(flags[people])
end

## Adds one vector of values to the running sums of a summary table. Used to average the time-resolved results over
## repetitions without keeping every row in memory.
function accumulate!(table, key, values)
    if haskey(table, key)
        table[key] = table[key] .+ values
    else
        table[key] = Float64.(values)
    end
end

## Entropy (in bits) of a verdict distribution: 0 when everybody gives the same verdict, log2(3) = 1.58 when the verdicts
## are spread evenly. It measures disagreement among the people of a decile, not the confidence of one person
function verdict_entropy!(shares)
    entropy = 0.0
    for p in shares
        if p > 0
            entropy -= p * log2(p)
        end
    end
    return entropy
end

## Income homophily left in the fixed network at one period: correlation between a person's log income and the log of
## the mean income of her contacts (self excluded). The network is built on initial incomes, so noise erodes this.
function homophily_correlation!(incomes, linksPerNodeList)
    own = zeros(length(incomes))
    contactmeans = zeros(length(incomes))
    for i in 1:length(incomes)
        contacts = [j for j in linksPerNodeList[i] if j != i]
        own[i] = log(incomes[i])
        contactmeans[i] = log(mean(incomes[contacts]))
    end
    return cor(own, contactmeans)
end

## Squared weights with which the observation rule averages the members of every ego network, per person and period.
## Their sum is 1 / (effective number of people averaged), because the noise of a weighted mean of independent shocks scales
## with sqrt(sum of squared weights):
##   "localmean":   income weights, so the sum is (sum of squared incomes) / (sum of incomes)^2 of the ego network
##   "growthrates": equal weights, so the sum is 1 / ego size
##   "localmedian": 1 / (ego size * 2/pi), the usual large-sample efficiency of the median (an approximation)
##   "own":         1 (one person)
## Computed from the noise-free incomes of the person's own world.
function squared_weights!(noiselesshistory, linksPerNodeList, observer)
    population, timepoints = size(noiselesshistory)
    squaredweights = zeros(population, timepoints)
    for i in 1:population
        members = linksPerNodeList[i]
        for t in 1:timepoints
            if observer == "localmean"
                incomes = noiselesshistory[members, t]
                squaredweights[i, t] = sum(incomes .^ 2) / sum(incomes)^2
            elseif observer == "growthrates"
                squaredweights[i, t] = 1 / length(members)
            elseif observer == "localmedian"
                squaredweights[i, t] = 1 / (length(members) * 2 / pi)
            else
                squaredweights[i, t] = 1.0
            end
        end
    end
    return squaredweights
end

## TIME-RESOLVED PERCEPTION. Every person classifies the history she has observed up to each horizon (expanding windows).
## Adds to the summary tables of this repetition:
##   "time":     verdict shares, verdict entropy (disagreement among the agents of a decile, NOT the confidence of an agent),
##               growth-level error, signal-to-noise, effective number of people averaged and perceived Gini, by horizon and decile
##   "learning": time to statistical identification (first horizon from which the verdict stays correct), share
##               identified, ever wrong, verdict changes, by decile
##   "snr":      verdicts grouped by each person's own signal-to-noise ratio, by horizon (test of the SNR mechanism)
## The signal of a person is how far her noise-free observed history (in her own world) is from the wrong growth shape, measured
## in log-increments (separation_signal!). Each increment carries noise sigma, reduced by averaging over the EFFECTIVE number
## of people her observation rule averages (squared_weights!, mean over the window): SNR = signal * sqrt(neff) / sigma,
## so that SNR^2 is roughly the delta-AIC she can expect.
function time_resolved_perceptions!(localtables, condition, seenpaths, noiselesspaths, squaredweights, globalpath, history, linksPerNodeList, world, noise, horizons, decilemembers, aicthreshold)
    population = size(seenpaths, 2)
    verdicts = fill("", population, length(horizons))
    for (hindex, h) in enumerate(horizons)
        globalgrowth = globalpath[h + 1] / globalpath[1]
        perceivedginis = gini_perceptions_at!(history, linksPerNodeList, h + 1)
        levelerrors = zeros(population)
        log10snr = zeros(population)
        effectivesizes = zeros(population)
        for i in 1:population
            path = view(seenpaths, 1:(h + 1), i)
            verdicts[i, hindex] = classify_shape!(increments_deltaaic!(path), aicthreshold)
            levelerrors[i] = (path[end] / path[1]) / globalgrowth - 1
            signal = separation_signal!(view(noiselesspaths, 1:(h + 1), i), world)
            effectivesizes[i] = 1 / mean(view(squaredweights, i, 1:(h + 1)))
            if noise > 0
                log10snr[i] = log10(max(signal * sqrt(effectivesizes[i]) / noise, 1e-6))
            else
                log10snr[i] = Inf
            end
        end
        isright = verdicts[:, hindex] .== world
        iswrong = (verdicts[:, hindex] .!= world) .& (verdicts[:, hindex] .!= "indistinguishable")
        isundecided = verdicts[:, hindex] .== "indistinguishable"

        for decile in 1:10
            people = decilemembers[decile]
            shares = [mean(verdicts[people, hindex] .== "linear"), mean(verdicts[people, hindex] .== "multiplicative"), mean(isundecided[people])]
            accumulate!(localtables["time"], (condition..., h, decile),
                [shares..., mean(isright[people]), verdict_entropy!(shares), mean(levelerrors[people]), mean(log10snr[people]),
                 mean(effectivesizes[people]), mean(perceivedginis[people])])
        end
        ## Verdicts by own signal-to-noise ratio, in bins of 0.25 on the log10 scale (noise-free conditions have no finite SNR)
        if noise > 0
            for i in 1:population
                binlower = -1.5 + 0.25 * clamp(floor((log10snr[i] + 1.5) / 0.25), 0, 18)
                accumulate!(localtables["snr"], (condition..., h, binlower), [1, isright[i], iswrong[i], isundecided[i], log10snr[i]])
            end
        end
    end

    ## Learning measures over the horizons
    otherworld = world == "linear" ? "multiplicative" : "linear"
    for decile in 1:10
        identified = 0
        sumtime = 0.0
        everwrong = 0
        changes = 0
        wrongthenright = 0
        for i in decilemembers[decile]
            correct = verdicts[i, :] .== world
            wrong = verdicts[i, :] .== otherworld
            if correct[end]
                ## Time to identification: the first horizon after which the verdict stays correct
                lastnotcorrect = findlast(.!correct)
                firststable = lastnotcorrect === nothing ? 1 : lastnotcorrect + 1
                identified += 1
                sumtime += horizons[firststable]
                if any(wrong)
                    wrongthenright += 1
                end
            end
            if any(wrong)
                everwrong += 1
            end
            for k in 2:length(horizons)
                if verdicts[i, k] != verdicts[i, k - 1]
                    changes += 1
                end
            end
        end
        accumulate!(localtables["learning"], (condition..., decile),
            [length(decilemembers[decile]), identified, sumtime, everwrong, changes, wrongthenright])
    end
end


## --------------------------------------------------------------------- One repetition ---------------------------------------------------------------------

## Everything that happens within repetition j. Returns four lists of rows (global, perception, rank, gini).
## If timeresolved, the time-resolved summaries of this repetition are added to the shared summarytables (under the lock).
function simulate_repetition!(j, Y, periods, distribution, gini, psilist, noiselist, Rholist, contactslist, regimelist, observerlist, ginitimes, horizons, timeresolved, aicthreshold, comparisonmethods, seedoffset, summarytables, summarylock)
    population = length(Y)
    globalrows = []
    perceptionrows = []
    rankrows = []
    ginirows = []
    localtables = Dict("time" => Dict(), "learning" => Dict(), "snr" => Dict(), "homophily" => Dict())
    horizons = sort(unique(filter(h -> 2 <= h <= periods, horizons)))
    deciles = income_deciles!(population)
    decilemembers = [findall(==(decile), deciles) for decile in 1:10]
    recordtimes = sort(unique(filter(t -> t <= periods, ginitimes)))
    worldnames = ["linear", "multiplicative"]

    ## One network per (contacts, Rho). The seed depends on the repetition only, so a network never depends on the
    ## position of Rho or contacts in their lists.
    networks = Dict()
    for contacts in contactslist
        for Rho in Rholist
            Random.seed!(seedoffset + 1000 + j)
            networks[(contacts, Rho)] = homophilic_linkage!(Y, Rho, contacts)
        end
    end
    ## One set of shocks per repetition, shared by every condition (common random numbers)
    Random.seed!(seedoffset + 2000 + j)
    innovations = randn(population, periods)

    for regime in regimelist
        for psi in psilist
            growthrates = regime_growth_rates!(Y, psi, regime)
            linear0, multiplicative0 = paired_growth_histories!(Y, growthrates, periods)
            ## Curvature of the deterministic global multiplicative path, divided by noise: a population-level descriptor kept in the
## global and perception tables. The agent-level signal-to-noise ratio is computed in time_resolved_perceptions!.
            signalconvexity = midpoint_convexity!(vec(mean(multiplicative0, dims=1)))
            ## Noise-free observed histories in both worlds (one column per person): each person's own signal for the SNR test
            noiselesspaths = Dict()
            if timeresolved
                for contacts in contactslist
                    for Rho in Rholist
                        for observer in observerlist
                            noiselesspaths[(contacts, Rho, observer, "linear")] = permutedims(observed_histories!(linear0, networks[(contacts, Rho)], observer))
                            noiselesspaths[(contacts, Rho, observer, "multiplicative")] = permutedims(observed_histories!(multiplicative0, networks[(contacts, Rho)], observer))
                        end
                    end
                end
            end

            for noise in noiselist
                linear, multiplicative = apply_stochasticity!(linear0, multiplicative0, noise, innovations)
                worlds = Dict("linear" => linear, "multiplicative" => multiplicative)
                curvaturenoiseratio = noise == 0 ? Inf : signalconvexity / (noise * sqrt(periods / 2))

                ## Population-level benchmark: is the growth type identifiable from the whole population?
                globalpaths = Dict(world => vec(mean(worlds[world], dims=1)) for world in worldnames)
                globaldistance = sqrt(mean((globalpaths["multiplicative"] ./ globalpaths["multiplicative"][1] .-
                                            globalpaths["linear"] ./ globalpaths["linear"][1]) .^ 2))
                globalshapes = Dict()
                for world in worldnames
                    path = globalpaths[world]
                    globaldeltaaic = increments_deltaaic!(path)
                    globalshapes[world] = classify_shape!(globaldeltaaic, aicthreshold)
                    push!(globalrows, [j, distribution, gini, regime, psi, noise, world, curvaturenoiseratio,
                        globalshapes[world], globaldeltaaic, path[end] / path[1], midpoint_convexity!(path), globaldistance])
                end

                for contacts in contactslist
                    for Rho in Rholist
                        linksPerNodeList = networks[(contacts, Rho)]
                        egosizes = egosizes!(linksPerNodeList)

                        ## Perceived inequality: independent of the observation rule, so once per network and world
                        ginistart = Dict()
                        giniend = Dict()
                        for world in worldnames
                            history = worlds[world]
                            ginistart[world] = gini_perceptions_at!(history, linksPerNodeList, 1)
                            giniend[world] = gini_perceptions_at!(history, linksPerNodeList, periods + 1)
                            for t in recordtimes
                                perceivedginis = gini_perceptions_at!(history, linksPerNodeList, t + 1)
                                globalgini = real_gini!(history[:, t + 1])
                                push!(ginirows, [j, distribution, gini, regime, psi, noise, contacts, Rho, world, t, globalgini,
                                    mean(perceivedginis), mean(perceivedginis) - globalgini, mean(abs.(perceivedginis .- globalgini))])
                            end
                            ## Homophily erosion: how much income homophily is left in the fixed network at every horizon
                            if timeresolved
                                for h in vcat(0, horizons)
                                    accumulate!(localtables["homophily"], (distribution, gini, regime, psi, noise, contacts, Rho, world, h),
                                        [homophily_correlation!(history[:, h + 1], linksPerNodeList)])
                                end
                            end
                        end

                        for observer in observerlist
                            observed = Dict(world => observed_histories!(worlds[world], linksPerNodeList, observer) for world in worldnames)
                            localdistance = growth_distance!(observed["multiplicative"], observed["linear"])
                            compression = globaldistance == 0 ? zeros(population) : localdistance ./ globaldistance

                            for world in worldnames
                                ## Each column of seenpaths is the history one person observes
                                seenpaths = permutedims(observed[world])
                                deltaaics = zeros(population)
                                shapes = fill("", population)
                                growthfactors = zeros(population)
                                convexities = zeros(population)
                                for i in 1:population
                                    path = view(seenpaths, :, i)
                                    deltaaics[i] = increments_deltaaic!(path)
                                    shapes[i] = classify_shape!(deltaaics[i], aicthreshold)
                                    growthfactors[i] = path[end] / path[1]
                                    convexities[i] = midpoint_convexity!(path)
                                end
                                islinear = shapes .== "linear"
                                ismultiplicative = shapes .== "multiplicative"
                                isindistinguishable = shapes .== "indistinguishable"
                                ginirising = giniend[world] .> ginistart[world]
                                globalpath = globalpaths[world]
                                globalgrowthfactor = globalpath[end] / globalpath[1]
                                relativegrowtherrors = growthfactors ./ globalgrowthfactor .- 1

                                ## Optional comparison classifiers (slow; for checks only)
                                comparison = [missing, missing, missing, missing]
                                if comparisonmethods
                                    nlsshapes = [classify_shape!(nls_deltaaic!(view(seenpaths, :, i)), aicthreshold) for i in 1:population]
                                    logolsshapes = [classify_shape!(logols_deltaaic!(view(seenpaths, :, i)), aicthreshold) for i in 1:population]
                                    comparison = [mean(nlsshapes .== "linear"), mean(nlsshapes .== "multiplicative"),
                                                  mean(logolsshapes .== "linear"), mean(logolsshapes .== "multiplicative")]
                                end

                                condition = [j, distribution, gini, regime, psi, noise, contacts, Rho, observer, world]
                                thisrun = []
                                push!(thisrun, condition...)
                                push!(thisrun, mean(egosizes), curvaturenoiseratio, globalshapes[world], globalshapes[world] == world,
                                    mean(islinear), mean(ismultiplicative), mean(isindistinguishable), mean(deltaaics), median(deltaaics),
                                    ## Level of growth: perceived total growth factor against the population's
                                    mean(growthfactors), globalgrowthfactor, mean(relativegrowtherrors), mean(abs.(growthfactors .- globalgrowthfactor)),
                                    midpoint_convexity!(globalpath), mean(convexities), mean(compression),
                                    ## Joint perception: objective and perceived inequality at start and end
                                    real_gini!(worlds[world][:, 1]), real_gini!(worlds[world][:, end]),
                                    mean(ginistart[world]), mean(giniend[world]), mean(ginirising))
                                push!(thisrun, comparison...)
                                push!(perceptionrows, thisrun)

                                for decile in 1:10
                                    people = decilemembers[decile]
                                    thisdecile = []
                                    push!(thisdecile, condition...)
                                    push!(thisdecile, decile, mean(egosizes[people]),
                                        share_of!(islinear, people), share_of!(ismultiplicative, people), share_of!(isindistinguishable, people),
                                        mean(deltaaics[people]), mean(growthfactors[people]), mean(relativegrowtherrors[people]),
                                        mean(convexities[people]), mean(compression[people]),
                                        mean(ginistart[world][people]), mean(giniend[world][people]), share_of!(ginirising, people),
                                        ## Joint verdicts: growth shape and whether perceived inequality rose (they sum to the rising share)
                                        share_of!(ismultiplicative .& ginirising, people), share_of!(islinear .& ginirising, people),
                                        share_of!(isindistinguishable .& ginirising, people))
                                    push!(rankrows, thisdecile)
                                end

                                if timeresolved
                                    noiselesshistory = world == "linear" ? linear0 : multiplicative0
                                    squaredweights = squared_weights!(noiselesshistory, linksPerNodeList, observer)
                                    time_resolved_perceptions!(localtables, (distribution, gini, regime, psi, noise, contacts, Rho, observer, world),
                                        seenpaths, noiselesspaths[(contacts, Rho, observer, world)], squaredweights, globalpath, worlds[world],
                                        linksPerNodeList, world, noise, horizons, decilemembers, aicthreshold)
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    ## Add this repetition's time-resolved summaries to the shared tables (one thread at a time)
    if timeresolved
        lock(summarylock) do
            for name in keys(localtables)
                for (key, values) in localtables[name]
                    accumulate!(summarytables[name], key, values)
                end
            end
        end
    end
    return globalrows, perceptionrows, rankrows, ginirows
end


## --------------------------------------------------------------------- Model ---------------------------------------------------------------------

## repetitions, population, periods, distribution ("exponential" or "lognormal"), gini (lognormal only),
## fixedpopulation (true: one starting distribution for all repetitions), psilist (Matthew strength), noiselist (GBM volatility),
## Rholist (homophily; Inf = perfect homophily), contactslist (contacts chosen per person),
## regimelist ("matthew", "uniform", "convergent"), observerlist ("localmean", "localmedian", "growthrates", "own"),
## ginitimes (periods at which inequality is recorded), horizons (expanding observation windows for the time-resolved analysis),
## timeresolved (true: also run the time-resolved analysis), aicthreshold, comparisonmethods (also the level-based classifiers), seedoffset.
## Returns the global, perception, rank and gini lists, followed by the time-resolved summaries (time, learning, snr, homophily),
## which are averaged over repetitions (empty when timeresolved is false).
function growth_perceptions_paired!(repetitions, population, periods, distribution, gini, fixedpopulation, psilist, noiselist, Rholist, contactslist, regimelist, observerlist, ginitimes, horizons, timeresolved, aicthreshold, comparisonmethods, seedoffset)
    globalresults = []
    push!(globalresults, ["repetition", "distribution", "initialgini", "regime", "psi", "noise", "world", "curvaturenoiseratio",
        "globalshape", "globaldeltaaic", "globalgrowthfactor", "globalconvexity", "globaldistance"])
    conditionheader = ["repetition", "distribution", "initialgini", "regime", "psi", "noise", "contacts", "Rho", "observer", "world"]
    perceptionresults = []
    push!(perceptionresults, vcat(conditionheader, ["meanegosize", "curvaturenoiseratio", "globalshape", "globallyidentifiable",
        "share_linear", "share_multiplicative", "share_indistinguishable", "meandeltaaic", "mediandeltaaic",
        "meanperceivedgrowthfactor", "globalgrowthfactor", "meanrelativegrowtherror", "meanabsolutegrowtherror",
        "globalconvexity", "meanlocalconvexity", "meancompressionratio",
        "globalgini_start", "globalgini_end", "meanperceivedgini_start", "meanperceivedgini_end", "share_perceivedginirising",
        "share_linear_nls", "share_multiplicative_nls", "share_linear_logols", "share_multiplicative_logols"]))
    rankresults = []
    push!(rankresults, vcat(conditionheader, ["decile", "meanegosize", "share_linear", "share_multiplicative", "share_indistinguishable",
        "meandeltaaic", "meanperceivedgrowthfactor", "meanrelativegrowtherror", "meanlocalconvexity", "meancompressionratio",
        "meanperceivedgini_start", "meanperceivedgini_end", "share_perceivedginirising",
        "share_multiplicative_and_ginirising", "share_linear_and_ginirising", "share_indistinguishable_and_ginirising"]))
    giniresults = []
    push!(giniresults, ["repetition", "distribution", "initialgini", "regime", "psi", "noise", "contacts", "Rho", "world", "time",
        "globalgini", "meanperceivedgini", "meanginierror", "meanabsoluteginierror"])

    if fixedpopulation
        Random.seed!(seedoffset)
        fixedY = draw_incomes!(population, distribution, gini)
    end

    ## The repetitions are split over the available threads (start Julia with -t 8). Every repetition sets its own seeds,
    ## so the results do not depend on the number of threads.
    repetitionresults = Vector{Any}(undef, repetitions)
    summarytables = Dict("time" => Dict(), "learning" => Dict(), "snr" => Dict(), "homophily" => Dict())
    u = ReentrantLock()
    Threads.@threads for j in 1:repetitions
        if fixedpopulation
            Y = fixedY
        else
            Random.seed!(seedoffset + j)
            Y = draw_incomes!(population, distribution, gini)
        end
        repetitionresults[j] = simulate_repetition!(j, Y, periods, distribution, gini, psilist, noiselist, Rholist, contactslist,
            regimelist, observerlist, ginitimes, horizons, timeresolved, aicthreshold, comparisonmethods, seedoffset, summarytables, u)
    end
    for j in 1:repetitions
        append!(globalresults, repetitionresults[j][1])
        append!(perceptionresults, repetitionresults[j][2])
        append!(rankresults, repetitionresults[j][3])
        append!(giniresults, repetitionresults[j][4])
    end
    timeresults, learningresults, snrresults, homophilyresults = summarise_time_tables!(summarytables, repetitions)
    return globalresults, perceptionresults, rankresults, giniresults, timeresults, learningresults, snrresults, homophilyresults
end

## Rounds the numbers of a row to 10 significant digits. The time-resolved sums are added in the order in which the
## threads finish, which changes the last digits (around 1e-14); rounding makes reruns give identical files.
function round_row!(row)
    return [x isa AbstractFloat && isfinite(x) ? round(x, sigdigits=10) : x for x in row]
end

## Turns the summed time-resolved tables into lists of rows with a header (means over repetitions)
function summarise_time_tables!(summarytables, repetitions)
    conditionheader = ["distribution", "initialgini", "regime", "psi", "noise", "contacts", "Rho", "observer", "world"]

    timeresults = []
    push!(timeresults, vcat(conditionheader, ["horizon", "decile", "share_linear", "share_multiplicative", "share_indistinguishable",
        "share_right", "verdictentropy", "meanlevelerror", "meanlog10snr", "meaneffectivesize", "meanperceivedgini"]))
    for key in sort(collect(keys(summarytables["time"])))
        push!(timeresults, round_row!(vcat(collect(key), summarytables["time"][key] ./ repetitions)))
    end

    learningresults = []
    push!(learningresults, vcat(conditionheader, ["decile", "share_identified", "meantimetoidentification", "share_everwrong",
        "meanverdictchanges", "share_wrongthenright"]))
    for key in sort(collect(keys(summarytables["learning"])))
        agents, identified, sumtime, everwrong, changes, wrongthenright = summarytables["learning"][key]
        meantime = identified > 0 ? sumtime / identified : missing
        push!(learningresults, vcat(collect(key), [identified / agents, meantime, everwrong / agents, changes / agents, wrongthenright / agents]))
    end

    snrresults = []
    push!(snrresults, vcat(conditionheader, ["horizon", "log10snr_lower", "agents", "share_right", "share_wrong", "share_indistinguishable", "meanlog10snr"]))
    for key in sort(collect(keys(summarytables["snr"])))
        agents, right, wrong, undecided, sumlog10snr = summarytables["snr"][key]
        push!(snrresults, round_row!(vcat(collect(key), [agents, right / agents, wrong / agents, undecided / agents, sumlog10snr / agents])))
    end

    homophilyresults = []
    push!(homophilyresults, ["distribution", "initialgini", "regime", "psi", "noise", "contacts", "Rho", "world", "horizon", "incomecorrelation"])
    for key in sort(collect(keys(summarytables["homophily"])))
        push!(homophilyresults, round_row!(vcat(collect(key), summarytables["homophily"][key] ./ repetitions)))
    end
    return timeresults, learningresults, snrresults, homophilyresults
end


## --------------------------------------------------------------------- Output ---------------------------------------------------------------------

## Turns a list of rows whose first entry is the header into a DataFrame with one typed column per header entry
function resultlist_to_dataframe!(resultlist)
    header = resultlist[1]
    rows = resultlist[2:end]
    return DataFrame([Symbol(header[k]) => identity.([row[k] for row in rows]) for k in 1:length(header)])
end

## Averages over repetitions, per condition
function summarise_perceptions!(perceptiondf)
    conditions = [:distribution, :initialgini, :regime, :psi, :noise, :contacts, :Rho, :observer, :world]
    return combine(groupby(perceptiondf, conditions),
        :globallyidentifiable => mean => :share_globallyidentifiable,
        :meanegosize => mean => :meanegosize,
        :share_linear => mean => :share_linear,
        :share_linear => (x -> quantile(x, 0.05)) => :p05_share_linear,
        :share_linear => (x -> quantile(x, 0.95)) => :p95_share_linear,
        :share_multiplicative => mean => :share_multiplicative,
        :share_indistinguishable => mean => :share_indistinguishable,
        :curvaturenoiseratio => first => :curvaturenoiseratio,
        :meandeltaaic => mean => :meandeltaaic,
        :meanperceivedgrowthfactor => mean => :meanperceivedgrowthfactor,
        :globalgrowthfactor => mean => :globalgrowthfactor,
        :meanrelativegrowtherror => mean => :meanrelativegrowtherror,
        :meanlocalconvexity => mean => :meanlocalconvexity,
        :meancompressionratio => mean => :meancompressionratio,
        :globalgini_end => mean => :globalgini_end,
        :meanperceivedgini_end => mean => :meanperceivedgini_end,
        :share_perceivedginirising => mean => :share_perceivedginirising)
end

## Averages perceived and actual inequality over repetitions, per condition and period
function summarise_ginis!(ginidf)
    conditions = [:distribution, :initialgini, :regime, :psi, :noise, :contacts, :Rho, :world, :time]
    return combine(groupby(ginidf, conditions),
        :globalgini => mean => :globalgini,
        :meanperceivedgini => mean => :meanperceivedgini,
        :meanginierror => mean => :meanginierror,
        :meanabsoluteginierror => mean => :meanabsoluteginierror)
end

## Writes the four result lists, the two summaries and (if present) the time-resolved summaries to one folder
function save_growth_perceptions!(outputdirectory, globalresults, perceptionresults, rankresults, giniresults, timeresults, learningresults, snrresults, homophilyresults)
    mkpath(outputdirectory)
    timetables = [("growth-shape-time.csv", timeresults), ("growth-shape-learning.csv", learningresults),
                  ("growth-shape-snr.csv", snrresults), ("growth-shape-homophily.csv", homophilyresults)]
    for (filename, resultlist) in timetables
        if length(resultlist) > 1
            CSV.write(joinpath(outputdirectory, filename), resultlist_to_dataframe!(resultlist))
        end
    end
    perceptiondf = resultlist_to_dataframe!(perceptionresults)
    ginidf = resultlist_to_dataframe!(giniresults)
    CSV.write(joinpath(outputdirectory, "growth-shape-global.csv"), resultlist_to_dataframe!(globalresults))
    CSV.write(joinpath(outputdirectory, "growth-shape-perception.csv"), perceptiondf)
    CSV.write(joinpath(outputdirectory, "growth-shape-rank.csv"), resultlist_to_dataframe!(rankresults))
    CSV.write(joinpath(outputdirectory, "growth-shape-gini.csv"), ginidf)
    CSV.write(joinpath(outputdirectory, "growth-shape-summary.csv"), summarise_perceptions!(perceptiondf))
    CSV.write(joinpath(outputdirectory, "growth-shape-gini-summary.csv"), summarise_ginis!(ginidf))
    return outputdirectory
end


## --------------------------------------------------------------------- RUN THE MODEL ---------------------------------------------------------------------
## Runs the blocks of BLOCKS with the parameters above. Arguments: the number of repetitions, optionally followed by
## the names of the blocks to run, e.g.
##   julia -t 8 --project=... GrowthPerceptionModel.jl 100 growth-regimes equal-weight-check
## Without block names, all blocks run. Fewer than 100 repetitions write to output/growth-perception-pilot/<block>/,
## otherwise to output/growth-perception-full/<block>/.

if abspath(PROGRAM_FILE) == @__FILE__
    repetitions = isempty(ARGS) ? 100 : parse(Int, ARGS[1])
    runlabel = repetitions < 100 ? "pilot" : "full"
    selectedblocks = length(ARGS) > 1 ? ARGS[2:end] : [block[1] for block in BLOCKS]

    println("Running ", repetitions, " repetitions on ", Threads.nthreads(), " threads: ", join(selectedblocks, ", "))
    for (name, contacts, regimes, observers, timeresolved) in BLOCKS
        if !(name in selectedblocks)
            continue
        end
        starttime = time()
        results = growth_perceptions_paired!(repetitions, POPULATION, PERIODS, DISTRIBUTION, INITIALGINI, FIXEDPOPULATION,
            PSIS, NOISES, RHOS, contacts, regimes, observers, GINITIMES, HORIZONS, timeresolved, AICTHRESHOLD,
            COMPARISONMETHODS, SEED)
        elapsed = time() - starttime
        outputdirectory = joinpath(@__DIR__, "output", "growth-perception-" * runlabel, name)
        save_growth_perceptions!(outputdirectory, results...)
        println(rpad(name, 20), round(elapsed / 60, digits=1), " min  ->  ", outputdirectory)
    end
end

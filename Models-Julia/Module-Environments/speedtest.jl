# Lognormal network speedtest -- run this file from top to bottom.
#
# GETTING STARTED
# 1. Install Julia if necessary: https://julialang.org/downloads/
# 2. Open this file in Julia's VS Code extension and execute each numbered
#    section in order, or open a terminal in Models-Julia/Module-Environments
#    and run:
#        julia --startup-file=no speedtest.jl
#    For a small installation/output check first, run:
#        julia --startup-file=no speedtest.jl --smoke-test
#    For four timing measurements at the full population size, run:
#        julia --startup-file=no speedtest.jl --pilot
#    For one MC run at EVERY Gini/rho combination, run:
#        julia --startup-file=no speedtest.jl --extended-pilot
#    When stepping through the file, choose input_run_mode below instead.
# Julia itself must already be installed; section 2 installs all Julia packages.
# The first execution needs internet access and can take several minutes.
# Keep this file beside InputDistributions.jl and HomophilicNetworks.jl in the
# repository. It loads those modules and installs the repository's packages;
# no prior Julia session setup is needed. Module paths are relative to this file;
# results go to the external output directory configured below.

## 1. Choose the inputs. These are the requested full-sweep defaults.
input_ginilist = collect(0:90) ./ 100    # 0.00, 0.01, ..., 0.90
input_Rholist = collect(0:25)           # 0, 1, ..., 25
input_population = 10_000
input_repetitions = 100                # MC runs per parameter combination
input_base_seed = 20261120
# Results are saved outside the repository, under your user home directory.
# On this Windows account: C:/Users/dmaye/Inequality-Perception-output/speedtest
# Change this input to another absolute path if desired. The runner creates it.
input_working_directory = joinpath(homedir(), "Inequality-Perception-output", "speedtest")
input_run_mode = "full"                # "full", "pilot", "extended-pilot", "smoke-test"

# Optional: save every agent's income and degree, too. The default saves timing
# measurements and scalar simulation results. Full agent data would contain
# 2,366,000,000 rows, so enable this only if those data are needed.
input_save_agent_data = false

# The full sweep contains 91 * 26 * 100 = 236,600 runs. The extended pilot uses
# the same ginilist, Rholist and population, but only one MC run per combination:
# 91 * 26 = 2,366 runs. Earlier timings suggest about six hours on this machine.
# Each network allocates an approximately 800 MB weight matrix plus other
# intermediate objects. Runs are sequential to avoid multiplying that memory
# requirement. Times include garbage collection and are elapsed, not CPU, seconds.

## 2. Install the repository's packages and load the existing model modules.
import Pkg
speedtest_environment = normpath(joinpath(@__DIR__, "..", ".."))
Pkg.activate(speedtest_environment)
Pkg.instantiate()

using CSV
using Dates, Random, Statistics, Printf, TOML

include(joinpath(@__DIR__, "InputDistributions.jl"))
include(joinpath(@__DIR__, "HomophilicNetworks.jl"))

## 3. Time the existing model functions. Execute this entire section together.
# All model calculations come from the modules above. This wrapper only seeds
# the simulation, measures the function calls and collects their results.

function speedtest_simulation(population, sigma, Rho, seed)
    Random.seed!(seed)
    income_stats = @timed InputDistributions.income_onegroup_lognormal!(population, sigma)
    Y = income_stats.value
    network_stats = @timed HomophilicNetworks.homophilic_linkage!(Y, Rho)
    linksTHISRUN, linkspernodeTHISRUN = network_stats.value
    degree_stats = @timed HomophilicNetworks.calculate_degree_distribution!(linkspernodeTHISRUN)
    degreedistri = degree_stats.value
    # Preserve the module's Gini convention. Currently real_gini! is the
    # conventional sample Gini + 1/population (also for equal incomes).
    sample_gini = InputDistributions.real_gini!(Y)
    measurements = (
        income_seconds=income_stats.time,
        network_seconds=network_stats.time,
        degree_seconds=degree_stats.time,
        total_seconds=income_stats.time + network_stats.time + degree_stats.time,
        allocated_bytes=income_stats.bytes + network_stats.bytes + degree_stats.bytes,
        gc_seconds=income_stats.gctime + network_stats.gctime + degree_stats.gctime,
        sample_gini=sample_gini,
        edge_count=length(linksTHISRUN),
        mean_degree=mean(degreedistri),
        max_degree=maximum(degreedistri),
    )
    return measurements, Y, degreedistri
end

## 4. Define the runner and output files. Execute this entire section together.
# Each invocation makes a new folder, so previous results are never overwritten.
# timings.csv: one row per MC run, appended after each completed combination.
# summary.csv: mean/median/min/max/std timings for each completed combination.
# overview.csv: statistics across all measured runs, including stage timings,
# allocation, garbage collection, throughput and total sweep wall time.
# overview.txt: the same human-readable overview printed to the console.
# settings.toml: parameter grid, hardware, Julia version and timing definitions.
# Project.toml / Manifest.toml: exact package environment used for this run.
# agents_*.csv (optional): one row per agent per repetition, income and degree.
# Completion is recorded in completed.toml only when the entire sweep finishes.
# If stopped, CSV files retain all previously completed parameter combinations.

function run_speedtest(; ginilist=input_ginilist, Rholist=input_Rholist,
        population=input_population, repetitions=input_repetitions,
        base_seed=input_base_seed, working_directory=input_working_directory,
        save_agent_data=input_save_agent_data)
    @assert population >= 6 "At least six agents are required for five neighbours."
    @assert repetitions >= 1 "At least one MC repetition is required."
    @assert !isempty(ginilist) && all(g -> isfinite(g) && 0 <= g < 1, ginilist)
    @assert !isempty(Rholist) && all(r -> isfinite(r) && r >= 0, Rholist)
    mkpath(working_directory)
    folder = mktempdir(working_directory; prefix=Dates.format(now(), "yyyymmdd_HHMMSS") * "_", cleanup=false)
    sigmalist = InputDistributions.ginitosigma!(ginilist)
    total_runs = length(ginilist) * length(Rholist) * repetitions
    settings = Dict(
        "started_at" => string(now()), "julia_version" => string(VERSION),
        "cpu" => Sys.CPU_NAME, "os" => string(Sys.KERNEL),
        "julia_threads" => Threads.nthreads(), "execution" => "sequential",
        "population" => population, "mc_runs" => repetitions, "total_runs" => total_runs,
        "ginilist" => collect(ginilist), "sigmalist" => sigmalist, "Rholist" => collect(Rholist),
        "base_seed" => base_seed, "save_agent_data" => save_agent_data,
        "seed_policy" => "seed = base_seed + MC repetition; common random numbers across parameter combinations",
        "timing" => "income generation + network generation + degrees; includes GC; excludes setup, warm-up, seeding, scalar summaries and file writing",
        "allocation" => "allocated_bytes is cumulative allocation, not peak memory",
        "gini" => "input is theoretical lognormal Gini; sample_gini is InputDistributions.real_gini!(Y), currently conventional sample Gini + 1/population",
    )
    open(joinpath(folder, "settings.toml"), "w") do io
        TOML.print(io, settings)
    end
    for filename in ("Project.toml", "Manifest.toml")
        cp(joinpath(speedtest_environment, filename), joinpath(folder, filename))
    end
    println("Results: ", folder)
    @printf("Population: %d | Ginis: %d | rho values: %d | MC runs per combination: %d\n",
        population, length(ginilist), length(Rholist), repetitions)
    println("Warming up Julia before measuring ", total_runs, " simulations...")
    # Warm up the module functions, including zero sigma and the upper-tail
    # weight-underflow fallback, before taking any measurements.
    speedtest_simulation(30, 0.0, 0, base_seed)
    speedtest_simulation(30, only(InputDistributions.ginitosigma!([0.9])), 25, base_seed)
    HomophilicNetworks.homophilic_linkage!(vcat(ones(29), 1000.0), 25)
    GC.gc()
    sweep_start = time()
    completed = 0
    first_combination = true
    all_seconds = Float64[]
    summaries = NamedTuple[]
    for (gini_index, gini) in enumerate(ginilist), Rho in Rholist
        sigma = sigmalist[gini_index]
        rows = NamedTuple[]
        combo_start = time()
        agent_path = joinpath(folder, @sprintf("agents_g%03d_rho%02d.csv", gini_index, Rho))
        for repetition in 1:repetitions
            seed = base_seed + repetition
            measurements, Y, degreedistri = speedtest_simulation(population, sigma, Rho, seed)
            push!(rows, merge((gini=gini, sigma=sigma, rho=Rho, mc_run=repetition,
                seed=seed, population=population), measurements))
            if save_agent_data
                CSV.write(agent_path, (gini=fill(gini, population), sigma=fill(sigma, population),
                    rho=fill(Rho, population), mc_run=fill(repetition, population), seed=fill(seed, population),
                    income_rank=collect(1:population), income=Y, degree=Int.(degreedistri));
                    append=repetition > 1)
            end
            if repetition == 1 || repetition % 10 == 0
                @printf("Gini %.2f | rho %2d | MC %3d/%d | simulation %.3f s\n",
                    gini, Rho, repetition, repetitions, measurements.total_seconds)
                flush(stdout)
            end
        end
        CSV.write(joinpath(folder, "timings.csv"), rows; append=!first_combination)
        seconds = [row.total_seconds for row in rows]
        summary = (gini=gini, sigma=sigma, rho=Rho, population=population, mc_runs=repetitions,
            mean_seconds=mean(seconds), median_seconds=median(seconds),
            min_seconds=minimum(seconds), max_seconds=maximum(seconds),
            std_seconds=repetitions > 1 ? std(seconds) : 0.0,
            mean_income_seconds=mean(row.income_seconds for row in rows),
            mean_network_seconds=mean(row.network_seconds for row in rows),
            mean_degree_seconds=mean(row.degree_seconds for row in rows),
            mean_allocated_bytes=mean(row.allocated_bytes for row in rows),
            mean_gc_seconds=mean(row.gc_seconds for row in rows),
            mean_sample_gini=mean(row.sample_gini for row in rows),
            mean_degree=mean(row.mean_degree for row in rows),
            combination_wall_seconds=time() - combo_start)
        CSV.write(joinpath(folder, "summary.csv"), [summary]; append=!first_combination)
        append!(all_seconds, seconds)
        push!(summaries, summary)
        first_combination = false
        completed += repetitions
        elapsed = time() - sweep_start
        remaining_hours = elapsed / completed * (total_runs - completed) / 3600
        @printf("Saved %d/%d runs. Elapsed %.2f h; rough remaining estimate %.2f h.\n",
            completed, total_runs, elapsed / 3600, remaining_hours)
        flush(stdout)
    end
    sweep_wall_seconds = time() - sweep_start
    overview = (
        population=population, parameter_combinations=length(summaries),
        mc_runs_per_combination=repetitions, completed_runs=completed,
        mean_seconds=mean(all_seconds), median_seconds=median(all_seconds),
        min_seconds=minimum(all_seconds), max_seconds=maximum(all_seconds),
        std_seconds=completed > 1 ? std(all_seconds) : 0.0,
        mean_income_seconds=mean(row.mean_income_seconds for row in summaries),
        mean_network_seconds=mean(row.mean_network_seconds for row in summaries),
        mean_degree_seconds=mean(row.mean_degree_seconds for row in summaries),
        mean_allocated_bytes=mean(row.mean_allocated_bytes for row in summaries),
        mean_gc_seconds=mean(row.mean_gc_seconds for row in summaries),
        total_simulation_seconds=sum(all_seconds), sweep_wall_seconds=sweep_wall_seconds,
        runs_per_second=completed / sweep_wall_seconds,
    )
    CSV.write(joinpath(folder, "overview.csv"), [overview])
    overview_text = sprint() do io
        println(io, "\nSpeedtest overview")
        @printf(io, "Population: %d | Parameter combinations: %d | MC runs per combination: %d\n",
            population, length(summaries), repetitions)
        @printf(io, "Completed simulations: %d\n", completed)
        @printf(io, "Simulation seconds: mean %.6f | median %.6f | min %.6f | max %.6f | std %.6f\n",
            overview.mean_seconds, overview.median_seconds, overview.min_seconds,
            overview.max_seconds, overview.std_seconds)
        @printf(io, "Mean stage seconds: income %.6f | network %.6f | degrees %.6f\n",
            overview.mean_income_seconds, overview.mean_network_seconds, overview.mean_degree_seconds)
        @printf(io, "Mean GC seconds: %.6f | Mean allocated memory per run: %.3f GB (not peak memory)\n",
            overview.mean_gc_seconds, overview.mean_allocated_bytes / 1e9)
        @printf(io, "Measured simulation time: %.3f s | Sweep wall time including saving: %.3f s (%.3f h)\n",
            overview.total_simulation_seconds, sweep_wall_seconds, sweep_wall_seconds / 3600)
        @printf(io, "Throughput including saving: %.6f simulations/s\n", overview.runs_per_second)
        println(io, "Setup and warm-up are excluded. Overall timing variation includes differences between parameter combinations.")
        if repetitions == 1
            println(io, "One MC run per combination: within-combination timing variability cannot be estimated.")
        end
        println(io, "Results saved in: ", folder)
    end
    write(joinpath(folder, "overview.txt"), overview_text)
    print(overview_text)
    flush(stdout)
    open(joinpath(folder, "completed.toml"), "w") do io
        TOML.print(io, Dict("finished_at" => string(now()), "runs" => completed,
            "sweep_wall_seconds" => sweep_wall_seconds))
    end
    println("Finished. Results saved in: ", folder)
    return folder
end

## 5. Run and save the speedtest. This is the section that starts the experiment.
# A command-line flag overrides input_run_mode. When stepping through the file,
# set input_run_mode in section 1; no command-line knowledge is needed.
# The extended pilot preserves the entire input grid and input_population,
# overriding only repetitions to one. Smoke tests use 30 agents and eight runs.
@assert length(ARGS) <= 1 "Use at most one run-mode flag."
run_mode = isempty(ARGS) ? input_run_mode : replace(only(ARGS), r"^--" => "")
@assert run_mode in ("full", "pilot", "extended-pilot", "smoke-test") "Unknown run mode."
if run_mode == "smoke-test"
    results_folder = run_speedtest(; ginilist=[0.0, 0.9], Rholist=[0, 25], population=30,
        repetitions=2, working_directory=joinpath(input_working_directory, "smoke_tests"), save_agent_data=true)
elseif run_mode == "pilot"
    results_folder = run_speedtest(; ginilist=[0.0, 0.9], Rholist=[0, 25],
        repetitions=1, working_directory=joinpath(input_working_directory, "pilots"))
elseif run_mode == "extended-pilot"
    results_folder = run_speedtest(; repetitions=1,
        working_directory=joinpath(input_working_directory, "extended_pilots"))
else
    results_folder = run_speedtest()
end

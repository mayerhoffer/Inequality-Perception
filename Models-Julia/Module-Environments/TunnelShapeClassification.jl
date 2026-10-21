module TunnelShapeClassification

using DataFrames
using Printf
using Statistics
import Plots

export classify_tunnel_shapes,
       print_shape_overview,
       u_counts_by_threshold,
       plot_u_counts_against_threshold,
       apply_prominence_threshold,
       plot_shape_heatmap

const SHAPE_ORDER = [
    "flat",
    "increasing",
    "decreasing",
    "inverted-U-shaped",
    "U-shaped",
    "complex",
    "insufficient data",
]

const DEFAULT_PARAMETER_COLUMNS = [:alpha, :beta, :Rho, :gamma]

const SHAPE_COLORS = [
    :lightgray,
    :seagreen3,
    :royalblue,
    :orange,
    :purple,
    :firebrick,
    :black,
]

const DIAGNOSTIC_COLUMNS = Set([
    :n_gini_points,
    :n_smoothed_points,
    :n_input_rows,
    :n_ignored_rows,
    :gini_min,
    :gini_max,
    :smoothed_gini_min,
    :smoothed_gini_max,
    :normalization_gini,
    :normalization_utility,
    :normalization_scale,
    :absolute_amplitude,
    :amplitude,
    :amplitude_tolerance,
    :moving_average_window,
    :direction_tolerance,
    :prominence_threshold,
    :prominence_metric,
    :n_inflection_points,
    :inflection_ginis,
    :inflection_types,
    :turning_gini,
    :turning_utility,
    :turning_type,
    :left_edge_utility,
    :right_edge_utility,
    :lowest_gini_utility,
    :highest_gini_utility,
    :edge_utility_difference,
    :normalized_edge_utility_difference,
    :left_edge_difference,
    :right_edge_difference,
    :turning_prominence,
    :relative_prominence,
    :monotonic_shape,
    :candidate_shape,
    :shape,
])

"""
    classify_tunnel_shapes(df; kwargs...) -> DataFrame

Classify each tunneleffect `utilitysum`-over-`gini` curve from direction changes
in a moving average. In this implementation, an "inflection point" means the
local minimum or maximum where the smoothed curve reverses direction.

The rules are:

- no direction change: increasing or decreasing;
- exactly one interior minimum: U-shaped;
- exactly one interior maximum: inverted-U-shaped;
- two or more direction changes: complex;
- only numerically negligible variation: flat.

For a U-shaped candidate, `left_edge_difference` and
`right_edge_difference` are the two edge utilities minus the minimum. For an
inverted-U candidate they are the maximum minus the two edge utilities.
`turning_prominence` is the smaller of the two differences. The classification
threshold is applied to `relative_prominence`, defined as
`turning_prominence / abs(U(highest Gini) - U(lowest Gini))`. Candidates below
the threshold are treated as monotonic according to their overall edge-to-edge
direction, while `candidate_shape` keeps their unthresholded classification.

Rows with the same Gini inside a parameter combination (for example, multiple
simulation seeds) are averaged first. By default, combinations are defined by
the columns among `alpha`, `beta`, `Rho`, and `gamma` that are present; `seed`
is intentionally averaged over.

In the returned DataFrame, `amplitude` is dimensionless because it is computed
on the normalized curve. `absolute_amplitude` retains the corresponding value
in the original utility units, and `normalization_utility` records the divisor.

Important keywords:

- `moving_average_window=5`: number of consecutive Gini values in each mean;
- `prominence_threshold=0.0`: required relative edge-to-turning-point
  prominence;
- `normalization_gini=0.0`: normalize every curve by the absolute utility at
  this Gini before applying amplitude and direction tolerances;
- `direction_tolerance=0.0`: ignore smaller changes between adjacent normalized
  moving averages;
- `amplitude_tolerance=nothing`: numerical flatness tolerance on normalized
  amplitude. For example, `0.01` means 1% of utility at Gini zero;
- `print_overview=true`: print counts and up to `max_examples=10` parameter
  combinations per shape.

# Example

```julia
include("TunnelShapeClassification.jl")
using .TunnelShapeClassification

shapes = classify_tunnel_shapes(workingdf; moving_average_window=5)
display(plot_u_counts_against_threshold(shapes))

# After selecting a useful threshold from the plot:
thresholded_shapes = apply_prominence_threshold(shapes, 0.15)
display(plot_shape_heatmap(shapes, 0.15))
```
"""
function classify_tunnel_shapes(
    df::AbstractDataFrame;
    gini_column::Symbol=:gini,
    utility_column::Symbol=:utilitysum,
    parameter_columns=nothing,
    moving_average_window::Integer=5,
    prominence_threshold::Real=0.0,
    normalization_gini::Real=0.0,
    direction_tolerance::Real=0.0,
    amplitude_tolerance::Union{Nothing,Real}=nothing,
    print_overview::Bool=true,
    max_examples::Integer=10,
    io::IO=stdout,
)
    nrow(df) > 0 || throw(ArgumentError("Cannot classify an empty DataFrame."))
    moving_average_window >= 1 ||
        throw(ArgumentError("moving_average_window must be at least 1."))
    prominence_threshold >= 0 ||
        throw(ArgumentError("prominence_threshold must be non-negative."))
    isfinite(normalization_gini) ||
        throw(ArgumentError("normalization_gini must be finite."))
    direction_tolerance >= 0 ||
        throw(ArgumentError("direction_tolerance must be non-negative."))
    isnothing(amplitude_tolerance) || amplitude_tolerance >= 0 ||
        throw(ArgumentError("amplitude_tolerance must be non-negative."))
    max_examples >= 0 || throw(ArgumentError("max_examples must be non-negative."))

    available_columns = Set(Symbol.(names(df)))
    gini_column in available_columns ||
        throw(ArgumentError("Gini column :$gini_column is not present in the DataFrame."))
    utility_column in available_columns ||
        throw(ArgumentError("Utility column :$utility_column is not present in the DataFrame."))

    parameters = if isnothing(parameter_columns)
        [column for column in DEFAULT_PARAMETER_COLUMNS if column in available_columns]
    else
        Symbol.(collect(parameter_columns))
    end

    length(unique(parameters)) == length(parameters) ||
        throw(ArgumentError("parameter_columns contains a duplicate column."))
    missing_parameters = [column for column in parameters if !(column in available_columns)]
    isempty(missing_parameters) || throw(ArgumentError(
        "Parameter column(s) not present in the DataFrame: " *
        join(":" .* string.(missing_parameters), ", "),
    ))
    gini_column in parameters &&
        throw(ArgumentError("The Gini column cannot also be a parameter column."))
    utility_column in parameters &&
        throw(ArgumentError("The utility column cannot also be a parameter column."))

    rows = NamedTuple[]
    classify_group = group -> _classify_group(
        group,
        parameters,
        gini_column,
        utility_column,
        moving_average_window,
        Float64(prominence_threshold),
        Float64(normalization_gini),
        Float64(direction_tolerance),
        amplitude_tolerance,
    )

    if isempty(parameters)
        push!(rows, classify_group(df))
    else
        for group in groupby(df, parameters; sort=true)
            push!(rows, classify_group(group))
        end
    end

    result = DataFrame(rows)

    if print_overview
        print_shape_overview(
            result;
            parameter_columns=parameters,
            max_examples=max_examples,
            io=io,
        )
    end

    return result
end

"""
    print_shape_overview(classifications; parameter_columns=nothing,
                         max_examples=10, io=stdout)

Print counts, percentages, and up to `max_examples` parameter combinations for
every curve shape. Shapes with no instances are included in the count table.
"""
function print_shape_overview(
    classifications::AbstractDataFrame;
    parameter_columns=nothing,
    max_examples::Integer=10,
    io::IO=stdout,
)
    max_examples >= 0 || throw(ArgumentError("max_examples must be non-negative."))
    :shape in Symbol.(names(classifications)) ||
        throw(ArgumentError("The classifications DataFrame needs a :shape column."))

    parameters = if isnothing(parameter_columns)
        [
            column for column in Symbol.(names(classifications))
            if !(column in DIAGNOSTIC_COLUMNS)
        ]
    else
        Symbol.(collect(parameter_columns))
    end

    available_columns = Set(Symbol.(names(classifications)))
    missing_parameters = [column for column in parameters if !(column in available_columns)]
    isempty(missing_parameters) || throw(ArgumentError(
        "Parameter column(s) not present in the classifications: " *
        join(":" .* string.(missing_parameters), ", "),
    ))

    shape_values = string.(classifications[!, :shape])
    observed_shapes = unique(shape_values)
    extra_shapes = sort(setdiff(observed_shapes, SHAPE_ORDER))
    shapes = vcat(SHAPE_ORDER, extra_shapes)
    counts = Dict(shape => count(==(shape), shape_values) for shape in shapes)
    total = nrow(classifications)

    println(io, "Tunnel-shape classification overview")
    println(io, repeat("=", 42))
    println(io, "Parameter combinations: $total")
    _print_setting(io, classifications, :moving_average_window, "Moving-average window")
    _print_setting(io, classifications, :normalization_gini, "Normalization Gini")
    _print_setting(io, classifications, :prominence_threshold, "Prominence threshold")
    _print_setting(io, classifications, :prominence_metric, "Prominence metric")
    println(io)
    @printf(io, "%-23s %8s %9s\n", "Shape", "Count", "Share")
    println(io, repeat("-", 42))
    for shape in shapes
        share = total == 0 ? 0.0 : 100 * counts[shape] / total
        @printf(io, "%-23s %8d %8.1f%%\n", shape, counts[shape], share)
    end

    println(io)
    println(io, "Examples (up to $max_examples parameter combinations per shape)")
    println(io, repeat("-", 62))

    for shape in shapes
        shape_count = counts[shape]
        shape_count == 0 && continue

        shown = min(max_examples, shape_count)
        qualifier = shown == shape_count ? "all" : "first"
        println(io, "$shape ($qualifier $shown of $shape_count):")

        indices = findall(==(shape), shape_values)
        for (example_number, row_index) in enumerate(Iterators.take(indices, shown))
            if isempty(parameters)
                println(io, "  $example_number. (single curve)")
            else
                combination = join(
                    [
                        "$(column)=$(_format_parameter(classifications[row_index, column]))"
                        for column in parameters
                    ],
                    ", ",
                )
                println(io, "  $example_number. $combination")
            end
        end
        println(io)
    end

    return nothing
end

"""
    u_counts_by_threshold(classifications; thresholds=nothing,
                          prominence_column=:relative_prominence) -> DataFrame

Count U-shaped and inverted-U-shaped candidates over a grid of prominence
thresholds. The input should be the result of `classify_tunnel_shapes`.

Relative prominence is the default. Set
`prominence_column=:turning_prominence` only when an absolute utility threshold
is desired. When `thresholds` is omitted, 101 evenly spaced values from zero
through the largest finite candidate prominence are used.
"""
function u_counts_by_threshold(
    classifications::AbstractDataFrame;
    thresholds=nothing,
    prominence_column::Symbol=:relative_prominence,
)
    available_columns = Set(Symbol.(names(classifications)))
    :candidate_shape in available_columns || throw(ArgumentError(
        "The classifications DataFrame needs a :candidate_shape column.",
    ))
    prominence_column in available_columns || throw(ArgumentError(
        "Prominence column :$prominence_column is not present.",
    ))

    candidate_prominences = Float64[]
    for row in eachrow(classifications)
        candidate_shape = row.candidate_shape
        candidate_shape isa AbstractString || continue
        candidate_shape in ("U-shaped", "inverted-U-shaped") || continue
        prominence = _prominence_float(row[prominence_column])
        !isnothing(prominence) && isfinite(prominence) &&
            push!(candidate_prominences, prominence)
    end

    threshold_values = if isnothing(thresholds)
        maximum_prominence = isempty(candidate_prominences) ? 0.0 :
                             max(0.0, maximum(candidate_prominences))
        maximum_prominence == 0.0 ? [0.0, 1.0] :
        collect(range(0.0, maximum_prominence; length=101))
    else
        sort!(unique(Float64.(collect(thresholds))))
    end

    isempty(threshold_values) && throw(ArgumentError("thresholds cannot be empty."))
    all(isfinite, threshold_values) ||
        throw(ArgumentError("Every threshold must be finite."))
    all(>=(0), threshold_values) ||
        throw(ArgumentError("Every threshold must be non-negative."))

    counts = DataFrame(
        threshold=threshold_values,
        U_shaped=zeros(Int, length(threshold_values)),
        inverted_U_shaped=zeros(Int, length(threshold_values)),
        total_U_shaped=zeros(Int, length(threshold_values)),
    )

    for (index, threshold) in enumerate(threshold_values)
        for row in eachrow(classifications)
            prominence = _prominence_float(row[prominence_column])
            isnothing(prominence) && continue
            prominence > threshold || continue

            if row.candidate_shape == "U-shaped"
                counts.U_shaped[index] += 1
            elseif row.candidate_shape == "inverted-U-shaped"
                counts.inverted_U_shaped[index] += 1
            end
        end
        counts.total_U_shaped[index] =
            counts.U_shaped[index] + counts.inverted_U_shaped[index]
    end

    return counts
end

"""
    plot_u_counts_against_threshold(classifications; thresholds=nothing,
                                    prominence_column=:relative_prominence)

Plot the number of true U, inverted U, and total U-shaped candidates that
remain at each prominence threshold. Returns a `Plots.Plot` object.
"""
function plot_u_counts_against_threshold(
    classifications::AbstractDataFrame;
    thresholds=nothing,
    prominence_column::Symbol=:relative_prominence,
)
    counts = u_counts_by_threshold(
        classifications;
        thresholds=thresholds,
        prominence_column=prominence_column,
    )

    xlabel = prominence_column == :relative_prominence ?
             "Relative prominence threshold" : "Prominence threshold (utility units)"

    plot = Plots.plot(
        counts.threshold,
        counts.U_shaped;
        label="U-shaped",
        linewidth=2.5,
        xlabel=xlabel,
        ylabel="Number of parameter combinations",
        title="U-shape classifications by prominence threshold",
        legend=:topright,
        marker=:none,
    )
    Plots.plot!(
        plot,
        counts.threshold,
        counts.inverted_U_shaped;
        label="Inverted-U-shaped",
        linewidth=2.5,
        marker=:none,
    )
    Plots.plot!(
        plot,
        counts.threshold,
        counts.total_U_shaped;
        label="Total",
        linewidth=2,
        linestyle=:dash,
        color=:black,
        marker=:none,
    )

    return plot
end

"""
    apply_prominence_threshold(classifications, threshold;
                               prominence_column=:relative_prominence,
                               print_overview=true) -> DataFrame

Reclassify an existing `classify_tunnel_shapes` output at a new prominence
threshold without recomputing moving averages or turning points. The input is
not mutated; the returned DataFrame is a copy with updated `shape`,
`prominence_threshold`, and `prominence_metric` columns.

U and inverted-U candidates above the threshold retain their candidate shape.
Candidates at or below it use the stored edge-to-edge `monotonic_shape`.
Relative prominence is the default; use
`prominence_column=:turning_prominence` for an absolute utility threshold.
"""
function apply_prominence_threshold(
    classifications::AbstractDataFrame,
    threshold::Real;
    prominence_column::Symbol=:relative_prominence,
    parameter_columns=nothing,
    print_overview::Bool=true,
    max_examples::Integer=10,
    io::IO=stdout,
)
    threshold >= 0 || throw(ArgumentError("threshold must be non-negative."))
    isfinite(threshold) || throw(ArgumentError("threshold must be finite."))
    max_examples >= 0 || throw(ArgumentError("max_examples must be non-negative."))

    available_columns = Set(Symbol.(names(classifications)))
    for required_column in (:shape, :candidate_shape, prominence_column)
        required_column in available_columns || throw(ArgumentError(
            "The classifications DataFrame needs a :$required_column column.",
        ))
    end

    result = copy(classifications)
    threshold_value = Float64(threshold)

    if :prominence_threshold in available_columns
        result[!, :prominence_threshold] .= threshold_value
    else
        result[!, :prominence_threshold] = fill(threshold_value, nrow(result))
    end
    result[!, :prominence_metric] = fill(prominence_column, nrow(result))

    for row_index in 1:nrow(result)
        candidate_shape = result[row_index, :candidate_shape]
        candidate_shape isa AbstractString || continue

        if candidate_shape in ("U-shaped", "inverted-U-shaped")
            prominence = _prominence_float(result[row_index, prominence_column])
            if isnothing(prominence) || prominence <= 0
                result[row_index, :shape] = "complex"
            elseif prominence > threshold_value
                result[row_index, :shape] = candidate_shape
            else
                result[row_index, :shape] = _stored_monotonic_shape(result, row_index)
            end
        else
            result[row_index, :shape] = candidate_shape
        end
    end

    if print_overview
        print_shape_overview(
            result;
            parameter_columns=parameter_columns,
            max_examples=max_examples,
            io=io,
        )
    end

    return result
end

"""
    plot_shape_heatmap(classifications, threshold;
                       prominence_column=:relative_prominence,
                       alpha_column=:alpha, beta_column=:beta)

Reclassify an existing analysis output at `threshold` and plot its shapes as a
categorical heatmap, with alpha on the x-axis and beta on the y-axis. This does
not rerun the moving-average analysis.

If the DataFrame contains multiple rows for one `(alpha, beta)` pair, identical
shapes are collapsed. Conflicting shapes produce an error; filter to one `Rho`,
`gamma`, or other scenario before plotting in that case.
"""
function plot_shape_heatmap(
    classifications::AbstractDataFrame,
    threshold::Real;
    prominence_column::Symbol=:relative_prominence,
    alpha_column::Symbol=:alpha,
    beta_column::Symbol=:beta,
)
    thresholded = apply_prominence_threshold(
        classifications,
        threshold;
        prominence_column=prominence_column,
        print_overview=false,
    )

    available_columns = Set(Symbol.(names(thresholded)))
    alpha_column in available_columns ||
        throw(ArgumentError("Alpha column :$alpha_column is not present."))
    beta_column in available_columns ||
        throw(ArgumentError("Beta column :$beta_column is not present."))

    shapes_by_pair = Dict{Tuple{Float64,Float64},String}()
    for row in eachrow(thresholded)
        alpha = _finite_float(row[alpha_column])
        beta = _finite_float(row[beta_column])
        isnothing(alpha) && throw(ArgumentError("Every alpha value must be finite and numeric."))
        isnothing(beta) && throw(ArgumentError("Every beta value must be finite and numeric."))

        shape = string(row.shape)
        pair = (alpha, beta)
        if haskey(shapes_by_pair, pair) && shapes_by_pair[pair] != shape
            throw(ArgumentError(
                "Multiple shapes exist for alpha=$alpha, beta=$beta. " *
                "Filter the classifications to one scenario before plotting.",
            ))
        end
        shapes_by_pair[pair] = shape
    end

    alphas = sort!(unique(first.(keys(shapes_by_pair))))
    betas = sort!(unique(last.(keys(shapes_by_pair))))
    isempty(alphas) && throw(ArgumentError("There are no alpha-beta pairs to plot."))

    alpha_indices = Dict(value => index for (index, value) in enumerate(alphas))
    beta_indices = Dict(value => index for (index, value) in enumerate(betas))
    shape_codes = Dict(shape => index for (index, shape) in enumerate(SHAPE_ORDER))
    heat_values = fill(NaN, length(betas), length(alphas))

    for ((alpha, beta), shape) in shapes_by_pair
        haskey(shape_codes, shape) || throw(ArgumentError("Unknown shape '$shape'."))
        heat_values[beta_indices[beta], alpha_indices[alpha]] = shape_codes[shape]
    end

    metric_label = prominence_column == :relative_prominence ? "relative" : "absolute"
    color_gradient = Plots.cgrad(SHAPE_COLORS, length(SHAPE_COLORS); categorical=true)

    main_heatmap = Plots.heatmap(
        alphas,
        betas,
        heat_values;
        xlabel="alpha",
        ylabel="beta",
        #title="Tunnel shapes ($metric_label prominence threshold = $(_format_parameter(threshold)))",
        color=color_gradient,
        clims=(0.5, length(SHAPE_ORDER) + 0.5),
        colorbar=false,
        aspect_ratio=:equal,
    )

    shape_codes_list = collect(1:length(SHAPE_ORDER))
    color_key = Plots.heatmap(
        [1.0],
        shape_codes_list,
        reshape(Float64.(shape_codes_list), :, 1);
        color=color_gradient,
        clims=(0.5, length(SHAPE_ORDER) + 0.5),
        colorbar=false,
        title="Shape",
        xticks=false,
        yticks=(shape_codes_list, SHAPE_ORDER),
        ymirror=true,
        xlims=(0.5, 1.5),
        ylims=(0.5, length(SHAPE_ORDER) + 0.5),
        legend=false,
        tickfontsize=9,
    )

    layout = Plots.grid(1, 2; widths=[0.80, 0.20])
    return Plots.plot(main_heatmap, color_key; layout=layout, size=(1100, 650))
end

function _classify_group(
    group::AbstractDataFrame,
    parameters::Vector{Symbol},
    gini_column::Symbol,
    utility_column::Symbol,
    moving_average_window::Integer,
    prominence_threshold::Float64,
    normalization_gini::Float64,
    direction_tolerance::Float64,
    requested_amplitude_tolerance,
)
    parameter_values = NamedTuple{Tuple(parameters)}(
        Tuple(group[1, column] for column in parameters),
    )

    ginis, utilities, ignored_rows = _mean_curve(group, gini_column, utility_column)
    n_points = length(ginis)
    input_rows = nrow(group)

    base = (
        n_points=n_points,
        input_rows=input_rows,
        ignored_rows=ignored_rows,
        moving_average_window=moving_average_window,
        prominence_threshold=prominence_threshold,
        prominence_metric=:relative_prominence,
        normalization_gini=normalization_gini,
        direction_tolerance=direction_tolerance,
    )

    if n_points < moving_average_window + 1
        return merge(
            parameter_values,
            _diagnostics(; base..., shape="insufficient data"),
        )
    end

    normalization_index = _normalization_index(ginis, normalization_gini)
    isnothing(normalization_index) && throw(ArgumentError(
        "No observation at normalization_gini=$normalization_gini for parameter " *
        "combination $parameter_values.",
    ))
    normalization_utility = utilities[normalization_index]
    normalization_scale = abs(normalization_utility)
    normalization_scale > 100 * eps(Float64) || throw(ArgumentError(
        "Cannot normalize parameter combination $parameter_values because utilitysum " *
        "at gini=$normalization_gini is zero or numerically indistinguishable from zero.",
    ))

    normalized_utilities = utilities ./ normalization_scale
    smoothed_ginis, smoothed_utilities =
        _moving_average(ginis, utilities, moving_average_window)
    _, smoothed_normalized_utilities =
        _moving_average(ginis, normalized_utilities, moving_average_window)
    n_smoothed_points = length(smoothed_ginis)
    absolute_amplitude = maximum(smoothed_utilities) - minimum(smoothed_utilities)
    amplitude = maximum(smoothed_normalized_utilities) -
                minimum(smoothed_normalized_utilities)
    epsilon_amplitude = isnothing(requested_amplitude_tolerance) ?
        _automatic_amplitude_tolerance(smoothed_normalized_utilities) :
        Float64(requested_amplitude_tolerance)

    common = (
        base...,
        n_smoothed_points=n_smoothed_points,
        gini_min=first(ginis),
        gini_max=last(ginis),
        smoothed_gini_min=first(smoothed_ginis),
        smoothed_gini_max=last(smoothed_ginis),
        normalization_utility=normalization_utility,
        normalization_scale=normalization_scale,
        lowest_gini_utility=first(utilities),
        highest_gini_utility=last(utilities),
        edge_utility_difference=abs(last(utilities) - first(utilities)),
        normalized_edge_utility_difference=
            abs(last(normalized_utilities) - first(normalized_utilities)),
        absolute_amplitude=absolute_amplitude,
        amplitude=amplitude,
        amplitude_tolerance=epsilon_amplitude,
    )

    if amplitude <= epsilon_amplitude
        return merge(
            parameter_values,
            _diagnostics(
                ;
                common...,
                n_inflection_points=0,
                monotonic_shape="flat",
                candidate_shape="flat",
                shape="flat",
            ),
        )
    end

    turning_points =
        _find_turning_points(
            smoothed_ginis,
            smoothed_normalized_utilities,
            direction_tolerance,
        )
    n_inflection_points = length(turning_points)
    inflection_ginis = [point.gini for point in turning_points]
    inflection_types = [point.kind for point in turning_points]

    turning_common = (
        common...,
        n_inflection_points=n_inflection_points,
        inflection_ginis=inflection_ginis,
        inflection_types=inflection_types,
    )

    if n_inflection_points == 0
        shape = _overall_direction(smoothed_normalized_utilities, direction_tolerance)
        return merge(
            parameter_values,
            _diagnostics(
                ;
                turning_common...,
                monotonic_shape=shape,
                candidate_shape=shape,
                shape=shape,
            ),
        )
    elseif n_inflection_points >= 2
        monotonic_shape =
            _overall_direction(smoothed_normalized_utilities, direction_tolerance)
        return merge(
            parameter_values,
            _diagnostics(
                ;
                turning_common...,
                monotonic_shape=monotonic_shape,
                candidate_shape="complex",
                shape="complex",
            ),
        )
    end

    turning_point = only(turning_points)
    turning_index = turning_point.index
    turning_utility = smoothed_utilities[turning_index]
    left_edge_utility = first(smoothed_utilities)
    right_edge_utility = last(smoothed_utilities)
    is_interior = 1 < turning_index < n_smoothed_points

    left_edge_difference, right_edge_difference, candidate_shape =
        if turning_point.kind == "minimum"
            (
                left_edge_utility - turning_utility,
                right_edge_utility - turning_utility,
                "U-shaped",
            )
        else
            (
                turning_utility - left_edge_utility,
                turning_utility - right_edge_utility,
                "inverted-U-shaped",
            )
        end

    turning_prominence = min(left_edge_difference, right_edge_difference)
    edge_utility_difference = common.edge_utility_difference
    relative_prominence = if common.normalized_edge_utility_difference > epsilon_amplitude
        turning_prominence / edge_utility_difference
    elseif turning_prominence > 0
        Inf
    else
        0.0
    end
    monotonic_shape =
        _overall_direction(smoothed_normalized_utilities, direction_tolerance)

    shape = if !is_interior || turning_prominence <= 0
        "complex"
    elseif relative_prominence > prominence_threshold
        candidate_shape
    else
        monotonic_shape
    end

    return merge(
        parameter_values,
        _diagnostics(
            ;
            turning_common...,
            turning_gini=turning_point.gini,
            turning_utility=turning_utility,
            turning_type=turning_point.kind,
            left_edge_utility=left_edge_utility,
            right_edge_utility=right_edge_utility,
            left_edge_difference=left_edge_difference,
            right_edge_difference=right_edge_difference,
            turning_prominence=turning_prominence,
            relative_prominence=relative_prominence,
            monotonic_shape=monotonic_shape,
            candidate_shape=candidate_shape,
            shape=shape,
        ),
    )
end

function _mean_curve(group, gini_column::Symbol, utility_column::Symbol)
    values_by_gini = Dict{Float64,Vector{Float64}}()
    ignored_rows = 0

    for row in eachrow(group)
        gini = _finite_float(row[gini_column])
        utility = _finite_float(row[utility_column])

        if isnothing(gini) || isnothing(utility)
            ignored_rows += 1
            continue
        end

        push!(get!(values_by_gini, gini, Float64[]), utility)
    end

    ginis = sort!(collect(keys(values_by_gini)))
    utilities = [mean(values_by_gini[gini]) for gini in ginis]
    return ginis, utilities, ignored_rows
end

function _normalization_index(ginis::Vector{Float64}, target_gini::Float64)
    tolerance = max(1.0, abs(target_gini)) * sqrt(eps(Float64))
    return findfirst(gini -> isapprox(gini, target_gini; atol=tolerance, rtol=0.0), ginis)
end

function _moving_average(
    ginis::Vector{Float64},
    utilities::Vector{Float64},
    window::Integer,
)
    n_smoothed = length(utilities) - window + 1
    smoothed_ginis = Vector{Float64}(undef, n_smoothed)
    smoothed_utilities = Vector{Float64}(undef, n_smoothed)

    for start_index in 1:n_smoothed
        indices = start_index:(start_index + window - 1)
        smoothed_ginis[start_index] = mean(@view ginis[indices])
        smoothed_utilities[start_index] = mean(@view utilities[indices])
    end

    return smoothed_ginis, smoothed_utilities
end

function _find_turning_points(
    ginis::Vector{Float64},
    utilities::Vector{Float64},
    direction_tolerance::Float64,
)
    differences = diff(utilities)
    directional_indices = findall(difference -> abs(difference) > direction_tolerance, differences)
    turning_points = NamedTuple[]

    for position in 2:length(directional_indices)
        previous_index = directional_indices[position - 1]
        current_index = directional_indices[position]
        previous_direction = sign(differences[previous_index])
        current_direction = sign(differences[current_index])
        previous_direction == current_direction && continue

        plateau_indices = (previous_index + 1):current_index
        if previous_direction < 0 && current_direction > 0
            relative_index = argmin(@view utilities[plateau_indices])
            turning_index = first(plateau_indices) + relative_index - 1
            kind = "minimum"
        else
            relative_index = argmax(@view utilities[plateau_indices])
            turning_index = first(plateau_indices) + relative_index - 1
            kind = "maximum"
        end

        push!(
            turning_points,
            (index=turning_index, gini=ginis[turning_index], kind=kind),
        )
    end

    return turning_points
end

function _overall_direction(utilities::Vector{Float64}, direction_tolerance::Float64)
    edge_difference = last(utilities) - first(utilities)
    if edge_difference > direction_tolerance
        return "increasing"
    elseif edge_difference < -direction_tolerance
        return "decreasing"
    end
    return "complex"
end

function _diagnostics(
    ;
    n_points,
    input_rows,
    ignored_rows,
    moving_average_window,
    prominence_threshold,
    prominence_metric,
    normalization_gini,
    direction_tolerance,
    n_smoothed_points=0,
    gini_min=missing,
    gini_max=missing,
    smoothed_gini_min=missing,
    smoothed_gini_max=missing,
    normalization_utility=missing,
    normalization_scale=missing,
    absolute_amplitude=missing,
    amplitude=missing,
    amplitude_tolerance=missing,
    n_inflection_points=missing,
    inflection_ginis=Float64[],
    inflection_types=String[],
    turning_gini=missing,
    turning_utility=missing,
    turning_type=missing,
    left_edge_utility=missing,
    right_edge_utility=missing,
    lowest_gini_utility=missing,
    highest_gini_utility=missing,
    edge_utility_difference=missing,
    normalized_edge_utility_difference=missing,
    left_edge_difference=missing,
    right_edge_difference=missing,
    turning_prominence=missing,
    relative_prominence=missing,
    monotonic_shape=missing,
    candidate_shape=missing,
    shape,
)
    return (
        n_gini_points=n_points,
        n_smoothed_points=n_smoothed_points,
        n_input_rows=input_rows,
        n_ignored_rows=ignored_rows,
        gini_min=gini_min,
        gini_max=gini_max,
        smoothed_gini_min=smoothed_gini_min,
        smoothed_gini_max=smoothed_gini_max,
        normalization_gini=normalization_gini,
        normalization_utility=normalization_utility,
        normalization_scale=normalization_scale,
        absolute_amplitude=absolute_amplitude,
        amplitude=amplitude,
        amplitude_tolerance=amplitude_tolerance,
        moving_average_window=moving_average_window,
        direction_tolerance=direction_tolerance,
        prominence_threshold=prominence_threshold,
        prominence_metric=prominence_metric,
        n_inflection_points=n_inflection_points,
        inflection_ginis=inflection_ginis,
        inflection_types=inflection_types,
        turning_gini=turning_gini,
        turning_utility=turning_utility,
        turning_type=turning_type,
        left_edge_utility=left_edge_utility,
        right_edge_utility=right_edge_utility,
        lowest_gini_utility=lowest_gini_utility,
        highest_gini_utility=highest_gini_utility,
        edge_utility_difference=edge_utility_difference,
        normalized_edge_utility_difference=normalized_edge_utility_difference,
        left_edge_difference=left_edge_difference,
        right_edge_difference=right_edge_difference,
        turning_prominence=turning_prominence,
        relative_prominence=relative_prominence,
        monotonic_shape=monotonic_shape,
        candidate_shape=candidate_shape,
        shape=shape,
    )
end

function _stored_monotonic_shape(classifications::AbstractDataFrame, row_index::Integer)
    available_columns = Set(Symbol.(names(classifications)))

    if :monotonic_shape in available_columns
        stored_shape = classifications[row_index, :monotonic_shape]
        stored_shape isa AbstractString && return stored_shape
    end

    if :left_edge_utility in available_columns && :right_edge_utility in available_columns
        left_edge = _finite_float(classifications[row_index, :left_edge_utility])
        right_edge = _finite_float(classifications[row_index, :right_edge_utility])
        if !isnothing(left_edge) && !isnothing(right_edge)
            tolerance = if :direction_tolerance in available_columns
                something(_finite_float(classifications[row_index, :direction_tolerance]), 0.0)
            else
                0.0
            end
            return _overall_direction([left_edge, right_edge], tolerance)
        end
    end

    current_shape = classifications[row_index, :shape]
    current_shape in ("increasing", "decreasing") && return current_shape
    return "complex"
end

function _automatic_amplitude_tolerance(utilities::Vector{Float64})
    scale = max(1.0, maximum(abs, utilities))
    return 100 * eps(Float64) * scale
end

function _finite_float(value)
    value isa Real || return nothing
    converted = Float64(value)
    return isfinite(converted) ? converted : nothing
end

function _prominence_float(value)
    value isa Real || return nothing
    converted = Float64(value)
    return isnan(converted) ? nothing : converted
end

function _format_parameter(value)
    if value isa AbstractFloat
        return @sprintf("%.6g", value)
    end
    return repr(value)
end

function _print_setting(io, classifications, column::Symbol, label::String)
    column in Symbol.(names(classifications)) || return nothing
    values = unique(classifications[!, column])
    length(values) == 1 && println(io, "$label: $(only(values))")
    return nothing
end

end # module TunnelShapeClassification

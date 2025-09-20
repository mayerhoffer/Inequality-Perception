

## Adds Lorenz curve to df with individual income values

const working_directory = "C:/Users/dmayerh/Onedrive - Personal/OneDrive/DATIpilot/Inhaltliches/Papers/Hirschman/SimOutput" 
workingdf = outputdfRho0 # CSV.read(joinpath(working_directory, "Rho4.csv"), DataFrame)


# --- helpers  ---
#const NUM_RE = r"[+-]?(?:\d+(?:\.\d+)?|\.\d+)(?:[eE][+-]?\d+)?"

parse_vec_str(s::AbstractString) = [parse(Float64, m.match) for m in eachmatch(NUM_RE, s)]

to_vec(v) = v isa AbstractVector{<:Real} ? collect(Float64, v) :
            v isa AbstractVector         ? Float64.(v) :
            v isa AbstractString         ? parse_vec_str(v) :
            error("Unsupported element type: $(typeof(v))")

# Stack a column of vector-like values into a seeds×rank Matrix{Float64}
function col_to_matrix(col)
    mats = Vector{Vector{Float64}}(undef, length(col))
    for i in eachindex(col)
        mats[i] = to_vec(col[i])
    end
    n = length(mats[1])
    @assert all(length.(mats) .== n) "Inconsistent vector lengths"
    reduce(vcat, (reshape(m, 1, n) for m in mats))::Matrix{Float64}
end

# --- NEW: Lorenz from a single income vector ---
lorenz_from_vec(v::AbstractVector{<:Real}) = begin
    vv = collect(Float64, v)
    s = sum(vv)
    s == 0 ? zeros(Float64, length(vv)) : cumsum(vv) ./ s
end

# Add the Lorenz column (length-1000 vector per row)
transform!(workingdf, :Y => ByRow(x -> lorenz_from_vec(to_vec(x))) => :lorenz)

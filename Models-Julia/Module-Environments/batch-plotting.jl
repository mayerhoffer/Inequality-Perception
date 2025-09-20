using DataFrames, Statistics, Plots, CSV

# ------------------------------------------
# CONFIG
# ------------------------------------------
const working_directory = "C:/Users/dmayerh/Onedrive - Personal/OneDrive/DATIpilot/Inhaltliches/Papers/Hirschman/SimOutput" 
const out_img_dir = joinpath(working_directory, "plots")
mkpath(out_img_dir)

# Choose output format:
const FILE_EXT = ".svg"       # switch to ".jpg" if you prefer JPEG
# ------------------------------------------

# Load CSV
workingdf = outputdfRho0gini # CSV.read(joinpath(working_directory, "Rho4.csv"), DataFrame)
workingdf = CSV.read(joinpath(working_directory, "Rho4gini.csv"), DataFrame)

if eltype(workingdf.utilitysum) <: AbstractString
    transform!(workingdf, :utilitysum => ByRow(x -> begin
        y = tryparse(Float64, x)
        y === nothing ? NaN : y
    end) => :utilitysum)
end

# Snap α,β to two decimals ⇒ integer indices 0..100 (stable filenames)
to_idx(x::Real) = round(Int, round(x, digits=2) * 100)

# Precompute integer keys for grouping
df = transform(workingdf,
    :alpha => ByRow(to_idx) => :aidx,
    :beta  => ByRow(to_idx) => :bidx)

gdf = groupby(df, [:aidx, :bidx])

# Plot a single (α,β) selection
function plot_pair(sel::AbstractDataFrame, α::Float64, β::Float64)
    p = scatter(sel.gini, sel.utilitysum;
        xlabel = "Gini",
        ylabel = "Aggregate Happiness",
        title  = "Impact of Inequality on Happiness (α=$(α), β=$(β))",
        legend = false,
        size   = (900, 560),
        markersize = 2,
        markerstrokewidth = 0.5,
        markercolor = "#A259FF",   # purple points
        alpha = 0.4                # semi-transparent points
    )

    g = combine(groupby(sel, :gini), :utilitysum => mean => :mean_utilitysum)
    sort!(g, :gini)

    plot!(p, g.gini, g.mean_utilitysum;
        lw = 2,
        marker = :circle,
        markersize = 5,
        label = "mean utilitysum",
        color = "#FFE164",         # yellow line
        markercolor = "#FFE164",   # yellow markers
        alpha = 1.0                # non-transparent
    )

    return p
end

# Generate images
valid_pairs = Vector{Tuple{Int,Int}}()
for sub in gdf
    a = first(sub.aidx); b = first(sub.bidx)
    a + b > 100 && continue                 # α+β ≤ 1 (in 0..100 space)
    isempty(sub) && continue

    α = a / 100; β = b / 100
    fname = joinpath(out_img_dir,
        "a$(lpad(string(a),3,'0'))_b$(lpad(string(b),3,'0'))" * FILE_EXT)

    if !isfile(fname)  # skip if already rendered
        p = plot_pair(sub, α, β)
        savefig(p, fname)
#        close(p)
    end
    push!(valid_pairs, (a, b))
end

isempty(valid_pairs) && error("No valid (α,β) pairs found.")
sort!(valid_pairs)
a0, b0 = valid_pairs[1]
println("Wrote $(length(valid_pairs)) images to $out_img_dir; default = (α=$(a0/100), β=$(b0/100))")

# Build small JS set for “no data” handling
function valid_pairs_js(pairs::Vector{Tuple{Int,Int}})
    keys = join(["'" * lpad(string(a),3,'0') * "-" * lpad(string(b),3,'0') * "'" for (a,b) in pairs], ",")
    return "new Set([$keys])"
end

# HTML shell with sliders that swap <img src>
html = """
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>Gini on Aggregate Happiness — static gallery</title>
<style>
 body { font-family: system-ui, -apple-system, Segoe UI, Roboto, sans-serif; margin: 16px; }
 .row { display:flex; gap: 16px; align-items:flex-start; flex-wrap: wrap; }
 .controls { display:flex; flex-direction:column; gap:12px; min-width: 420px; }
 .control-row { display:flex; align-items:center; gap:8px; }
 .control-row label { min-width: 70px; display:inline-block; }
 .slider { width: 280px; }
 .stepper { display:inline-flex; gap:6px; }
 .stepper button { width: 28px; height: 28px; border:1px solid #ccc; background:#f7f7f7; border-radius:6px; cursor:pointer; line-height: 0; font-size:18px; }
 .stepper button:active { transform: translateY(1px); }
 .card { max-width: 100%; }
 #plot { max-width: 100%; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,.06); }
 .warn, .missing { padding: 8px 12px; border-radius: 6px; display:none; }
 .warn { background:#fff7e6; border:1px solid #ffe0a3; }
 .missing { background:#ffe6e6; border:1px solid #ffb3b3; }
 .val { font-variant-numeric: tabular-nums; width: 60px; display:inline-block; text-align:right; }
</style>
</head>

<body>
  <h2>Gini vs Aggregate Happiness</h2>

  <div class="row">
    <!-- Left column: rho dropdown above the sliders -->
    <div class="controls">
      <div class="control-row">
        <label>ρ</label>
        <select id="rho"></select>
      </div>

      <div class="control-row">
        <label>α</label>
        <button id="alphaMinus" class="stepper-btn" type="button" aria-label="Decrease alpha">−</button>
        <input id="alpha" class="slider" type="range" min="0" max="100" step="1" value="%A0%">
        <button id="alphaPlus"  class="stepper-btn" type="button" aria-label="Increase alpha">+</button>
        <span class="val" id="alphaVal"></span>
      </div>

      <div class="control-row">
        <label>β</label>
        <button id="betaMinus" class="stepper-btn" type="button" aria-label="Decrease beta">−</button>
        <input id="beta"  class="slider" type="range" min="0" max="100" step="1" value="%B0%">
        <button id="betaPlus"  class="stepper-btn" type="button" aria-label="Increase beta">+</button>
        <span class="val" id="betaVal"></span>
      </div>
    </div>

    <!-- Right column: image card -->
    <div class="card">
      <div class="warn"    id="warn">α + β must be ≤ 1</div>
      <div class="missing" id="missing">No data for this (α, β).</div>
      <img id="plot" alt="plot" src="" loading="lazy" />
    </div>
  </div>

<script>
/* Provided at generation time: */
const validPairs = %VALID%;   // Set like new Set(['000-000','000-001', ...])
const EXT = "%EXT%";          // ".svg" or ".jpg"

/* Define your rho options here (label shown, folder name used) */
const RHO_OPTIONS = [
  { label: "ρ = 0", folder: "plotsrho0_gini" },
//  { label: "ρ = 1.5", folder: "plotsrho1point5_sigma" },
  { label: "ρ = 4", folder: "plotsrho4_gini" },
//  { label: "ρ = 8", folder: "plotsrho8_sigma" },
];

const alpha    = document.getElementById('alpha');
const beta     = document.getElementById('beta');
const alphaVal = document.getElementById('alphaVal');
const betaVal  = document.getElementById('betaVal');
const missing  = document.getElementById('missing');
const img      = document.getElementById('plot');
const rhoSel   = document.getElementById('rho');

/* Stepper buttons */
const alphaMinus = document.getElementById('alphaMinus');
const alphaPlus  = document.getElementById('alphaPlus');
const betaMinus  = document.getElementById('betaMinus');
const betaPlus   = document.getElementById('betaPlus');

/* Populate rho dropdown */
for (const opt of RHO_OPTIONS) {
  const o = document.createElement('option');
  o.value = opt.folder;
  o.textContent = opt.label;
  rhoSel.appendChild(o);
}
/* Default to ρ=4 (first option) */
if (RHO_OPTIONS.length) rhoSel.value = RHO_OPTIONS[0].folder;

function pad3(n){ return n.toString().padStart(3,'0'); }
function setAlphaValue(a){ a = Math.max(0, Math.min(100, a)); alpha.value = a; alphaVal.textContent = (a/100).toFixed(2); }
function setBetaValue(b){  b = Math.max(0, Math.min(100, b)); beta.value  = b; betaVal.textContent  = (b/100).toFixed(2); }

function updateImage(){
  const a = parseInt(alpha.value,10);
  const b = parseInt(beta.value,10);
  const key = pad3(a) + '-' + pad3(b);
  const folder = rhoSel.value;                 // use selected rho’s folder
  if (validPairs.has(key)){
    img.src = folder + '/a' + pad3(a) + '_b' + pad3(b) + EXT;
    img.style.display = 'block';
    missing.style.display = 'none';
  } else {
    img.style.display = 'none';
    missing.style.display = 'block';
  }
}

/* Cooperative sliders: keep α+β ≤ 1 (i.e., a+b ≤ 100) */
let isUpdating = false;

function updateFromAlpha(){
  if (isUpdating) return; isUpdating = true;
  let a = parseInt(alpha.value,10);
  let b = parseInt(beta.value,10);
  const bmax = 100 - a;
  if (b > bmax) b = bmax;
  setAlphaValue(a); setBetaValue(b);
  updateImage(); isUpdating = false;
}

function updateFromBeta(){
  if (isUpdating) return; isUpdating = true;
  let a = parseInt(alpha.value,10);
  let b = parseInt(beta.value,10);
  const amax = 100 - b;
  if (a > amax) a = amax;
  setAlphaValue(a); setBetaValue(b);
  updateImage(); isUpdating = false;
}

/* Stepper handlers: ±1 on the 0..100 scale (i.e., ±0.01 in value) */
alphaMinus.addEventListener('click', () => { setAlphaValue(parseInt(alpha.value,10) - 1); updateFromAlpha(); });
alphaPlus .addEventListener('click', () => { setAlphaValue(parseInt(alpha.value,10) + 1); updateFromAlpha(); });
betaMinus .addEventListener('click', () => { setBetaValue (parseInt(beta.value,10)  - 1); updateFromBeta();  });
betaPlus  .addEventListener('click', () => { setBetaValue (parseInt(beta.value,10)  + 1); updateFromBeta();  });

/* Wire up slider & rho events */
alpha.addEventListener('input', updateFromAlpha);
beta .addEventListener('input', updateFromBeta);
rhoSel.addEventListener('change', updateImage);

/* Initialize from your %A0% / %B0% values already in the inputs */
updateFromAlpha();
</script>

</body></html>
"""


# Fill placeholders and write HTML
html = replace(html,
    "%A0%" => string(a0),
    "%B0%" => string(b0),
    "%VALID%" => valid_pairs_js(valid_pairs),
    "%EXT%" => FILE_EXT,
)

open(joinpath(working_directory, "happiness_gini.html"), "w") do io
    write(io, html)
end

println("Open: ", joinpath(working_directory, "happiness_gini.html"))

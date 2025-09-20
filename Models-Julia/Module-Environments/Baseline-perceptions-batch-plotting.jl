using DataFrames, Statistics, Plots, CSV, Printf

# ------------------------------------------
# CONFIG
# ------------------------------------------
const working_directory = "C:/Users/dmayerh/Onedrive - Personal/OneDrive/DATIpilot/Inhaltliches/Papers/Hirschman/SimOutput" 

# Choose output format:
const FILE_EXT = ".png"       # switch to ".jpg" if you prefer JPEG
# ------------------------------------------

# Load CSV
workingdf = outputdfRho0perceptionsGini # CSV.read(joinpath(working_directory, "Rho4.csv"), DataFrame)
#workingdf = CSV.read(joinpath(working_directory, "Rho4perceptionsGini.csv"), DataFrame)


ENV["GKSwstype"] = "nul"   # headless GR (no pop-up windows)

isdir(working_directory) || mkpath(working_directory)

# ── Helpers to parse vector-like strings -> Vector{Float64} ───────────────────
const NUM_RE = r"[+-]?(?:\d+(?:\.\d+)?|\.\d+)(?:[eE][+-]?\d+)?"
parse_vec_str(s::AbstractString) = [parse(Float64, m.match) for m in eachmatch(NUM_RE, s)]

to_vec(v) = v isa AbstractVector{<:Real} ? collect(Float64, v) :
            v isa AbstractVector         ? Float64.(v) :
            v isa AbstractString         ? parse_vec_str(v) :
            error("Unsupported element type in vector-like column: $(typeof(v))")

# Stack a column of vector-like values into a seeds×rank Matrix{Float64}
function col_to_matrix(col)
    mats = Vector{Vector{Float64}}(undef, length(col))
    for i in eachindex(col)
        mats[i] = to_vec(col[i])                  # <- converts String/Any/Vector→Vector{Float64}
    end
    n = length(mats[1])
    @assert all(length.(mats) .== n) "Inconsistent vector lengths in column"
    # rows = seeds, cols = income rank
    reduce(vcat, (reshape(m, 1, n) for m in mats))::Matrix{Float64}
end



# ── Output folder for this rho ─────────────────────────────────────────────────
rho_val = first(workingdf.Rho)
out_folder = joinpath(working_directory, "plotsrho$(rho_val)")
mkpath(out_folder)

# ── Plot helpers (colors consistent with your scheme) ──────────────────────────
const POINT_COLOR = "#8fa674"   # motivational green
const LINE_COLOR  = "#1e1e1e"   # black

# flatten a seeds×rank matrix into scatter (x,y) for plotting
flatten_for_scatter(M) = (repeat(1:size(M,2), size(M,1)), vec(transpose(M)))

function save_series_image_svg!(fname::AbstractString;
    M::AbstractMatrix{<:Real}, ylabel::AbstractString, title::AbstractString, figsize=(900,560))

    # scatter points for all seeds
    X, Y = flatten_for_scatter(M)
    ranks = 1:size(M,2)
    meanv = vec(mean(M; dims=1))

    p = scatter(X, Y;
        legend=false, markersize=0.5, markerstrokewidth=0, markercolor=POINT_COLOR, alpha=0.20,
        xlabel="Income rank", ylabel=ylabel, size=figsize, title=title
    )
    plot!(p, ranks, meanv; color=LINE_COLOR, lw=2, marker=:circle, markersize=1)
    savefig(p, fname)
end


function save_series_image_jpeg!(fname::AbstractString;
    M::AbstractMatrix{<:Real}, ylabel::AbstractString, title::AbstractString, figsize=(675,420))

    # scatter points for all seeds
    X, Y = flatten_for_scatter(M)
    ranks = 1:size(M,2)
    meanv = vec(mean(M; dims=1))

    p = scatter(X, Y;
        legend=false, markersize=0.25, markerstrokewidth=0, markercolor=POINT_COLOR, alpha=0.15,
        xlabel="Income rank", ylabel=ylabel, size=figsize, title=title
    )
    plot!(p, ranks, meanv; color=LINE_COLOR, lw=2, marker=:circle, markersize=1)
    savefig(p, fname)
end

# ── Generate FOUR images per gini: Y / MEAN / MAX / MIN ───────────────────────
to_gidx(x::Real) = round(Int, round(x, digits=2) * 100)   # 0..100 integer key
ginis = sort(unique(workingdf.gini))
gkeys = String[]

@info "Rendering per-gini images for ρ=$(rho_val) → $(out_folder) as $(FILE_EXT)"
for g in ginis
    rows = view(workingdf, workingdf.gini .== g, :)
    isempty(rows) && continue

    # build matrices seeds×rank
    Ymat   = col_to_matrix(rows.Y)
    MEmat  = col_to_matrix(rows.perceptionsOfMEAN)
    MAXmat = col_to_matrix(rows.perceptionsOfMAX)
    MINmat = col_to_matrix(rows.perceptionsOfMIN)
    LZmat = col_to_matrix(rows.lorenz)

    gi = to_gidx(g); gkey = @sprintf("%03d", gi)
    gfile = x -> joinpath(out_folder, @sprintf("g%03d_%s%s", gi, x, FILE_EXT))

    # filenames
    fY   = gfile("Y")
    fLZ = gfile("LORENZ")
    fME  = gfile("MEAN")
    fMAX = gfile("MAX")
    fMIN = gfile("MIN")

    # skip if already exists (resume-friendly)
    if FILE_EXT == ".png"
        if !(isfile(fY) && isfile(fLZ) && isfile(fME) && isfile(fMAX) && isfile(fMIN))
            title_base = "ρ=$(rho_val), Gini=$(round(g, digits=2))"
            !isfile(fY)   && save_series_image_jpeg!(fY,  M=Ymat,   ylabel="Y",                   title="Y — "*title_base)
            !isfile(fLZ) && save_series_image_svg!(fLZ,M=LZmat, ylabel="Cumulative Y",   title="Lorenz Curve — "*title_base)
            !isfile(fME)  && save_series_image_jpeg!(fME, M=MEmat,  ylabel="Perception of mean",  title="Perception of mean — "*title_base)
            !isfile(fMAX) && save_series_image_jpeg!(fMAX,M=MAXmat, ylabel="Perception of max",   title="Perception of max — "*title_base)
            !isfile(fMIN) && save_series_image_jpeg!(fMIN,M=MINmat, ylabel="Perception of min",   title="Perception of min — "*title_base)
        end
    else
        if !(isfile(fY) && isfile(LZmat) && isfile(fME) && isfile(fMAX) && isfile(fMIN))
            title_base = "ρ=$(rho_val), Gini=$(round(g, digits=2))"
            !isfile(fY)   && save_series_image_svg!(fY,  M=Ymat,   ylabel="Y",                   title="Y — "*title_base)
            !isfile(fLZ) && save_series_image_svg!(fLZ,M=LZmat, ylabel="Cumulative Y",   title="Lorenz Curve — "*title_base)
            !isfile(fME)  && save_series_image_svg!(fME, M=MEmat,  ylabel="Perception of mean",  title="Perception of mean — "*title_base)
            !isfile(fMAX) && save_series_image_svg!(fMAX,M=MAXmat, ylabel="Perception of max",   title="Perception of max — "*title_base)
            !isfile(fMIN) && save_series_image_svg!(fMIN,M=MINmat, ylabel="Perception of min",   title="Perception of min — "*title_base)
        end
    end

    push!(gkeys, gkey)
end

# ---- Choose the initial gini position shown when the page opens (e.g., "000" = 0.00) ----
g0_key = "000"   # "050" for 0.50, "100" for 1.00, etc.

# ---- HTML (manual rho list; edit inside RHO_OPTIONS below) ----
html = """
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>Income, Perceptions & Lorenz — static gallery</title>
<style>
 body { font-family: system-ui, -apple-system, Segoe UI, Roboto, sans-serif; margin: 16px; }
 .row { display:flex; gap: 16px; align-items:flex-start; flex-wrap: wrap; }
 .controls { display:flex; flex-direction:column; gap:12px; min-width: 420px; }
 .control-row { display:flex; align-items:center; gap:8px; }
 .control-row label { min-width: 70px; display:inline-block; }
 .slider { width: 280px; }
 .stepper-btn { width: 28px; height: 28px; border:1px solid #ccc; background:#f7f7f7; border-radius:6px; cursor:pointer; line-height:0; font-size:18px; }
 .stepper-btn:active { transform: translateY(1px); }
 .val { font-variant-numeric: tabular-nums; width: 60px; display:inline-block; text-align:right; }

 /* Two columns so Y and Lorenz sit side-by-side; stack to one column on narrow screens */
 .grid { display:grid; grid-template-columns: repeat(2, minmax(360px, 1fr)); gap: 16px; width: min(1200px, 100%); }
 @media (max-width: 850px){ .grid { grid-template-columns: 1fr; } }

 figure { margin:0; }
 figcaption { margin-top:6px; font-size: 0.95rem; color: #444; }
 img.plot { width:100%; border:1px solid #ddd; border-radius:8px; box-shadow:0 1px 4px rgba(0,0,0,.06); }
 .missing { padding: 8px 12px; border-radius: 6px; display:none; background:#ffe6e6; border:1px solid #ffb3b3; }
</style>
</head>

<body>
  <h2>Income, Perceptions & Lorenz (by Income Rank)</h2>

  <div class="row">
    <!-- Left column controls -->
    <div class="controls">
      <div class="control-row">
        <label>ρ</label>
        <select id="rho"></select>
      </div>

      <div class="control-row">
        <label>Gini</label>
        <button id="gMinus" class="stepper-btn" type="button" aria-label="Decrease gini">−</button>
        <input id="gini" class="slider" type="range" min="0" max="55" step="1" value="%G0%">
        <button id="gPlus"  class="stepper-btn" type="button" aria-label="Increase gini">+</button>
        <span class="val" id="giniVal"></span>
      </div>
    </div>

    <!-- Right column: 5 images; Y and Lorenz are first row -->
    <div style="flex:1; min-width: 360px;">
      <div class="missing" id="missing">One or more images for this (ρ, gini) are missing.</div>
      <div class="grid" id="grid">
        <figure>
          <img id="imgY"   class="plot" alt="Y vs rank"                  src="" loading="lazy" />
          <figcaption>Y</figcaption>
        </figure>
        <figure>
          <img id="imgLZ"  class="plot" alt="Lorenz curve"               src="" loading="lazy" />
          <figcaption>Lorenz curve</figcaption>
        </figure>
        <figure>
          <img id="imgME"  class="plot" alt="Perception of mean vs rank" src="" loading="lazy" />
          <figcaption>Perception of mean</figcaption>
        </figure>
        <figure>
          <img id="imgMAX" class="plot" alt="Perception of max vs rank"  src="" loading="lazy" />
          <figcaption>Perception of max</figcaption>
        </figure>
        <figure>
          <img id="imgMIN" class="plot" alt="Perception of min vs rank"  src="" loading="lazy" />
          <figcaption>Perception of min</figcaption>
        </figure>
      </div>
    </div>
  </div>

<script>
/* === EDIT THIS LIST MANUALLY === */
const RHO_OPTIONS = [
  { label: "ρ = 0", folder: "plotsrho0" },
  { label: "ρ = 4", folder: "plotsrho4" },
  // { label: "ρ = 5", folder: "plotsrho5" },
];

const EXT = "%EXT%";  // e.g., ".png"

/* DOM refs */
const rhoSel  = document.getElementById('rho');
const gini    = document.getElementById('gini');
const giniVal = document.getElementById('giniVal');
const missing = document.getElementById('missing');

const imgY   = document.getElementById('imgY');
const imgLZ  = document.getElementById('imgLZ');
const imgME  = document.getElementById('imgME');
const imgMAX = document.getElementById('imgMAX');
const imgMIN = document.getElementById('imgMIN');

const gMinus = document.getElementById('gMinus');
const gPlus  = document.getElementById('gPlus');

/* Helpers */
function pad3(n){ return n.toString().padStart(3,'0'); }
function setGiniValue(g){ g = Math.max(0, Math.min(100, g)); gini.value = g; giniVal.textContent = (g/100).toFixed(2); }

/* Populate rho dropdown */
(function initRho(){
  rhoSel.innerHTML = "";
  for (const opt of RHO_OPTIONS){
    const o = document.createElement('option');
    o.value = opt.folder;
    o.textContent = opt.label;
    rhoSel.appendChild(o);
  }
  if (RHO_OPTIONS.length) rhoSel.value = RHO_OPTIONS[0].folder;
})();

/* Update all images for current (ρ, gini) */
function updateImages(){
  const folder = rhoSel.value;
  const key = pad3(parseInt(gini.value,10));

  const paths = {
    Y:   folder + "/g" + key + "_Y"      + EXT,
    LZ:  folder + "/g" + key + "_LORENZ" + EXT,
    ME:  folder + "/g" + key + "_MEAN"   + EXT,
    MAX: folder + "/g" + key + "_MAX"    + EXT,
    MIN: folder + "/g" + key + "_MIN"    + EXT
  };

  function setImg(el, src){
    el.onerror = () => { el.style.display = 'none'; missing.style.display = 'block'; };
    el.onload  = () => { el.style.display = 'block'; };
    el.src = src;
  }

  missing.style.display = 'none';
  setImg(imgY,   paths.Y);
  setImg(imgLZ,  paths.LZ);
  setImg(imgME,  paths.ME);
  setImg(imgMAX, paths.MAX);
  setImg(imgMIN, paths.MIN);
}

/* Events */
gini.addEventListener('input', () => { setGiniValue(parseInt(gini.value,10)); updateImages(); });
rhoSel.addEventListener('change', updateImages);
gMinus.addEventListener('click', () => { setGiniValue(parseInt(gini.value,10) - 1); updateImages(); });
gPlus .addEventListener('click', () => { setGiniValue(parseInt(gini.value,10) + 1); updateImages(); });

/* init */
setGiniValue(%G0%);
updateImages();
</script>

</body></html>
"""


# ---- Write it out ----
html_out = replace(html, "%EXT%" => FILE_EXT, "%G0%" => g0_key)

open(joinpath(working_directory, "indvidual-perceptions_gini.html"), "w") do io
    write(io, html_out)
end




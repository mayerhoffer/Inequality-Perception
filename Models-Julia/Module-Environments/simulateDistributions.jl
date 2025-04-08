using .InputDistributions

using StatsBase, Combinatorics, Distributions, StatsPlots, DataFrames, CSV

exb = Exponential(1)
ywork = rand(exb, 9500) 

ywork = ywork / mean(ywork)
maximum(ywork)
paredo = Pareto(1.2, 1) # maximum(ywork))
ycap = rand(paredo,500)
yall = vcat(ywork,ycap)
yall = yall / mean(yall)
yall = yall * 2430
yall = round.(Int,yall)
histogram(yall, bins=200, ylabel="", title="Income distribution", legend=false)
maximum(ycap)
real_gini!(yall)



sigma = sqrt(2) * quantile(Normal(), 0.75)  # ≈ 0.954
ln = LogNormal(0, sigma)

mode_val = exp(0 - sigma^2)

lognommal = rand(ln, 9500) .- mode_val
lognommal = lognommal / mean(lognommal)
lognommal
 
mean(lognommal)
minimum(lognommal)
maximum(lognommal)

paredo = Pareto(1.05, 1) #maximum(lognommal))
wcap = rand(paredo,500)
wealth = vcat(lognommal,wcap)
wealth = wealth / mean(wealth)
real_gini!(wealth)




wealth = wealth * (316.5/ sqrt(2.03))
wealth = round.(Int, wealth)

histogram(wealth, bins=200, ylabel="", title="Wealth distribution", legend=false)


df = DataFrame(income = ysave, Wealth = wealthsave)
df = DataFrame(income = yall, Wealth = wealth)
CSV.write("C:\\Users\\dmayerh\\Onedrive - Personal\\OneDrive\\MODUS\\simulatedincomeandwealth.csv",df)

wealthsave=wealth
ysave = yall
real_gini!(wealth)
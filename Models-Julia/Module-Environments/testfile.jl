using .InputDistributions, .HomophilicNetworks, .IndividualPerceptions

incomedistri = income_onegroup_exponential!(1000)

linklist, linksPerNodeList = homophilic_linkage!(incomedistri,8)

visiblenodelist = find_visible_nodes!(incomedistri,linksPerNodeList)

perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty = distri_perceptions!(visiblenodelist)

mean(perceptionlist_meanproperty)






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
using .InputDistributions, .HomophilicNetworks, .IndividualPerceptions

incomedistri = income_onegroup_exponential!(1000)

linklist, linksPerNodeList = homophilic_linkage!(incomedistri,8)

visiblenodelist = find_visible_nodes!(incomedistri,linksPerNodeList)

perceptionlist_meanproperty, perceptionlist_maxproperty, perceptionlist_minproperty, perceptionlist_stdproperty = distri_perceptions!(visiblenodelist)

mean(perceptionlist_meanproperty)
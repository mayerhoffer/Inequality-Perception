;; ====================================================== DECLARATIONS ======================================================

extensions [ Rnd stats NW] ;; for roulette wheel selection / for network generation

breed [privilegeds privileged]
breed [underprivilegeds underprivileged]

undirected-link-breed [templinks templink]
undirected-link-breed [permalinks permalink]

globals [
;; All lists ordered by turtle's who number
;; --------------------------------------- Input and Network Generation
  distribution-list                     ;; income distribution - read from file (first value)
  distribution-list-length              ;; length of the input distribution list: determines number of turtles
  positions-list                        ;; income position (e.g. decile) read from file (second value if switch "pos?" is enabled)
  maxincome                             ;; maximum income  of turtles
  meanincome                            ;; avg income of turtles
  used-random-seed                      ;; random seed used - to allow reproducing the results
  maxLinks                              ;; maximum link count of a single turtle

  perceptionLinks                       ;; Gini Perception
  perceptionLinksSD

;; --------------------------------------- Baseline Model: Perceptions
  perceptionNodesList                   ;; list of agents' gini percepions made of myperceptionNodesBetweenNeighbours
  perceptionNodesBetweenNeighboursList  ;; list of agents' gini percepions including the differences between linkneighbors
  perceptionNodes                       ;; mean of individual gini perceptions
  perceptionNodesSD                     ;; standard-deviation of individual gini perceptions

  richerneighborslist                   ;;
  perceptionricherneighbors             ;;

  degreelist                            ;;
  incomelist                            ;;
  localmeanlist                         ;;
  localmeanmultipleselflist             ;;
  realdecilelist                        ;;

;; --------------------------------------- Baseline Model: Network Analysis
  E-I-index
  Priviledge-E-I-index
  interdecilelinkslist                  ;;
  diameter
  avgShortestPath
  globalClusteringCoefficient
  assortativityDegree
  assortativityIncome
  assortativityRank                     ;;

;; --------------------------------------- Consumption Model
  consumptionlist                       ;; list of agent's consumption
  meanconsumption
  sdconsumption

;; --------------------------------------- Wage Gap Model
  g-perceptionList-underpriv            ;; list of underprivileged agents' wage gap perceptions
  g-perceptionList-priv                 ;; list of privileged agents' wage gap perceptions
  intergenderproportionList-priv
  intergenderproportionList-underpriv
  totaldecileList-priv
  totaldecileList-underpriv
  lowestprivincome
  richestneighborbreed-priv
  richestneighborbreed-underpriv        ;;
  mean-privList-priv                    ;;
  mean-underprivList-priv               ;;
  mean-privList-underpriv               ;;
  mean-underprivList-underpriv          ;;
  incomeassortativity                   ;;
  incomeassortativitySingle             ;;
]

turtles-own [
  income                                ;; income (externally given)
  pos                                   ;; income decile (externally given)
  ingrouppos                            ;; decile in privilege group
  neighborlinks
  linkcount                             ;; number of link-neighbors
  myperception                          ;; own Gini perception without differences between linkneighbors (for internal validation only)
  myperceptionNodesBetweenNeighbours    ;; own Gini perception including the differences between linkneighbors
  onpath?

  clusteringCoefficient                 ;; local clustering coefficient

  consumption
  idiosyncraticconsumption
  my-g-perception                       ;; individual wage gap perception in the ego network

  myneighborsmeanincome
]

links-own [
  incomeend1
  incomeend2
  incomeDifference
  interdecile?
  interpriviledge?
]

;; ====================================================== MAIN CALLING PROCEDURE ======================================================
to RUN-ALL
  SETUP
  ask turtles [
    set linkcount count my-permalinks
  ]
  CALCULATE-GINI
  if assess-consumption? [ CALCULATE-CONSUMPTION ]
  if assess-wagegap? [ CALCULATE-WAGE-GAP ]
  if vizualise? [ ask turtles [ COLORIZE ] ]
  if analyse-network? [ ANALYSE-NETWORK ]
  reset-ticks
end  ;; RUN-ALL

;; ====================================================== SETUP PROCEDURES ======================================================
to SETUP
  clear-all
  file-close
  SETUP-SEED
  INITIALISE-DISTRIBUTION
  SETUP-NETWORK
end ;; SETUP


to SETUP-SEED  ;; Initializes predefined random seed or saves and shows one determined by NetLogo.
  set used-random-seed ifelse-value (randomSeed?) [ randomSeed ] [ new-seed ]
  random-seed used-random-seed
  output-print used-random-seed
end ;; SETUP-SEED


to INITIALISE-DISTRIBUTION  ;; Reads an externally defined list of random numbers.
  ifelse assess-wagegap? [ INITIALISE-DISTRIBUTION-PRIVILEGE ] [INITIALISE-DISTRIBUTION-STANDARD ]
end ;; INITIALISE-DISTRIBUTION


;; Reads an externally defined list of random numbers for the case of only one privilege class
to INITIALISE-DISTRIBUTION-STANDARD
  set distribution-list []
  set positions-list []
  file-open distributionFile
  ifelse fileLineNumber = 0 [
    ;; File consists of a single list that also determines the number of agents.
    ;; For each agent, its income is given as float first and its decile - if pos? is set true - as integer thereafter.
    ;; Individual values in the file must be separated by whitespace or line break. The file must not contain any leading identifiers or the like (unless this code is modified to ignore them).
    ;; See appended files for an example.
    while [not file-at-end?] [
      set distribution-list lput (file-read) distribution-list
      if pos? [ set positions-list lput (file-read) positions-list ]
    ]
  ]
  [
    ;; File contains multiple lines; each line is one distributions, identifiable for humans= by a single leading value (if this is changed, the dump code below must change, too)
    ;; Population is fixed 1000
    ;; For each agent, its income is given as float first and its decile - if pos? is set true - as integer thereafter.
    ;; Individual values within a distributiom must be separated by whitespaces only. The file must not contain any leading identifiers or the like (unless this code is modified to ignore them).
    ;; See appended files for an example.
    let k 0
    repeat fileLineNumber - 1 [set k file-read-line ]    ;; reads through lines before the one of interest and dumps them
    repeat 1 [ set k file-read ]                         ;; reads the identifier at start of line and dumps it
    repeat 1000 [
      set distribution-list lput (file-read) distribution-list
      if pos? [ set positions-list lput (file-read) positions-list ]
    ]
  ]
  set distribution-list-length (length distribution-list)
  file-close
  set maxincome (max distribution-list)
  set meanincome (mean distribution-list)
end ;; INITIALISE-DISTRIBUTION-SANDARD

;; Reads an externally defined list of random numbers for the comparison between privileged and underprivileged agents
;; Works similar to the standard case and only splits the population between privileged and underprivileged agents
;; Warning: The variable numprivileged must fit the number of privileged incomes in te input file!
to INITIALISE-DISTRIBUTION-PRIVILEGE
  set distribution-list []
  set positions-list []
  file-open distributionFile
  ifelse fileLineNumber = 0 [
    repeat numprivileged [
      set distribution-list lput precision ( file-read ) 5 distribution-list
      if pos? [ set positions-list lput (file-read) positions-list ]
    ]
    set lowestprivincome item 0 distribution-list
    repeat 1000 - numprivileged [
      set distribution-list lput precision ( file-read * (1 - g) ) 5 distribution-list
      if pos? [ set positions-list lput (file-read) positions-list ]
    ]
  ]
  [
    let k 0
    repeat fileLineNumber - 1 [set k file-read-line ]
    repeat 1 [ set k file-read ]
    repeat numprivileged [
      set distribution-list lput precision ( file-read ) 5 distribution-list
      if pos? [ set positions-list lput (file-read) positions-list ]
    ]
    set lowestprivincome item 0 distribution-list
    repeat 1000 - numprivileged [
      set distribution-list lput precision ( file-read * (1 - g) ) 5 distribution-list
      if pos? [ set positions-list lput (file-read) positions-list ]
    ]
  ]
;  show distribution-list  ;; for debugging
  set distribution-list-length (length distribution-list)
  file-close
  set maxincome (max distribution-list)
  set meanincome (mean distribution-list)
end ;; INITIALISE-DISTRIBUTION-PRIVILEGE


;; Calls the initialisation of turtles and creates their network
to SETUP-NETWORK
  ifelse assess-wagegap? [ INITIALISE-TURTLES-PRIVILEGE ] [INITIALISE-TURTLES-STANDARD ]
;  INITIALISE-DISTRIBUTION
  set maxlinks 1
  let myincome 0
  ask turtles [
    set myincome income
    let differencelist []
    let attachto rnd:weighted-n-of newLinks (other turtles) [1 / (e ^ (homophilystrength * abs (income - myincome) )) ]       ;; draws links weighted according to income distance and homophily
;; allows rebuilding the rnd:weighted-n-of function without an extension (e.g. for NetLogo-Web/.html versions)
;    foreach distribution-list [ x -> if (x != myincome) [set differencelist (fput  (e ^ (homophilystrength * abs (x - myincome) ) ) differencelist)] ]
;    let attachto n-of newLinks other turtles with [not permalink-neighbor? myself ]
    ask attachto [
      create-permalink-with myself
      set linkcount ( linkcount + 1 )
    ]
    set linkcount (count my-links)
  ]
  set maxlinks (max [linkcount] of turtles)
  ask links [
    CALCULATE-INCOME-DIFFERENCES
  ]
end ;; SETUP-NETWORK-PA


;; Initialises turtles from the pre-read distribution list.
to INITIALISE-TURTLES-STANDARD
  repeat distribution-list-length [
    create-turtles 1 [
      set income item 0 distribution-list
      set distribution-list (remove-item 0 distribution-list)
      set shape "dot"
     ; set test 1
      if pos? [
        set pos item 0 positions-list
        set positions-list (remove-item 0 positions-list)
      ]
    ]
  ]
end  ;; INITIALISE-TURTLES-STANDARD


;; Initialises turtles from the pre-read distribution list (two privilege classes).
to INITIALISE-TURTLES-PRIVILEGE
  repeat numprivileged [
    create-privilegeds 1 [
      set income item 0 distribution-list
      set distribution-list (remove-item 0 distribution-list)
      set shape "dot"
     ; set test 1
      if pos? [
        set ingrouppos item 0 positions-list
        set positions-list (remove-item 0 positions-list)
      ]
    ]
  ]
  repeat 1000 - numprivileged [
    create-underprivilegeds 1 [
      set income item 0 distribution-list
      set distribution-list (remove-item 0 distribution-list)
      set shape "dot"
     ; set test 1
      if pos? [
        set ingrouppos item 0 positions-list
        set positions-list (remove-item 0 positions-list)
      ]
    ]
  ]
  let positionassessment (sort-on [income] turtles)
  let i 0
  let currentdecile 0
  let decilesize (distribution-list-length / 10)
  repeat 10 [
    repeat decilesize [
      ask item (currentdecile * decilesize + i) positionassessment [ set pos currentdecile + 1]
      set i (i + 1)
    ]
    set currentdecile (currentdecile + 1)
    set i 0
  ]
end  ;; INITIALISE-TURTLES-PRIVILEGE


;; Lets a link calculate the income differences between its ends
to CALCULATE-INCOME-DIFFERENCES
  set incomeend1 [income] of end1
  set incomeend2 [income] of end2
  set incomeDifference abs (incomeend1 - incomeend2 )
  if pos? [
    set interdecile? ifelse-value ([pos] of end1 = [pos] of end2) [false] [true]
  ]
  if assess-wagegap? [
    set interpriviledge? ifelse-value ([breed] of end1 = [breed] of end2) [false] [true]
  ]

end  ;; CALCULATE-INCOME-DIFFERENCES


;; ====================================================== BASELINE INEQUALITY PERCEPTION ======================================================
to CALCULATE-GINI
  CALCULATE-NODEBASED
  CALCULATE-LINKBASED   ;; for validation and as preparation for network analysis only
end  ;; CALCULATE-GINI

;; Calculates Gini perception as mean of nodes (turtles)
to CALCULATE-NODEBASED
  set perceptionNodesList []
  set perceptionNodesBetweenNeighboursList []
  set degreelist []
  set richerneighborslist []
  set incomelist []
  set localmeanlist []
  set localmeanmultipleselflist []
  set realdecilelist []
  set interdecilelinkslist []
  let i 0
  repeat distribution-list-length [
    ask turtle i [
      set myperception (mean [incomeDifference] of my-links / 2)
      let perceivedgroup link-neighbors
      ask perceivedgroup [
        create-templinks-with other perceivedgroup [CALCULATE-INCOME-DIFFERENCES]                     ;; creates temporary links for all pairwise comparisons between neighbors
      ]
      set myperceptionNodesBetweenNeighbours ( (mean sentence [incomeDifference] of templinks [incomeDifference] of my-links ) / 2)            ;; This is the main perception. All other values for validation only
      set perceptionNodesBetweenNeighboursList lput myperceptionNodesBetweenNeighbours perceptionNodesBetweenNeighboursList
      set myneighborsmeanincome mean [income] of link-neighbors
      set degreelist lput linkcount degreelist
      set richerneighborslist lput (count link-neighbors with [income > [income] of myself] / count link-neighbors) richerneighborslist        ;; Adds the agent's perceived decile to the list of perceived deciles
      set incomelist lput income incomelist
      let localmean ( (sum [income] of link-neighbors + [income] of self) / ( linkcount + 1) )
      set localmeanlist lput localmean localmeanlist
      let localmeanmultipleself ( ( sum [income] of link-neighbors + linkcount * income ) / (linkcount * 2) )
      set localmeanmultipleselflist lput localmeanmultipleself localmeanmultipleselflist
      set realdecilelist lput pos realdecilelist
      if pos? [set interdecilelinkslist lput (count my-links with [interdecile?] / count my-links) interdecilelinkslist]                       ;; For network/segregation evaluation
      set myperception (myperception / localmeanmultipleself)
      set myperceptionNodesBetweenNeighbours (myperceptionNodesBetweenNeighbours / localmean)                                                  ;; Divide by mean for actual Gini perception
      set perceptionNodesList lput myperceptionNodesBetweenNeighbours perceptionNodesList
    ]
    ask templinks [die]
    set i (i + 1)
  ]
  set perceptionricherneighbors (mean richerneighborslist)
  set perceptionNodes (mean perceptionNodesList)
  set perceptionNodesSD (standard-deviation perceptionNodesList)
end  ;; CALCULATE-NODEBASED

;; Calculates Gini perception as mean of links - for comparison/validation and network analysis only
to CALCULATE-LINKBASED
  set perceptionLinks ( (mean [incomeDifference] of links) / 2)
  set perceptionLinksSD ( (standard-deviation [incomeDifference] of links) / 2)
  if pos? [
    let intergrouplinks count links with [interdecile?]
    let totallinks count links
    set E-I-index ( ( intergrouplinks - ( totallinks - intergrouplinks) ) / totallinks )
  ]
end  ;; CALCULATE-LINKBASED

;; ====================================================== BASELINE NETWORK ANALYSIS ======================================================
;; Calculates standard network parameters using the nw-extension
to ANALYSE-NETWORK
;; network features calculated from nodes
  let pop (count turtles - 1)
  let i 0
  let shortestpathslist (list)
  let assortativityIncomeTheoreticalList (list)
  ask turtles [
  set neighborlinks (mean [linkcount] of permalink-neighbors)
    set clusteringCoefficient nw:clustering-coefficient
    set i who
    repeat newlinks [
      set assortativityIncomeTheoreticalList lput (list [income] of one-of other turtles [income] of self) assortativityIncomeTheoreticalList
    ]
    repeat pop - i [
      set i (i + 1)
      set shortestpathslist lput nw:distance-to turtle i shortestpathslist
    ]
  ]
  ifelse not member? false shortestpathslist [
    set avgShortestPath (mean shortestpathslist)
    set diameter (max shortestpathslist)
  ] [
    set avgShortestPath false
    set diameter false
  ]
;; network features calculated from links
  let assortativityDegreeList (list)
  let assortativityIncomeList (list)
  let assortativityRankList (list)
  ask permalinks [
    set assortativityDegreeList lput (list [linkcount] of end1 [linkcount] of end2) assortativityDegreeList
    set assortativityIncomeList lput shuffle (list [income] of end1 [income] of end2) assortativityIncomeList
    set assortativityRankList lput (list [who] of end1 [who] of end2) assortativityRankList
  ]
  set globalClusteringCoefficient global-clustering-coefficient
  set assortativityDegree item 0 item 1 (stats:correlation (stats:newtable-from-row-list assortativityDegreeList))
  set assortativityIncome  item 0 item 1 (stats:correlation (stats:newtable-from-row-list assortativityIncomeList))
  set assortativityRank item 0 item 1 (stats:correlation (stats:newtable-from-row-list assortativityRankList))
end ;;ANALYSE-NETWORK

to-report global-clustering-coefficient
  let closed-triplets sum [ clusteringCoefficient * linkcount * (linkcount - 1) ] of turtles
  let triplets sum [ count my-links * (linkcount - 1) ] of turtles
  report closed-triplets / triplets
end


;; ====================================================== CONSUMPTION MODEL ======================================================
;; Calculates idiosyncratic and status consumption
to CALCULATE-CONSUMPTION
  ask turtles [
    set idiosyncraticconsumption w * b * income
  ]
  set consumptionlist (list)
  let i (distribution-list-length - 1) ;; 0 for ascending order and orientation only on richest link neighbor
  repeat distribution-list-length [
    ask turtle i [
      let orientation
      max [consumption] of permalink-neighbors
      set consumption ( idiosyncraticconsumption + (1 - w) *  c * max (list (orientation - idiosyncraticconsumption) 0) )
      set consumptionlist (fput consumption consumptionlist)
    ]
    set i (i - 1)  ;; + 1 for ascending order and orientation only on richest link neighbor
  ]
  set meanconsumption mean consumptionlist
  set sdconsumption standard-deviation consumptionlist
end  ;; CALCULATE-CONSUMPTION

;; ====================================================== WAGE GAP PERCEPTION MODEL ======================================================
;; Assesses the wage gap and takes basic network meassures (similar to the baseline case) specific for each privilege group.
to CALCULATE-WAGE-GAP
  set g-perceptionList-priv []
  set g-perceptionList-underpriv []
  set intergenderproportionList-priv []
  set intergenderproportionList-underpriv []
  set richestneighborbreed-priv []
  set richestneighborbreed-underpriv []
  set totaldecileList-priv []
  set totaldecileList-underpriv []
  let income-privileged-neighbors []
  let income-underprivileged-neighbors []
  set mean-privList-priv []
  set mean-underprivList-priv []
  set mean-privList-underpriv []
  set mean-underprivList-underpriv []
  let i 0
  repeat numprivileged [
  ask privileged i [
      set my-g-perception "NA"
      set income-privileged-neighbors (lput income income-privileged-neighbors)
    ask permalink-neighbors [
      ifelse is-privileged? self [
        set income-privileged-neighbors (lput income income-privileged-neighbors)
      ] [
        set income-underprivileged-neighbors (lput income income-underprivileged-neighbors)
      ]
    ]
      let my-mean-priv "NA"
      let my-mean-underpriv "NA"
      let myrichestneighbor "NA"
    if (length income-underprivileged-neighbors > 0 and length income-privileged-neighbors > 0) [
        set my-mean-priv (mean income-privileged-neighbors)
        set my-mean-underpriv (mean income-underprivileged-neighbors)
        set my-g-perception (my-mean-priv - my-mean-underpriv)
        set myrichestneighbor ifelse-value (max income-privileged-neighbors > max income-underprivileged-neighbors) ["privileged"] ["underprivileged"]
    ]
    set g-perceptionList-priv lput my-g-perception g-perceptionList-priv
    set mean-privList-priv lput my-mean-priv  mean-privList-priv
    set mean-underprivList-priv lput my-mean-underpriv  mean-underprivList-priv
      set intergenderproportionList-priv lput (count my-permalinks with [interpriviledge?] / count my-permalinks ) intergenderproportionList-priv
      set richestneighborbreed-priv lput myrichestneighbor richestneighborbreed-priv
  ]
    set income-privileged-neighbors []
    set income-underprivileged-neighbors []
  set i (i + 1)
  ]
  repeat distribution-list-length - numprivileged [
  ask underprivileged i [
    set my-g-perception "NA"
    set income-underprivileged-neighbors (lput income income-underprivileged-neighbors)
    ask permalink-neighbors [
      ifelse is-privileged? self [
        set income-privileged-neighbors (lput income income-privileged-neighbors)
      ] [
        set income-underprivileged-neighbors (lput income income-underprivileged-neighbors)
      ]
    ]
      let my-mean-priv "NA"
      let my-mean-underpriv "NA"
      let myrichestneighbor "NA"
    if (length income-underprivileged-neighbors > 0 and length income-privileged-neighbors > 0) [
        set my-mean-priv (mean income-privileged-neighbors)
        set my-mean-underpriv (mean income-underprivileged-neighbors)
        set my-g-perception (my-mean-priv - my-mean-underpriv)
;      set my-g-perception precision (1 - (mean income-underprivileged-neighbors / mean income-privileged-neighbors) ) 3
        set my-g-perception ((mean income-privileged-neighbors) - (mean income-underprivileged-neighbors))
;      set my-g-perception (abs (mean income-privileged-neighbors - mean income-underprivileged-neighbors) / mean income-privileged-neighbors) ;mean income-all-neighbors )
;        if mean income-privileged-neighbors < mean income-underprivileged-neighbors [set my-g-perception (my-g-perception * (- 1))]
        set myrichestneighbor ifelse-value (max income-privileged-neighbors > max income-underprivileged-neighbors) ["privileged"] ["underprivileged"]
    ]
    set g-perceptionList-underpriv lput my-g-perception g-perceptionList-underpriv
    set mean-privList-underpriv lput my-mean-priv  mean-privList-underpriv
    set mean-underprivList-underpriv lput my-mean-underpriv  mean-underprivList-underpriv
    set intergenderproportionList-underpriv lput (count my-permalinks with [interpriviledge?] / count my-permalinks ) intergenderproportionList-underpriv
    set richestneighborbreed-underpriv lput myrichestneighbor richestneighborbreed-underpriv
  ]
    set income-privileged-neighbors []
    set income-underprivileged-neighbors []
  set i (i + 1)
  ]
    if assess-wagegap?  [
    let intergrouplinks count links with [interpriviledge?]
    let totallinks count links
    set Priviledge-E-I-index ( ( intergrouplinks - ( totallinks - intergrouplinks) ) / totallinks )
  ]
end  ;; CALCULATE-WAGE-GAP

;; ====================================================== VISUALIZATION ======================================================
;; Colors the turtles according to their proportion of interdecilelinks and sets the x-position according to income rank and the y-position according to gini estimate
to COLORIZE
  let coloroption int ((count my-links with [interdecile?] / count my-links) * 10)
  set color (125 - coloroption * 10)
  setxy (who / 10) (min (list 100 (myperception * 100) ))
  set size 5
end ;; COLORIZE
@#$#@#$#@
GRAPHICS-WINDOW
1250
70
1744
565
-1
-1
4.812
1
10
1
1
1
0
0
0
1
0
100
0
100
0
0
1
ticks
30.0

BUTTON
925
10
1073
70
NIL
SETUP
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
930
500
1089
560
distributionFile
exp_israel.csv
1
0
String

SWITCH
933
332
1088
365
randomSeed?
randomSeed?
0
1
-1000

INPUTBOX
932
373
1087
433
randomSeed
100.0
1
0
Number

OUTPUT
930
441
1090
495
11

MONITOR
35
475
277
520
Perceived Gini as mean of links
perceptionLinks
10
1
11

SLIDER
45
95
220
128
newLinks
newLinks
1
10
5.0
1
1
NIL
HORIZONTAL

PLOT
430
10
919
325
log-log Degree
degree
number of nodes
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "let max-degree max [linkcount] of turtles\n;; for this plot, the axes are logarithmic, so we can't\n;; use \"histogram-from\"; we have to plot the points\n;; ourselves one at a time\nplot-pen-reset  ;; erase what we plotted before\n;; the way we create the network there is never a zero degree node,\n;; so start plotting at degree one\nlet degree 1\nwhile [degree <= max-degree] [\n  let matches turtles with [count link-neighbors = degree]\n  if any? matches\n    [ plotxy log degree 10\n             log (count matches) 10 ]\n  set degree degree + 1\n]"

BUTTON
925
75
1220
316
NIL
RUN-ALL
NIL
1
T
OBSERVER
NIL
G
NIL
NIL
1

SLIDER
45
165
220
198
homophilystrength
homophilystrength
0
50
12.0
0.5
1
NIL
HORIZONTAL

MONITOR
35
532
277
577
Perceived Gini as mean of node estimates
perceptionNodes
10
1
11

MONITOR
35
695
279
740
Perceived % of richer neighbors
perceptionricherneighbors
10
1
11

PLOT
430
330
920
646
Perceived Deciles
NIL
NIL
0.0
1.0
0.0
250.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "set-histogram-num-bars 10" "histogram richerneighborslist"

MONITOR
287
474
418
519
SD of links
perceptionLinksSD
10
1
11

MONITOR
287
533
420
578
SD of nodes
perceptionNodesSD
10
1
11

INPUTBOX
931
565
1091
625
fileLineNumber
0.0
1
0
Number

TEXTBOX
962
580
1112
598
If 0, the whole file is read
11
0.0
1

SWITCH
928
633
1091
666
pos?
pos?
1
1
-1000

MONITOR
290
280
425
325
NIL
E-I-index
10
1
11

TEXTBOX
55
75
205
93
PA style network
11
0.0
1

PLOT
430
655
920
970
Proportion Interdecilelinks
NIL
NIL
0.0
1.0
0.0
250.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "set-histogram-num-bars 20" "histogram interdecilelinkslist"

MONITOR
35
585
275
630
node Gini normalised
\"EDIT TO TURN ON\"\n;;mean [myperception] of turtles
10
1
11

MONITOR
35
640
275
685
node Gini compare between link-neighbours
\"EDIT TO TURN ON\"\n;;mean [myperceptionNodesBetweenNeighbours] of turtles
10
1
11

TEXTBOX
1485
570
1635
588
Income Rank
11
0.0
1

TEXTBOX
1235
220
1250
416
P\ne\nr\nc\ne\ni\nv\ne\nd\n\nG\ni\nn\ni
11
0.0
1

TEXTBOX
930
695
1080
713
Consumption
14
0.0
1

SLIDER
930
715
1110
748
w
w
0
1
0.35
0.01
1
NIL
HORIZONTAL

SLIDER
930
785
1110
818
c
c
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
930
750
1110
783
b
b
0
2
0.75
0.05
1
NIL
HORIZONTAL

SWITCH
930
855
1082
888
assess-wagegap?
assess-wagegap?
0
1
-1000

TEXTBOX
930
835
1080
853
Wage Gap
14
0.0
1

SLIDER
930
890
1102
923
g
g
0
1
0.0
0.01
1
NIL
HORIZONTAL

BUTTON
1075
10
1221
70
NIL
CALCULATE-GINI
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
35
815
280
860
perception privileged
median g-perceptionList-priv
5
1
11

MONITOR
35
765
280
810
perception underprivileged
median g-perceptionList-underpriv
2
1
11

MONITOR
35
880
280
925
NIL
incomeassortativitySingle
5
1
11

MONITOR
35
930
280
975
NIL
incomeassortativity
17
1
11

SLIDER
930
925
1102
958
numprivileged
numprivileged
0
1000
528.0
1
1
NIL
HORIZONTAL

SWITCH
1185
730
1357
763
assess-consumption?
assess-consumption?
1
1
-1000

SWITCH
1340
665
1492
698
analyse-network?
analyse-network?
1
1
-1000

SWITCH
1540
695
1647
728
vizualise?
vizualise?
1
1
-1000

@#$#@#$#@
# Model Outline
The model consists of three distinct phases run in sequential order:

1. Agent initialisation and income allocation
2. Network formation
3. Gini perception and network evaluation

Each phase runs only once and phases one and two build the structure which phase three then analyses. This implies that during network generation, agents adapt others‘ income level but there is no reaction to others‘ linking behaviour or perception and thus the model does not feature interaction in a narrowsense. Moreover, in the model, an agent‘s social contacts depend on their income. Wechoose this direction of causality for technical reasons and because it seems empirically likely (cf. section ). Nevertheless, our process scheduling would also be consistent with the opposite direction of causality, or positive feedback eects between income and social contacts.
The model is designed that way because it focusses entirely on income perception given defined income distributions and network structures. Hence, both an agent‘s income and their social contacts remain constant for the evaluated time frame or, put dierently, that the simulation outcome is a snapshot of a certain point in
time.

## Agent initialisation and income allocation
There are 1000 agents in the model and each agent draws their income from an exponential distribution with a mean of 1. Such a distribution could be understood as normalising the empirical observed income distributions in various industrialised countries, and thus, the model population represents a representative sample of empirical populations of these countries. We use the same pre-validated distribution for all Monte Carlo runs and also all levels of homophily. Doing so excludes any noise from the distribution draw from the simulation results. Agents also store their true income decile for evaluation purposes.

## Network formation
Each agent draws five other agents to link to. Thereby, agent j‘s weight in agent i‘s draw is denoted by w(i;j) and determined as follows: links to five other agents, with agent j's weight:

w(i;j) = 1 / e ^ (rho *  |I(i) - I(j)|)

I denotes the income of an agent and  denotes the homophily strength in income selection, externally set and identical for all agents. rho = 0 represents a random graph and for an increasing positive value of rho, an agent becomes ever more likely to pick link neighbours with incomes being closer to their own. The exponential character of theweighting function ensures that those others with are large income difference become unlikely picks even at low homophily strengths.

The resulting network is a member of the family of Random Geometric Graphs (Dall & Christensen 2002), which Talaga & Nowak (2020) showed to eiciently reproduce core features of many social networks. Specifically, we combine the notions of homophily (Boguná et al. 2004) with pre-setting node degrees (Newman et al. 2001; Newman 2009). However, with regard to our application, we are able to simplify both approaches by predetermination of only the global minimum degree and consequently defining relative weights rather than absolute probabilities.

Links are undirected and links have identical weights for evaluation purposes below. Agents pick their neighbours in random sequential order. If an agent i picks agent j who had themself picked i before that, the already existing link between the two agents remains untouched but i does not pick another neighbour instead of j. Consequently, each agent has at least 5 link-neighbours (i.e. social contacts) but may have more.

## Gini perception and network evaluation
Agents know about their own income and also their social contact‘s incomes. However, they do not possess knowledge about any other agent or the income distribution. Thus, agents judge income inequality in the population as well as their own income position solely based on themself and their link neighbours. Besides the agents‘ perceptions, there is a global assessment of various network parameters in order to validate the model. For their perception of overall inequality, an agent finds the mean of all income differences between themself and each link neighbour and between any two of their link neighbours. Then, the overall perceived Gini is simply the mean of individual perceptions.
To estimate their incomedecile, an agent compares the number of link-neighbours having a higher incomethan the agent themself to the link-neighbours having a lower income than the agent themself.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Consumption-Paraspace-rho0.5" repetitions="1" runMetricsEveryStep="false">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>consumptionlist</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <steppedValueSet variable="c" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp1_pos.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="w" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Consumption-Paraspace" repetitions="1" runMetricsEveryStep="false">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>consumptionlist</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp1_pos.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="w" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="homophilystrength" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>consumptionlist</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;lncons03.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="lognormallong_new" repetitions="1" runMetricsEveryStep="true">
    <setup>Run-ALL</setup>
    <go>stop</go>
    <metric>perceptionNodes</metric>
    <metric>meanconsumption</metric>
    <metric>sdconsumption</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fileLineNumber" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;lognormallong_new.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Consumption-Paraspace-Few" repetitions="1" runMetricsEveryStep="false">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>perceptionNodes</metric>
    <metric>meanconsumption</metric>
    <metric>sdconsumption</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="b">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="c" first="0.1" step="0.2" last="1"/>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp1_pos.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="w" first="0.1" step="0.2" last="1"/>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="0.5"/>
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Consumption-Paraspace-rho1" repetitions="1" runMetricsEveryStep="false">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>consumptionlist</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <steppedValueSet variable="c" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp1_pos.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="w" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Consumption-Paraspace-rho1.5" repetitions="1" runMetricsEveryStep="false">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>consumptionlist</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <steppedValueSet variable="c" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp1_pos.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="w" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Consumption-Paraspace-rho4" repetitions="1" runMetricsEveryStep="false">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>consumptionlist</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <steppedValueSet variable="c" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp1_pos.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="w" first="0.1" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Consumption-Paraspace-rho4_BUGFIX" repetitions="1" runMetricsEveryStep="false">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>consumptionlist</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp1_pos.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Network-Type">
      <value value="&quot;Preferential Attachment&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>g-perceptionList-underpriv</metric>
    <metric>g-perceptionList-priv</metric>
    <metric>intergenderproportionList-priv</metric>
    <metric>intergenderproportionList-underpriv</metric>
    <metric>Priviledge-E-I-Index</metric>
    <metric>richestneighborbreed-priv</metric>
    <metric>richestneighborbreed-underpriv</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assess-priviledge?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="g" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;listeisrael_single.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="homophilystrength" first="0" step="1" last="4"/>
  </experiment>
  <experiment name="rohdaten" repetitions="1" runMetricsEveryStep="true">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>g-perceptionList-underpriv</metric>
    <metric>g-perceptionList-priv</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assess-priviledge?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="g" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;liste500.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="homophilystrength" first="0" step="1" last="4"/>
  </experiment>
  <experiment name="WageGapAll" repetitions="1" runMetricsEveryStep="true">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>mean-privList-priv</metric>
    <metric>mean-underprivList-priv</metric>
    <metric>mean-privList-underpriv</metric>
    <metric>mean-underprivList-underpriv</metric>
    <metric>intergenderproportionList-priv</metric>
    <metric>intergenderproportionList-underpriv</metric>
    <metric>Priviledge-E-I-Index</metric>
    <metric>perceptionNodesList</metric>
    <metric>incomeList</metric>
    <metric>incomeassortativitySingle</metric>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assess-wagegap?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="g" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp_500-500.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="homophilystrength" first="8" step="1" last="15"/>
    <enumeratedValueSet variable="numprivileged">
      <value value="500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>mean-privList-priv</metric>
    <metric>mean-underprivList-priv</metric>
    <metric>mean-privList-underpriv</metric>
    <metric>mean-underprivList-underpriv</metric>
    <metric>intergenderproportionList-priv</metric>
    <metric>intergenderproportionList-underpriv</metric>
    <metric>Priviledge-E-I-Index</metric>
    <metric>richestneighborbreed-priv</metric>
    <metric>richestneighborbreed-underpriv</metric>
    <metric>perceptionNodesList</metric>
    <metric>incomeList</metric>
    <metric>incomeassortativitySingle</metric>
    <enumeratedValueSet variable="random-link-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="50" last="10000"/>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="g" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;liste500.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assess-wagegap?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="homophilystrength" first="0.5" step="0.5" last="4"/>
  </experiment>
  <experiment name="baseline-exponential-new" repetitions="1" runMetricsEveryStep="true">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>perceptionNodesList</metric>
    <metric>richerneighborslist</metric>
    <metric>interdecilelinkslist</metric>
    <metric>avgShortestPath</metric>
    <metric>globalClusteringCoefficient</metric>
    <metric>degreelist</metric>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;expSmooth_pos.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assess-wagegap?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="homophilystrength" first="5" step="1" last="15"/>
  </experiment>
  <experiment name="baseline-lognorm-new" repetitions="1" runMetricsEveryStep="true">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>mean perceptionNodesList</metric>
    <steppedValueSet variable="fileLineNumber" first="26" step="1" last="95"/>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;lognormallong_new.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assess-wagegap?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilystrength">
      <value value="1"/>
      <value value="4"/>
      <value value="8"/>
      <value value="14"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Israel_empirical" repetitions="1" runMetricsEveryStep="true">
    <setup>RUN-ALL</setup>
    <go>stop</go>
    <metric>mean-privList-priv</metric>
    <metric>mean-underprivList-priv</metric>
    <metric>mean-privList-underpriv</metric>
    <metric>mean-underprivList-underpriv</metric>
    <metric>intergenderproportionList-priv</metric>
    <metric>intergenderproportionList-underpriv</metric>
    <metric>Priviledge-E-I-Index</metric>
    <metric>perceptionNodesList</metric>
    <metric>incomeList</metric>
    <metric>incomeassortativitySingle</metric>
    <steppedValueSet variable="randomSeed" first="1" step="1" last="100"/>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fileLineNumber">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numprivileged">
      <value value="528"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pos?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distributionFile">
      <value value="&quot;exp_israel.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="w">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="b">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assess-wagegap?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newLinks">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophilyWeight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extremophilyWeight">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="homophilystrength" first="0" step="1" last="15"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@

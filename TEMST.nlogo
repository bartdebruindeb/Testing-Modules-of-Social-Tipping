extensions[nw stats]

globals [
  people-per-community
  stop-time
  lsp
  rewired
  Max-Green
  Tipped?
  majority-green?
  surface-area-green
  cum-surface-area-green
  norm-csag
  norm-csab
  sim-ended?
  tick-ended
  gcc
  MPL
  MCRPL
  count-people-updated-this-tick
  green-percentage
  max-cc-weight
  MSCe
  MSCl
  MCCe
  VCCe
  VEC
  VBC
  VCCo
  VD
  MEC
  MBC
  ratio-influencers-innovators
  influencers

]

breed[people person]
directed-link-breed [connections connection]
connections-own [
  community-bridge?
  complex-centrality-weight
  complex-connection-weight
  first-side?
]
people-own [
  peers
  my-behavior
  my-behavior-t-1
  ticks-to-new-decision
  my-community
  social-innovator?
  number-of-peers
  my-threshold
  Simple-Centrality
  Complex-Centrality
  Clus_Coefficient

]

;to profile
 ; profiler:reset                                       ;; set up the model
 ; profiler:start                                 ;; start profiling
 ; setup                               ;; run something you want to measure
 ; while [report-sim-ended = false] [go]
 ; profiler:stop
 ; print profiler:report          ;; stop profiling
  ;csv:to-file "profiler_data.csv" profiler:data  ;; save the results
                                 ;; clear the data;
;end

to setup
  clear-all
  reset-ticks
  if static-seed? [random-seed new-seed ] ;determine-seed]
  setup-people
  complex-path-weight
  setup-social-innovators
  setup-metrics
  update-plots
  ;random-seed new-seed


end

to-report determine-seed
  let dekkers-power-settings (list 1 3 5 7)
  let n-communities-settings (list 1 5 10)
  let rewiring-proportion-settings (list 0.01 0.05 0.1 0.2)
  let clustering-coefficient-settings (list 1 1.4 1.8 2.2)
  let reporter 1 + position dekkers-power dekkers-power-settings * 1 + position n-communities n-communities-settings * 4 + position rewiring-proportion rewiring-proportion-settings * 12 + position clustering-coefficient clustering-coefficient-settings * 64

  report reporter
end

to create-person
  set my-threshold threshold-pop ;normalize-data-in-range threshold-pop 0.15 0 1
      set size 3
      set color blue
      fd 10
end

to setup-people
  if network-generator = "Small-World" [
    nw:generate-watts-strogatz people connections population-size 6 rewiring-proportion [
      create-person ]
    stop
  ]
  if network-generator = "Kleinberg" [
    nw:generate-small-world people connections 29 29 clustering-coefficient true [
      create-person]
        ask connections [
      ;print end1
      ;print end2
      ask end1 [
        create-connection-from [end2] of myself
      ]

    ]
    stop
  ]
  if network-generator = "Scale-Free" [
    nw:generate-preferential-attachment people connections population-size 3 [
      create-person]
    ask connections [
      ask end1 [
        create-connection-from [end2] of myself
      ]
    ]
    stop
  ]




  set people-per-community population-size / n-communities
  if average-node-degree > people-per-community [error "average-node-degree is bigger than the people-per-community"]

    let index 1
  repeat n-communities [

    create-people people-per-community [
      create-person
      set my-community index
        ]
    set index index + 1
  ]
   setup-network
   if layout-network? [
    layout-network]
end


to set-default-settings
 ; set dekkers-power 4.5
  ;set seed 2
 ; set average-node-degree 4
  set module "stochastic"
  set seeding-strategy "Shotgun"
  set n-communities 4
  set rewiring-proportion 0.15
  set clustering-coefficient 2
;  set k1 0.000001
  set static-seed? false
 ; set k2 0.0000011
 ; set layout "spring"
 ; set social-innovators 0.10
  set sim-ended? false
 ; ask patch 37 43 [ output-print ticks]

end

to setup-network




  ; The social network is determined by the number of communities, number of members per community, clustering component, rewiring-proportion, dekkers-power.
  ; The network consists of n-communities. Firstly (Step 1) the agent with the highest label (max-who-community) of community n is connected with the agents with the median label of community n + 1.
  ; Afterwards (step 2) the connections are added by looping max-communities times over a procedure that is derived from the kleinburg version of the small world network
  ; (see https://www.nature.com/articles/35022643); Each iteration is started with one-of the agents with lowest connections. The probability of connecting agent i with another node j is
  ; proportonial to 1/distance(i,j)^q. Where distance d between agent i and j d = abs(who-i - who-j) for d < people-per-community / 2 and d = people-per-community - abs(who-i - who-j) for d > people-per-community / 2.
  ; Where q is the clustering component. For high values of q, agents make connections with more closely labeled nodes.
  ; Lastly the connections are rewired with random agents irrespectively of their community using the rewiring-proportion and dekkers-power. (See rewire procedure).

  let lwcc 2
  while [lwcc > 1] [
    ask connections [die]
    setup-communities-connections
    rewire
    set lwcc length nw:weak-component-clusters
  ]
end

to rewire
  ; ;Based on the rewiring-proportion and dekkers-power the network is rewired.

  ask connections with [first-side? = true] [
    ;Similarly as in Kawachi rewiring process the Links are rewired with probability p/3, where p is a rewiring parameter, and the rewiring is repeated three times, so that the probability of
    ;an edge being rewired is 1 – (1 – p/3)^3
    repeat 3 [
      if random-float 1 < (rewiring-proportion / 3) [
        let NodeA 0
        let NodeB 0
        ask connection [who] of end2 [who] of end1 [
              die]
      ifelse count ([out-connection-neighbors] of end1) > (count [out-connection-neighbors] of end2) [
        set NodeA end1
        set NodeB end2
      ][
        set NodeA end2
        set NodeB end1
      ]


        ask NodeA [
          ;First ensure nodeA to rewire with an isolated nodes
        let isolated-nodes other people with [count connection-neighbors = 0]
        ; If there are no isolated nodes, create connection with new node using 'new-rewired-node' reporter
        ifelse any? isolated-nodes [
          let nn1 one-of isolated-nodes
            create-connection-to nn1
            create-connection-from nn1
          ][
          let nn2 new-rewired-node NodeB
            create-connection-to nn2
            create-connection-from nn2
          ]
        ]
        set rewired rewired + 1
       ; Print NodeA
       ; print NodeB
        die
      ]
    ]
  ]

end

to-report new-rewired-node [NodeB]
  ; The new node for rewiring is determined by the extend kawachi rewiring from (Dekker, 2007, see https://www.academia.edu/download/30698577/RealisticSocial_s20_Dekker_.pdf). Rewiring
  ; is biased so as to preferentially move links to be adjacent to highly linked nodes (nodes of high degree). The higher the degree of a node, the higher the likelyhood of establishing
  ; an edge with this particular node.
  let agent-set-to-work-with other people who-are-not connection-neighbors
  ; Create an list of the unique node degree distribution of all people which are currently not connected to Node A including Node B
  let unique-degree-of-nodes remove-duplicates sort [count connection-neighbors] of other agent-set-to-work-with
  ;create a cumulative probability list in which the likelyhood of picking a node with degree d is determined by (d + 1) ^ dekkers-power * agents with that node-degree.
  let probability-list (list)
  foreach unique-degree-of-nodes [
    d ->
    let p count agent-set-to-work-with  with [count connection-neighbors = d]
      ifelse length probability-list > 0 [
        let v max probability-list + ((d + 1) ^ dekkers-power) * p
        set probability-list lput v probability-list
    ] [
        let v ((d + 1) ^ dekkers-power) * p
        set probability-list lput v probability-list]
  ]

  ; Pick a random-number and loop over the probability list to select the final node-degree
  let r-value random-float max probability-list
  let position-in-list 0
  let found-it? false
  foreach probability-list [
    x ->
    if r-value < x and found-it? = false [
      set position-in-list position x probability-list
      set found-it? true
    ]
  ]


  ;Pick randomly one of the agents as a new friends that have the selected node-degree

  let potential-friends agent-set-to-work-with  with [count connection-neighbors = item position-in-list unique-degree-of-nodes]
  report one-of potential-friends

end

to setup-communities-connections

  let max-iterations average-node-degree * population-size / 2
  let iterations-for-network-formation 0
  ; Step 1 Connect different communities with one another with by connecting the highest who of one community with the middle who of the other community.
;  if n-communities > 1 [
;    let index 1
;    let new-neighbor 0
;    repeat n-communities [
;      let max-who-community max [who] of people with [my-community = index]
;      ask person max-who-community [
;        ifelse who = max [who] of people [
;          set new-neighbor min [who] of people + ceiling (people-per-community / 2)
;        ][
;          set new-neighbor who + ceiling (people-per-community / 2) ]
;        create-connection-from person new-neighbor
;        create-connection-to person new-neighbor
;        ask connection who [who] of person new-neighbor [
;          set community-bridge? true]
;           ask connection [who] of person new-neighbor who [
;          set community-bridge? true]
;        set iterations-for-network-formation iterations-for-network-formation + 1
 ;     ]
 ;     set index index + 1
 ;   ]
 ; ]
  ;Step 2 Continue adding connections until the max connections is reached.
  while [max-iterations > iterations-for-network-formation] [
    ask min-one-of people [count connection-neighbors] [
      let community-list sort [who] of other people with [my-community = [my-community] of myself and not connection-neighbor? myself]
      let list-1 map [i -> abs(who - i)]  community-list
      let kleinburg-list (list)
      foreach list-1 [
        i -> if i > (people-per-community / 2) [
          set i people-per-community - i]
        set kleinburg-list lput i kleinburg-list
      ]
      set kleinburg-list map [i -> i ^ (clustering-coefficient * -1)] kleinburg-list
      let random-number random-float sum kleinburg-list
      let index-2 0
      while [random-number > item index-2 kleinburg-list] [
        set random-number random-number - item index-2 kleinburg-list
        set index-2 index-2 + 1
      ]
      let new-friend person item index-2 community-list
      create-connection-to new-friend
      create-connection-from new-friend
      ask connection who [who] of new-friend [set first-side? true]

      set iterations-for-network-formation iterations-for-network-formation + 1
    ]
  ]

end

to complex-path-weight
  ask connections [
   ; print [who] of end1
   ; print [who] of end2
    let N1 ([connection-neighbors] of end1) who-are-not end2
    let N2 ([connection-neighbors] of end2) who-are-not end1
    let Oij count N1 - count N1 who-are-not N2
   ; print [who] of N1
   ; print [who] of N2
   ; print Oij
    let Dij N2 who-are-not N1
    ;  print [who] of connection-neighbors
    let Rij count Dij with [count connection-neighbors - count connection-neighbors who-are-not N1 > 0]

   ; print Dij
    set complex-connection-weight 1 + Rij + Oij
    set complex-connection-weight 1 / complex-connection-weight]

  ask people [
    set Complex-Centrality nw:weighted-closeness-centrality complex-connection-weight
    set Simple-Centrality nw:closeness-centrality
  ]
end




to setup-social-innovators ;;; Assigns which agents are the social entrepreneurs (green) at the start of the simulation. Based on Centola (2018) you choose between Shotgun (assinging the
 ; the social-innovators randomly), Snowball (one random-agent and it's connected peers), Silver-Bullets (Select the agents with heighest node-degree). An additonal scenario has been
 ; added to instead of selecting the silver bullets (high degree nodes) the Outskirts (the low degree nodes).


  let n-social-entrepreneurs ceiling (social-innovators * population-size)


  if seeding-strategy = "Shotgun-NS" [
    ask n-of n-social-entrepreneurs  people with [count my-connections != max [count my-connections] of people] [
      setup-as-social-innovator]]

   if seeding-strategy = "Shotgun" [
    ask n-of n-social-entrepreneurs  people [
      setup-as-social-innovator]]

    if seeding-strategy = "Silver-Bullets" [
      ask max-n-of n-social-entrepreneurs people [count my-connections] [
      setup-as-social-innovator]]


  if seeding-strategy = "S_Closeness" [
    ; Ensure that agents that are connected to one another are also spatially closely located to another.
    let index n-social-entrepreneurs
    let k max-one-of people [Simple-Centrality]
      ask k [
        setup-as-social-innovator
        ask up-to-n-of index connection-neighbors [
          setup-as-social-innovator
        ]
        set index floor (social-innovators * population-size) - count people with [social-innovator? = true]
        while [index > 0] [
          ask max-one-of people with [social-innovator? != true] [(count connection-neighbors - (count connection-neighbors who-are-not  people with [social-innovator? = true])) / count connection-neighbors ] [
            setup-as-social-innovator]
            set index index - 1
          ]
    ]
  ]

   if seeding-strategy = "CR_Closeness" [
    ; Ensure that agents that are connected to one another are also spatially closely located to another.
    let index n-social-entrepreneurs
    let k max-one-of people [Complex-Centrality]
      ask k [
        setup-as-social-innovator
        ask up-to-n-of index connection-neighbors [
          setup-as-social-innovator
        ]
        set index floor (social-innovators * population-size) - count people with [social-innovator? = true]
        while [index > 0] [
          ask max-one-of people with [social-innovator? != true] [(count connection-neighbors - (count connection-neighbors who-are-not  people with [social-innovator? = true])) / count connection-neighbors ] [
            setup-as-social-innovator]
            set index index - 1
      ]
    ]
  ]

end

to setup-as-social-innovator
  set my-behavior-t-1 1
  set my-behavior 1
  set color green
  set size 5
  set social-innovator? true
end

to setup-metrics
  ask people [
    set peers connection-neighbors
    set number-of-peers count connection-neighbors
 ;   set Complex-Centrality nw:weighted-closeness-centrality connection-reinforcing-weight
 ;   set Simple-Centrality nw:closeness-centrality
    set Clus_Coefficient nw:clustering-coefficient
  ]
  set influencers people with-max [number-of-peers]
  set ratio-influencers-innovators count influencers with [social-innovator? = true] / count influencers
  set tipped? false
 ; set cum-surface-area-green calc-surface-area-green
 ; set norm-csag cum-surface-area-green
 ; set norm-csab 1 - norm-csag
  set sim-ended? false
  set tick-ended 0
  log-network-metrics
  nw:set-context (first nw:get-context) (last nw:get-context)
end

to log-network-metrics
  set gcc global-clustering-coefficient
  set MPL nw:mean-path-length
  set MCRPL nw:mean-weighted-path-length complex-connection-weight
;  set MSCe mean [Simple-Centrality] of people
;  set MCCe mean [Complex-Centrality] of people
  set MSCl mean [Clus_Coefficient] of people
;  set VCCe variance [nw:closeness-centrality] of people
 ; set VEC variance [nw:eigenvector-centrality] of people
 ; set VBC variance [nw:betweenness-centrality] of people
;  set VCCe variance [nw:weighted-closeness-centrality connection-reinforcing-weight] of people
  set VCCo variance [Clus_Coefficient] of people
  set VD variance [number-of-peers] of people
 ; set MEC mean [nw:eigenvector-centrality] of people
 ; set MBC mean [nw:betweenness-centrality] of people
end

to-report calc-surface-area-green
  report count people with [my-behavior = 1] / count people

end

to layout-network
  ask patches [set pcolor white]
   ask patch 37 43 [set plabel ticks set plabel-color black]
  if layout = "radial" and count turtles > 1 [
    let root-agent max-one-of turtles [ nw:closeness-centrality ]
    layout-radial turtles links root-agent
  ]
  if layout = "spring" [
    let factor people-per-community * 2
    if factor = 0 [ set factor 1 ]
    repeat 100 [layout-spring people connections 1 1 (1 / population-size) * 10000  ]
  ]
  if layout = "circle" [
    layout-circle sort turtles max-pxcor * 0.9
  ]
  if layout = "tutte" [
    layout-circle sort turtles max-pxcor * 0.9
    layout-tutte max-n-of (count turtles * 0.5) turtles [ count my-links ] links 12
  ]
  display
end

to-report network-assertiveness
  let giant-nodes max-n-of 20 people [count connection-neighbors]
  let number-of-giant-friends 0
  ask giant-nodes [
    set number-of-giant-friends count giant-nodes with [member? myself connection-neighbors]
  ]
  report mean [number-of-giant-friends] of giant-nodes
end

to go
  consider-adopting-new-behavior
  update-metrics
  tick-advance 1
  if behaviorspace-experiment-name = "" [ update-plots ]
end


to consider-adopting-new-behavior
  ifelse module = "stochastic" [
    ask people [
      ifelse my-behavior = 0 [
        if random-float 1 < (count peers with [my-behavior-t-1 = 1] + k1) / (number-of-peers + k2) [                   ; Where k1 < k2
          set my-behavior 1
          set color green
        ]
      ][
        if random-float 1 < (count peers with [my-behavior-t-1 = 0] + k1) / (number-of-peers + k2) [
          set my-behavior 0
          set color blue
        ]
      ]
    ]
  ][
  ask people [
    if my-behavior = 0 [
      if my-threshold < (count peers with [my-behavior-t-1 = 1] + k1) / (number-of-peers + k2) [
        set my-behavior 1
        set color green
        set count-people-updated-this-tick count-people-updated-this-tick +  1
      ]
    ]
  ]
]
ask people [
    set my-behavior-t-1 my-behavior
]
end



to update-metrics

  let g count people with [my-behavior = 1]
  let b count people with [my-behavior =  0]



  if count-people-updated-this-tick = 0 and module = "threshold-model" [
    set sim-ended? true
    set tick-ended ticks - 1
    ;log-network-metrics
    set green-percentage (g + 1) / (b + g + 1) - 1 / (b + g + 1)
  ]
  if b = 0 or g = 0 and sim-ended? = false [
    set sim-ended? true
    set tick-ended ticks
    ;log-network-metrics
    set green-percentage (g + 1) / (b + g + 1) - 1 / (b + g + 1)
  ]
  set count-people-updated-this-tick 0
end

to-report report-sim-ended
  ifelse sim-ended? = true and ticks > 0
  [report true]
  [report false]
end


;;; Reporters for Social Network Analysis




to-report global-clustering-coefficient
  let closed-triplets sum [ nw:clustering-coefficient * count my-connections * (count my-connections - 1) ] of people
  let triplets sum [ count my-connections * (count my-connections - 1) ] of people
  report closed-triplets / triplets
end


to-report clustering-of-greens
  let mean-path-to (list)
  ask people with [my-behavior = 1] [
    let path-to (list)
    ask other people with [my-behavior = 1] [
      set path-to lput length (nw:path-to myself) path-to
    ]
    set mean-path-to lput mean path-to mean-path-to
  ]
  report mean mean-path-to
end


to-report normalize-data-in-range [mean-data std-data low high]
  let x -1
  while [x < low or x > high] [
    set x precision (random-normal mean-data std-data) 3 ]
  report x
end
@#$#@#$#@
GRAPHICS-WINDOW
159
10
567
419
-1
-1
3.9604
1
10
1
1
1
0
0
0
1
-50
50
-50
50
1
1
1
ticks
30.0

BUTTON
2
16
65
49
setup
setup\n
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
1
51
67
84
go
go\nif report-sim-ended [stop]
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

CHOOSER
336
469
474
514
seeding-strategy
seeding-strategy
"Snowball" "Shotgun" "Shotgun-NS" "Silver-Bullets" "Outskirts" "Bridges" "CCC" "S_Closeness" "CD_Closeness" "CR_Closeness"
3

SLIDER
336
434
477
467
social-innovators
social-innovators
0
1
0.08
0.01
1
NIL
HORIZONTAL

MONITOR
638
491
769
536
#social entrepeneurs
count people with [ social-innovator? = true]
17
1
11

SLIDER
158
432
330
465
rewiring-proportion
rewiring-proportion
0
1
0.1
0.01
1
NIL
HORIZONTAL

CHOOSER
0
308
138
353
module
module
"stochastic" "threshold-model"
1

SLIDER
1
381
93
414
k1
k1
0
k2
1.0E-6
0.000001
1
NIL
HORIZONTAL

SLIDER
0
415
92
448
k2
k2
k1
0.00001
1.1E-6
0.0000001
1
NIL
HORIZONTAL

BUTTON
0
86
73
119
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
158
470
330
503
dekkers-power
dekkers-power
0
10
6.0
0.5
1
NIL
HORIZONTAL

TEXTBOX
4
362
154
380
Stochastic Module
10
0.0
1

TEXTBOX
0
283
150
302
Decision Modules
15
0.0
1

MONITOR
635
444
769
489
Average node degree
mean [count connections] of people / population-size
17
1
11

MONITOR
632
393
722
438
#connections
COUNT CONNECTIONS
17
1
11

MONITOR
631
348
688
393
NIL
rewired
17
1
11

MONITOR
788
440
862
485
NIL
Max-Green
17
1
11

MONITOR
793
497
850
542
NIL
Tipped?
17
1
11

SWITCH
0
152
116
185
static-seed?
static-seed?
0
1
-1000

INPUTBOX
0
185
155
245
seed
2.0
1
0
Number

SLIDER
157
507
329
540
n-communities
n-communities
1
7
5.0
1
1
NIL
HORIZONTAL

SLIDER
157
544
330
577
Population-Size
Population-Size
1
840
840.0
1
1
NIL
HORIZONTAL

MONITOR
724
394
826
439
NIL
count people
17
1
11

BUTTON
575
33
755
66
NIL
repeat 1 [layout-network]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
575
78
713
123
layout
layout
"spring" "circle" "tutte" "radial"
1

SLIDER
156
580
331
613
average-node-degree
average-node-degree
2
10
6.0
1
1
NIL
HORIZONTAL

BUTTON
0
249
133
282
NIL
set-default-settings
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
155
616
332
649
clustering-coefficient
clustering-coefficient
0
12
7.95
0.05
1
NIL
HORIZONTAL

BUTTON
0
121
89
154
Go N times
repeat N [go]
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
90
94
140
154
N
100.0
1
0
Number

MONITOR
922
256
1016
301
Surface-Green
precision norm-csag 5
17
1
11

MONITOR
915
300
1018
345
NIL
majority-green?
17
1
11

MONITOR
945
211
1017
256
NIL
tick-ended
17
1
11

SLIDER
672
240
844
273
duration-to-decision
duration-to-decision
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
674
274
846
307
threshold-pop
threshold-pop
0
1
0.48
0.01
1
NIL
HORIZONTAL

SWITCH
576
124
704
157
layout-network?
layout-network?
0
1
-1000

MONITOR
831
392
944
437
NIL
green-percentage
17
1
11

MONITOR
720
198
831
243
NIL
report-sim-ended
17
1
11

MONITOR
1120
256
1407
301
NIL
variance [number-of-peers] of people
17
1
11

PLOT
804
21
1004
171
plot 2
NIL
NIL
0.0
50.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [number-of-peers] of people"

CHOOSER
4
488
142
533
network-generator
network-generator
"Small-World" "Kleinberg" "Scale-Free" "custom"
3

MONITOR
669
582
726
627
NIL
MPL
17
1
11

MONITOR
638
536
695
581
NIL
MPL
17
1
11

MONITOR
723
549
780
594
NIL
GCC
17
1
11

PLOT
1131
137
1331
287
plot 1
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.05 1 -16777216 true "histogram [my-threshold] of people" "histogram [my-threshold] of people"

MONITOR
1065
424
1253
469
NIL
mean [my-threshold] of people
17
1
11

MONITOR
917
561
1086
606
NIL
ratio-influencers-innovators
17
1
11

MONITOR
951
636
1061
681
NIL
count influencers
17
1
11

MONITOR
1094
659
1389
704
NIL
mean [number-of-peers] of influencers
17
1
11

@#$#@#$#@
## WHAT IS IT?


## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

Role of narrative --> Compassionship, Clear distinction between good and bad, hardship. 

Role of collectiveness 
- (Synnergie in becoming green)
- Role of Network properties (average path)



## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.43"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-population">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="4.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="5"/>
      <value value="8"/>
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;Snowball&quot;"/>
      <value value="&quot;Outskirts&quot;"/>
      <value value="&quot;Bridges&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exclude-center-from-social-entrpeneur?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-design">
      <value value="&quot;Fishing-net&quot;"/>
      <value value="&quot;Small-World&quot;"/>
      <value value="&quot;Fireworks&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment (1)" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <exitCondition>stop-simulation?</exitCondition>
    <metric>norm-csag</metric>
    <metric>norm-csab</metric>
    <metric>tick-ended</metric>
    <metric>sim-ended?</metric>
    <metric>green-majority?</metric>
    <metric>nw:mean-path-length</metric>
    <metric>COUNT CONNECTIONS</metric>
    <metric>mean [count connection-neighbors] of people</metric>
    <metric>mean [ nw:clustering-coefficient ] of turtles</metric>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.43"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-time">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-size">
      <value value="420"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="4.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;Snowball&quot;"/>
      <value value="&quot;Bridges&quot;"/>
      <value value="&quot;Outskirts&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment 2" repetitions="5" runMetricsEveryStep="true">
    <preExperiment>set-default-settings</preExperiment>
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>count people with [my-behavior = 1]</metric>
    <metric>gcc</metric>
    <metric>mal</metric>
    <enumeratedValueSet variable="Population-Size">
      <value value="420"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="2"/>
      <value value="3.5"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;Outskirts&quot;"/>
      <value value="&quot;Bridges&quot;"/>
      <value value="&quot;Snowball&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.15"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;circle&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="0.1"/>
      <value value="1"/>
      <value value="2"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment10NCC" repetitions="75" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>mean [nw:closeness-centrality] of people</metric>
    <metric>mean [nw:eigenvector-centrality] of people</metric>
    <metric>mean [nw:betweenness-centrality] of people</metric>
    <metric>mean [nw:page-rank] of people</metric>
    <metric>nw:mean-path-length</metric>
    <metric>variance [nw:closeness-centrality] of people</metric>
    <metric>variance [nw:eigenvector-centrality] of people</metric>
    <metric>variance [nw:betweenness-centrality] of people</metric>
    <metric>variance [nw:page-rank] of people</metric>
    <metric>majority-green?</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="4.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Snowball&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="820"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment1DR" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
  </experiment>
  <experiment name="experimentNDCC" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>mean [nw:closeness-centrality] of people</metric>
    <metric>mean [nw:eigenvector-centrality] of people</metric>
    <metric>mean [nw:betweenness-centrality] of people</metric>
    <metric>mean [nw:page-rank] of people</metric>
    <metric>nw:mean-path-length</metric>
    <metric>variance [nw:closeness-centrality] of people</metric>
    <metric>variance [nw:eigenvector-centrality] of people</metric>
    <metric>variance [nw:betweenness-centrality] of people</metric>
    <metric>variance [nw:page-rank] of people</metric>
    <metric>majority-green?</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="4.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Snowball&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="420"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment4-100" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>mean [nw:closeness-centrality] of people</metric>
    <metric>mean [nw:eigenvector-centrality] of people</metric>
    <metric>mean [nw:betweenness-centrality] of people</metric>
    <metric>mean [nw:page-rank] of people</metric>
    <metric>nw:mean-path-length</metric>
    <metric>variance [nw:closeness-centrality] of people</metric>
    <metric>variance [nw:eigenvector-centrality] of people</metric>
    <metric>variance [nw:betweenness-centrality] of people</metric>
    <metric>variance [nw:page-rank] of people</metric>
    <metric>majority-green?</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="4.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="420"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment5-100" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>mean [nw:closeness-centrality] of people</metric>
    <metric>mean [nw:eigenvector-centrality] of people</metric>
    <metric>mean [nw:betweenness-centrality] of people</metric>
    <metric>mean [nw:page-rank] of people</metric>
    <metric>nw:mean-path-length</metric>
    <metric>variance [nw:closeness-centrality] of people</metric>
    <metric>variance [nw:eigenvector-centrality] of people</metric>
    <metric>variance [nw:betweenness-centrality] of people</metric>
    <metric>variance [nw:page-rank] of people</metric>
    <metric>majority-green?</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="4.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="420"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment8DRCC" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>mean [nw:closeness-centrality] of people</metric>
    <metric>mean [nw:eigenvector-centrality] of people</metric>
    <metric>mean [nw:betweenness-centrality] of people</metric>
    <metric>mean [nw:page-rank] of people</metric>
    <metric>nw:mean-path-length</metric>
    <metric>variance [nw:closeness-centrality] of people</metric>
    <metric>variance [nw:eigenvector-centrality] of people</metric>
    <metric>variance [nw:betweenness-centrality] of people</metric>
    <metric>variance [nw:page-rank] of people</metric>
    <metric>majority-green?</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Snowball&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentTH_D1_840_4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.4"/>
    <steppedValueSet variable="threshold-pop" first="0.25" step="0.05" last="0.75"/>
  </experiment>
  <experiment name="experimentTH_D3_840_025" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentTH_D5_840" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.4"/>
    <steppedValueSet variable="threshold-pop" first="0.25" step="0.05" last="0.75"/>
  </experiment>
  <experiment name="experimentTH_D7_840" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.4"/>
    <steppedValueSet variable="threshold-pop" first="0.25" step="0.05" last="0.75"/>
  </experiment>
  <experiment name="Network_Metrics" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>mean [nw:closeness-centrality] of people</metric>
    <metric>mean [nw:eigenvector-centrality] of people</metric>
    <metric>mean [nw:betweenness-centrality] of people</metric>
    <metric>mean [nw:weighted-closeness-centrality connection-weight] of people</metric>
    <metric>mean [ nw:clustering-coefficient ] of people</metric>
    <metric>MCRPL</metric>
    <metric>MCPL</metric>
    <metric>MPL</metric>
    <metric>variance [nw:closeness-centrality] of people</metric>
    <metric>variance [nw:eigenvector-centrality] of people</metric>
    <metric>variance [nw:betweenness-centrality] of people</metric>
    <metric>variance [nw:weighted-closeness-centrality connection-weight] of people</metric>
    <metric>variance [ nw:clustering-coefficient ] of people</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Silver-Bullets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="5"/>
      <value value="7"/>
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="0.5"/>
      <value value="1"/>
      <value value="1.5"/>
      <value value="2"/>
      <value value="2.5"/>
      <value value="3"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentST" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="dekkers-power">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
  </experiment>
  <experiment name="experiment7DR" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
  </experiment>
  <experiment name="experiment10DR" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.25"/>
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="2"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-entrepeneurs" first="0.1" step="0.05" last="0.4"/>
  </experiment>
  <experiment name="experiment_1_run" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_1330_runs" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-entrepeneurs" first="0.1" step="0.1" last="0.3"/>
  </experiment>
  <experiment name="experiment_64_run" repetitions="64" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tipping-strategy">
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-entrepeneurs">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment5DR" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
  </experiment>
  <experiment name="experimentTH_D1_840_7_1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.4"/>
    <steppedValueSet variable="threshold-pop" first="0.25" step="0.05" last="0.75"/>
  </experiment>
  <experiment name="experimentTH_D1_840_10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.4"/>
    <steppedValueSet variable="threshold-pop" first="0.25" step="0.05" last="0.75"/>
  </experiment>
  <experiment name="experimentTH_1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.4"/>
    <steppedValueSet variable="threshold-pop" first="0.25" step="0.1" last="0.55"/>
  </experiment>
  <experiment name="experimentTH_D1_840_7_10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.4"/>
    <steppedValueSet variable="threshold-pop" first="0.25" step="0.05" last="0.75"/>
  </experiment>
  <experiment name="experiment5DRn" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCe</metric>
    <metric>MEC</metric>
    <metric>MBC</metric>
    <metric>MCCe</metric>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCe</metric>
    <metric>VEC</metric>
    <metric>VBC</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>majority-green?</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="seed">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="4"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;stochastic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;S_Closeness&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
  </experiment>
  <experiment name="experimentTH_030" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentTH_040" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentTH_050_NO" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="network-generator">
      <value value="&quot;custom&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentTH_060" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentTH_070" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="social-innovators" first="0.1" step="0.1" last="0.3"/>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentKL_6" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="network-generator">
      <value value="&quot;Kleinberg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Silver-Bullets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.2"/>
      <value value="1.4"/>
      <value value="1.6"/>
      <value value="1.8"/>
      <value value="2.2"/>
      <value value="2.4"/>
      <value value="2.6"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-innovators">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentCU_6" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="network-generator">
      <value value="&quot;custom&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Silver-Bullets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-innovators">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentWS_6" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="network-generator">
      <value value="&quot;Small-World&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-innovators">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentPA_6" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <enumeratedValueSet variable="network-generator">
      <value value="&quot;Scale-Free&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Silver-Bullets&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-innovators">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentTH_050_NO_IN" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>report-sim-ended</exitCondition>
    <metric>MSCl</metric>
    <metric>gcc</metric>
    <metric>MCRPL</metric>
    <metric>MPL</metric>
    <metric>VCCo</metric>
    <metric>VD</metric>
    <metric>green-percentage</metric>
    <metric>tick-ended</metric>
    <metric>ratio-influencers-innovators</metric>
    <enumeratedValueSet variable="network-generator">
      <value value="&quot;custom&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dekkers-power">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="module">
      <value value="&quot;threshold-model&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seeding-strategy">
      <value value="&quot;Shotgun&quot;"/>
      <value value="&quot;Silver-Bullets&quot;"/>
      <value value="&quot;CR_Closeness&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-communities">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewiring-proportion">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-to-decision">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k1">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Population-Size">
      <value value="840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k2">
      <value value="1.1E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="static-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;spring&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clustering-coefficient">
      <value value="1"/>
      <value value="1.4"/>
      <value value="1.8"/>
      <value value="2.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-innovators">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-pop">
      <value value="0.5"/>
    </enumeratedValueSet>
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
0
@#$#@#$#@

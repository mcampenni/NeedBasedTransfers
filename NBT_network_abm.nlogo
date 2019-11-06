
breed [links-link]

globals 
 [
 volatility_s_sd   ;standard dev of volatility SD
 count_asks        
 count_gives
 sum_asks          
 sum_gives
 ;memory_size
 players
 beta
 alive_agents
 avg_numb_transactions_round
 avg_size_transactions_round
 counter
 flag
 ]
 
turtles-own 
[
active prev_cattle cattle net_received net_received_list p_goodstanding_list time_since_give_list give_osotua requested_amount cur_request cur_give
c_asking_rate c_returning_rate c_rounds_min partner age t50survival t0cattle t5cattle t10cattle t15cattle t20cattle t25cattle t30cattle t35cattle 
t40cattle t45cattle t50cattle t55cattle t60cattle t65cattle t70cattle t75cattle t80cattle t85cattle t90cattle t95cattle t100cattle tsum_gives tcount_gives
generosity

coupled
]

links-own
[
rewired?
]

to setup
 ifelse pa-net = FALSE [
   ca
   create-turtles agent_num
 ] 
 ;; next procedure will implement a different type of network, namely, a "preferential-attachment one"
 [ create-PA-network ]
 
  if P-osotua = 1 [
  set P-osotua agent_num
  ]
 
   ask turtles
  [
  ;use a specific distribution to set generosity property of agents
  set generosity 100
  ;set generosity random-normal p-generosity 15
  
  set cattle initial_cattle
  set active 1
  set net_received_list n-values agent_num [0]
  set p_goodstanding_list n-values agent_num [1]
  set time_since_give_list n-values agent_num [0]
  
  set coupled 0
  
  set shape "circle"
  set size 1
  set color blue
  set partner -1
  ]
  ask n-of P-osotua turtles [ set give_osotua 1 ] 
  
  setup-plots
  update-plots
  
  set alive_agents n-values 20 [0]
  set avg_numb_transactions_round n-values 20 [0]
  set avg_size_transactions_round n-values 20 [0]
  
  set beta round agent_num / 100 * 20
  
  if pa-net = FALSE [
    layout-circle (sort turtles) max-pxcor - 1
    wire-them
    ifelse small-world = TRUE [
      repeat beta [rewire-one]
    ]
    []
    ;;[rewire-all]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;
;;NETWORK procedures;;
;;;;;;;;;;;;;;;;;;;;;;

;; creates a new lattice
to wire-them
  ;; iterate over the turtles
  let n 0
  while [n < count turtles]
  [
    ;; make edges with the next two neighbors
    ;; this makes a lattice with average degree of 4
    ;; so, in other words, k = 4
    make-edge turtle n
              turtle ((n + 1) mod count turtles)
    ;;comment next two lines of code to implement a network with N = 2
    make-edge turtle n
              turtle ((n + 2) mod count turtles)
    set n n + 1
  ]
end

;; connects the two turtles
to make-edge [node1 node2]
  ask node1 [ create-link-with node2  [
    set rewired? false
  ] ]
end

;;REWIRING PROCEDURES
to rewire-one

  ;; make sure num-turtles is setup correctly else run setup first
  if count turtles != agent_num [
    setup
  ]

  let potential-edges links with [ not rewired? ]
  ifelse any? potential-edges [
    ask one-of potential-edges [
      ;; "a" remains the same
      let node1 end1
      ;; if "a" is not connected to everybody
      if [ count link-neighbors ] of end1 < (count turtles - 1)
      [
        ;; find a node distinct from node1 and not already a neighbor of node1
        let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
        ;; wire the new edge
        ask node1 [ create-link-with node2 [ set color cyan  set rewired? true ] ]

        ;set number-rewired number-rewired + 1  ;; counter for number of rewirings

        ;; remove the old edge
        die
      ]
    ]
  ]
  [ user-message "all edges have already been rewired once" ]
end

to rewire-all

  ;; make sure num-turtles is setup correctly; if not run setup first
  if count turtles != agent_num [
    setup
  ]

  ;; set up a variable to see if the network is connected
  let success? false

  ;; if we end up with a disconnected network, we keep trying, because the APL distance
  ;; isn't meaningful for a disconnected network.
  while [not success?] [
    ;; kill the old lattice, reset neighbors, and create new lattice
    ask links [ die ]
    wire-them
    ;set number-rewired 0

    ask links [

      ;; whether to rewire it or not?
      if (random-float 1) < .5 ;rewiring-probability
      [
        ;; "a" remains the same
        let node1 end1
        ;; if "a" is not connected to everybody
        if [ count link-neighbors ] of end1 < (count turtles - 1)
        [
          ;; find a node distinct from node1 and not already a neighbor of node1
          let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
          ;; wire the new edge
          ask node1 [ create-link-with node2 [ set color cyan  set rewired? true ] ]

          ;set number-rewired number-rewired + 1  ;; counter for number of rewirings
          set rewired? true
        ]
      ]
      ;; remove the old edge
      if (rewired?)
      [
        die
      ]
    ]

    ;; check to see if the new network is connected and calculate path length and clustering
    ;; coefficient at the same time
    set success? TRUE
  ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to create-PA-network
  clear-all
  set-default-shape turtles "circle"
  ;; make the initial network of two turtles and an edge
  make-node nobody        ;; first node, unattached
  make-node turtle 0      ;; second node, attached to first nod
  repeat agent_num - 2 [make-node find-partner]
  ;ask turtles [set size 0.5]
  layout
end

;; used for creating a new node
to make-node [old-node]
  crt 1
  [
    set color red
    if old-node != nobody
      [ create-link-with old-node [ set color green ]
        ;; position the new node near its partner
        move-to old-node
        fd 10
      ]
  ]
end

;; This code is borrowed from Lottery Example (in the Code Examples
;; section of the Models Library).
;; The idea behind the code is a bit tricky to understand.
;; Basically we take the sum of the degrees (number of connections)
;; of the turtles, and that's how many "tickets" we have in our lottery.
;; Then we pick a random "ticket" (a random number).  Then we step
;; through the turtles to figure out which node holds the winning ticket.

to-report find-partner
  let total random-float sum [count link-neighbors] of turtles
  let newpartner nobody
  ask turtles
  [
    let nc count link-neighbors
    ;; if there's no winner yet...
    if newpartner = nobody
    [
      ifelse nc > total
        [ set newpartner self ]
        [ set total total - nc ]
    ]
  ]
  report newpartner
end

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ set size sqrt count link-neighbors ]
  ]
  [
    ask turtles [ set size 1 ]
  ]
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; MAIN PROCEDURE ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go       ;This is the main loop for agents and cattle
 ;ask turtle 0 [show cattle]  ;for debugging
 
 ;cattle procedures

 ask turtles [ if active = 1 [set prev_cattle cattle] ] ;for debugging
 ask turtles [ if active = 1 [grow-cattle] ]
 
 ifelse random-float 1 < synch_prob [
 ;;this makes shocks events synchronous instead of asynchronous
 let change synchronous-volatile-change
 ask turtles [if active = 1 [set cattle (cattle * ((100 - change) / 100))]]
 ]
 [
 ask turtles [ if active = 1 [volatile-change] ]
 ]
 
 
 ask turtles [ if active = 1 [upper-limit] ]
 ask turtles [ if active = 1 [round-off] ]
 ask turtles [ if active = 1 [check-status] ]

 ;agent procedures
 if not noexchange [
 
  let newplay 0
  
  ;this part of the code will split the whole agentset into couples of playing agents randomly selected
  while [(count turtles with [coupled = 0 and active = 1] > 1) AND newplay = 0] [
   
   let node1 no-turtles
   let node2 no-turtles
   let first-node 0
   
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;;this makes couples of connected nodes;;
    if any? turtles with [coupled = 0 AND active = 1] [
     set node1 one-of turtles with [coupled = 0 AND active = 1]
     set first-node 1
     
     if first-node = 1 AND (count turtles with [coupled = 0 AND active = 1 AND who != [who] of node1] >= 1) [
     ifelse random-ask = TRUE [
       set node2 one-of turtles with [coupled = 0 AND link-neighbor? node1 AND active = 1]
       ]

      ;; this is to implement selective ask to the richest linked node
      [
      let possible-nodes turtles with [coupled = 0 AND link-neighbor? node1 AND active = 1 AND who != [who] of node1]
      ifelse count possible-nodes > 1 [
      set node2 max-one-of possible-nodes [cattle]
;      output-write node1
;      output-write node2
;      output-write [cattle] of node2
;      output-print ""
      ]
        [set node2 one-of possible-nodes]
        ]
      ]
 
    set players (turtle-set node1 node2)
    ifelse count players > 1 [
      ask players [set partner [who] of one-of other players with [self != myself]]
      ask players [set coupled 1]
      ]
      [set newplay 1]
     
     ]
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;if the agents "players" is not big enough (< 2 + 1 agents - otherwise the same couple will play over and over), a new coupling cycle starts
    if count players < 2 [set newplay 1]
    
    ;;this code let them play ONLY as couples
    ;;it makes a double check on length of agentset "players"
    if count players > 1 and newplay = 0 [
      ask players [ if (active = 1) and (give_osotua = 0) [payback]]
      ask players [ if active = 1 [update-pcredit]]
      ask players [ if active = 1 [request]]
      ask players [ if active = 1 [give]]
      ask players [ if active = 1 [set coupled 1]]
      ]
    
    ]
    ask turtles [ set coupled 0 ]
 ]
 
 ask turtles [ if active = 1 [reset-variables]]
 ask turtles [ if active = 1 [check-die]]
 
 ;variables and data collection procedures
 progress    ;this has an active check embedded as well
 update-plots
 update-output
 
 
 ;;--------------------------------------------------------
 ;; this update transactions lists and alive agents runtime
 ;;--------------------------------------------------------

 if ticks = 0 [
   set counter 0
   set flag 0
 ]
 if remainder ticks 5 = 0 and ticks <= 100 [
   set alive_agents replace-item counter alive_agents count turtles with [active = 1]
   set avg_numb_transactions_round replace-item counter avg_numb_transactions_round count_gives
   set avg_size_transactions_round replace-item counter avg_size_transactions_round sum_gives
   set counter counter + 1
 ]
 if (ticks = 100 OR count turtles with [active = 1] < 1) AND flag = 0 [
   file-open (word precision synch_prob 1 "_" P-osotua "_" agent_num "_" pa-net "_" small-world "_" "alive_agents.csv")
   foreach alive_agents [
     file-write ?
   ]
   file-print " "
   
   file-open (word precision synch_prob 1 "_" P-osotua "_" agent_num "_" pa-net "_" small-world "_" "avg_numb_transactions_rounds.csv")
   foreach avg_numb_transactions_round [
     file-write ?
   ]
   file-print " "
   
   file-open (word precision synch_prob 1 "_" P-osotua "_" agent_num "_" pa-net "_" small-world "_" "avg_size_transactions_rounds.csv")
   foreach avg_size_transactions_round [
     file-write ?
   ]
   file-print " "   
   
   file-open (word precision synch_prob 1 "_" P-osotua "_" agent_num "_" pa-net "_" small-world "_" "received_matrices.csv")
   let agents-list sort turtles
   foreach agents-list [
     ask ? [
       foreach net_received_list [
         file-write ?
       ]
       file-print ""
     ]
   ]
     
   set flag 1
   file-close-all
 ]

 ;;--------------------------------------------------------
 ;;--------------------------------------------------------
 
 if count turtles with [active = 1] < 1 [ stop ]
end

to check-status      ;this checks whether agents have herds below the minimum size 
 ifelse cattle < herd_min
  [
  set c_rounds_min (c_rounds_min + 1)
  ;print c_rounds_min
  ]
  [
  set c_rounds_min 0      ;set consecutive rounds below min to 0 if count cattle reaches herd_min or above
  ]
end

to grow-cattle            ;implements herd growth algorithm
 let cur_growth_rate (random-normal cattle_growth_rate cattle_gr_sd)
 let cur_grow_amount ((cur_growth_rate / 100) * cattle)
 set cattle (cattle + cur_grow_amount)
 ;show cur_growth_rate
 ;show cur_grow_amount
end
 
to volatile-change      ;implements volatility algorithm
 if random 100 < volatility_rate   ;volatile change happens at volatility_rate
  [
  set volatility_s_sd (volatility_size / 3)  ; SD of volatility size should be 1/3 volatility size
  let cur_vol (random-normal volatility_size volatility_s_sd)
  set cattle (cattle * ((100 - cur_vol) / 100))
  ;print ticks
  ;show cur_vol
  ]
end

to-report synchronous-volatile-change      ;implements volatility algorithm
 let cur_vol 0
 if random 100 < volatility_rate   ;volatile change happens at volatility_rate
  [
  set volatility_s_sd (volatility_size / 3)  ; SD of volatility size should be 1/3 volatility size
  set cur_vol (random-normal volatility_size volatility_s_sd)
  ;set cattle (cattle * ((100 - cur_vol) / 100))
  ;print ticks
  ;show cur_vol
  ]
 report cur_vol
end

to upper-limit      ;doesn't allow herd size to go above maximum
 if cattle > herd_max [set cattle herd_max]
end
 
to progress      ;increments ticks representing years
 tick
 ask turtles [if active = 1 [set age (age + 1)]]
end
 
to check-die
 if c_rounds_min > 1 ;more than 0 rounds below herd_min means you're out
  [
  ;print age 
  set active 0
  ]   

end

to update-pcredit    ;keeps track of whether partner is in goodstanding
 ;if credit = 1 []  ;code here for credit case if included

 if (item partner net_received_list) < 0 ;if net_received (amount owed) is negative (i.e., there is a debt) then
  [
  set time_since_give_list (replace-item partner time_since_give_list (item partner time_since_give_list + 1))  ;increments number of days since loan as long as net_received is less than 0
  if (item partner time_since_give_list) > tolerated_delay 
   [
   set p_goodstanding_list (replace-item partner p_goodstanding_list 0) ;if you gave to your partner and your partner didn't pay back within tolerated delay, you place them in bad standing
   ]
  ]
 if (item partner net_received_list) >= 0 [set time_since_give_list (replace-item partner time_since_give_list 0)]
end

to request      ;upper level request proceedure - separate rules go to different subprocedures
 if count players > 1       ;if there is another turtle on your patch
  [
  needbased-request
  set [requested_amount] of turtle partner cur_request   ;sets partner 'requested_amount' to the current request amount of the agent
  ]
end

to needbased-request                   ;you ask for what you need (herd_min-cattle) if you need it
 ifelse c_rounds_min > 0
  [
  set cur_request (herd_min - cattle)
  update-asks
  ]
  [set cur_request 0]
end

to update-asks
  set count_asks (count_asks + 1)
  set sum_asks (sum_asks + cur_request)
end

;; in this procedure agents with "give_osotua = 0" do two things: first payback then reciprocate
to give      ;upper level give proceedure - separate rules go to different subprocedures 
  if give_osotua = 0 [payback] ;reciprocity agents go through payback proceedure
  if requested_amount > 0     ;if a request has been made
   [
   if give_osotua = 0  
    [
    if(random 100 < generosity) [recip-give]   ; this makes generosity the likelihood of making a transfer in the culturally typical ways of making a transfer.   
    ] 
   if give_osotua = 1 
    [
    if(random 100 < generosity)
     [osotua-give] 
    ]
   ]
end

to payback  ;agents check if they can payback - only for recip agents
 ;show "r1"
 if (item partner net_received_list > 0) and ((cattle - item partner net_received_list) >= herd_min)  ;this part repays if repay_prob met and you have enough

  [
  ;show "r2"
  if (random 100 < repay_prob)
   [
   ;show "r3"                       ;this part repays if repay_prob met and you have enough
   let c-g item partner net_received_list
   set cur_give c-g
   account-cattle          ;sets self and partner cattle variables property to account for transfers
   
   ;update the list of received ????
   ;set net_received_list replace-item partner net_received_list 0
   ;show "repay"
   ]
  update-gives
  ]
end

to recip-give                      
;recip-give means granting credit; partners are in good standing as long as they do not have outstanding debt and payed back debt on time
 ;show "recip give"
 ;give check 
  ifelse c_rounds_min = 0
  [
  if (item partner p_goodstanding_list = 1) and (item partner net_received_list >= 0) 
  ;if the partner has no outstanding debt and is in good standing (has not exceeded tolerated delay or credit in cases when relevant)
   [
   if ((requested_amount + cur_give) <= cattle)            ;cur_give is augmented because there might be a cur_give from repay
    [
    ifelse (requested_amount <= credit_size)
     [set cur_give (cur_give + requested_amount)]                ;if amount asked is less than credit size cattle, asked amount
     [set cur_give (cur_give + credit_size)]                          ;else give amount of cattle - this seems weird, like there should be a limit here
    update-gives
    account-cattle          ;sets self and partner cattle variables property to account for transfers
    ]
   ]
  ]
  [set cur_give 0]
end

to osotua-give                  ;osotua give means you give what you can (cattle - herd_min) if asked
 ifelse c_rounds_min = 0
  [
  ifelse (requested_amount > (cattle - herd_min))  ;if the amount requested would put the giver below herd min
    [set cur_give (cattle - herd_min)]             ;then set cur_give to what the giver can give without going below herd_min
    [set cur_give requested_amount]                ;otherwise give the full amount
  update-gives
  account-cattle          ;sets self and partner cattle variables property to account for transfers
  ;show "osotua give"
  ]
  [set cur_give 0]
end

to account-cattle
  set cattle (cattle - cur_give)
  set net_received (net_received - cur_give)  ;sets your own net recieved to reflect transfer

  set [cattle] of turtle partner ([cattle] of turtle partner + cur_give)
  set [net_received] of turtle partner ([net_received] of turtle partner + cur_give) ;sets your partners net received to reflect transfer
  
  set net_received_list replace-item partner net_received_list (item partner net_received_list + cur_give)
  set [net_received_list] of turtle partner (replace-item partner [net_received_list] of turtle partner (item partner [net_received_list] of turtle partner + [cur_give] of turtle partner))
end

to update-gives
  set count_gives (count_gives + 1)
  set sum_gives (sum_gives + cur_give)

 ;these are turtle specific
   set tcount_gives (tcount_gives + 1)
   set tsum_gives (tsum_gives + cur_give)
end

to reset-variables
;;commented for debugging
 set cur_give 0
 set cur_request 0
 set requested_amount 0
end
 
to round-off
 set cattle round cattle
end

to update-plots
;this updates old plot update
  set-current-plot-pen "mean-cattle"
  plot mean [cattle] of turtles
  ;set-current-plot-pen "median-cattle"
  ;plot median [cattle] of turtles

; plot [cattle] of turtle 0
; plot [cattle] of turtle 1
end

to setup-plots
  set-current-plot "Cattle"
;  set-plot-x-range 0 (initial_cattle * 100)
;  set-plot-y-range 0 count turtles
;  set-histogram-num-bars 100
end

to update-output
  if ticks = 0
   [
   ask turtles [if active = 1 [set t0cattle cattle]]
   ]
  if ticks = 5 
   [
   ask turtles [if active = 1 [set t5cattle cattle]]
   ]
  if ticks = 10 
   [
   ask turtles [if active = 1 [set t10cattle cattle]]
   ]
  if ticks = 15 
   [
   ask turtles [if active = 1 [set t15cattle cattle]]
   ]
  if ticks = 20 
   [
   ask turtles [if active = 1 [set t20cattle cattle]]
   ]
  if ticks = 25 
   [
   ask turtles [if active = 1 [set t25cattle cattle]]
   ]
  if ticks = 30 
   [
   ask turtles [if active = 1 [set t30cattle cattle]]
   ]
  if ticks = 35 
   [
   ask turtles [if active = 1 [set t35cattle cattle]]  
   ]
  if ticks = 40 
   [
   ask turtles [if active = 1 [set t40cattle cattle]]
   ]
  if ticks = 45 
   [
   ask turtles [if active = 1 [set t45cattle cattle]]
   ]
  if ticks = 50 
   [
   ask turtles [if active = 1 [set t50cattle cattle]]
   ]
  if ticks = 55 
   [
   ask turtles [if active = 1 [set t55cattle cattle]]
   ]
  if ticks = 60 
   [
   ask turtles [if active = 1 [set t60cattle cattle]]
   ]
  if ticks = 65 
   [
   ask turtles [if active = 1 [set t65cattle cattle]]
   ]
  if ticks = 70 
   [
   ask turtles [if active = 1 [set t70cattle cattle]]
   ]
  if ticks = 75 
   [
   ask turtles [if active = 1 [set t75cattle cattle]]
   ]
  if ticks = 80 
   [
   ask turtles [if active = 1 [set t80cattle cattle]]
   ]
  if ticks = 85 
   [
   ask turtles [if active = 1 [set t85cattle cattle]]
   ]
  if ticks = 90 
   [
   ask turtles [if active = 1 [set t90cattle cattle]]
   ]
  if ticks = 95 
   [
   ask turtles [if active = 1 [set t95cattle cattle]]  
   ]
  if ticks = 100 
   [
   ask turtles [if active = 1 [set t100cattle cattle]]
   ]
   
end


to show-generosity
  set-current-plot "generosity_distribution"
  set-histogram-num-bars 100
  let generosities [generosity] of turtles
  histogram generosities
end
  

@#$#@#$#@
GRAPHICS-WINDOW
550
10
865
346
30
30
5.0
1
10
1
1
1
0
0
0
1
-30
30
-30
30
0
0
1
ticks

CC-WINDOW
5
643
1198
738
Command Center
0

SLIDER
26
95
198
128
agent_num
agent_num
4
1000
100
2
1
NIL
HORIZONTAL

BUTTON
39
19
105
52
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
131
20
194
53
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

TEXTBOX
57
75
207
93
Agent variables
11
0.0
1

MONITOR
541
417
598
462
NIL
ticks
17
1
11

PLOT
608
383
888
532
Cattle
NIL
NIL
0.0
800.0
0.0
2.0
true
false
PENS
"mean-cattle" 1.0 0 -2674135 true
"median-cattle" 1.0 0 -16777216 true

MONITOR
503
467
601
512
average cattle
mean [cattle] of turtles
4
1
11

INPUTBOX
292
47
405
107
initial_cattle
70
1
0
Number

INPUTBOX
289
111
404
171
cattle_growth_rate
3.4
1
0
Number

INPUTBOX
288
183
405
243
volatility_rate
10
1
0
Number

INPUTBOX
412
113
510
173
cattle_gr_sd
2.53
1
0
Number

INPUTBOX
288
254
407
314
volatility_size
30
1
0
Number

INPUTBOX
269
320
336
380
herd_min
64
1
0
Number

INPUTBOX
343
320
407
381
herd_max
600
1
0
Number

SWITCH
9
191
117
224
P1-osotua
P1-osotua
0
1
-1000

SWITCH
124
192
233
225
P2-osotua
P2-osotua
0
1
-1000

MONITOR
45
527
128
572
NIL
count_asks
17
1
11

MONITOR
140
528
228
573
NIL
count_gives
17
1
11

MONITOR
51
583
125
628
NIL
sum_asks
17
1
11

MONITOR
132
584
211
629
NIL
sum_gives
17
1
11

INPUTBOX
296
436
392
496
tolerated_delay
50
1
0
Number

INPUTBOX
401
436
497
496
repay_prob
100
1
0
Number

INPUTBOX
290
509
393
569
credit_size
10
1
0
Number

INPUTBOX
10
229
116
289
P1-generosity
100
1
0
Number

INPUTBOX
124
229
234
289
P2-generosity
100
1
0
Number

SWITCH
9
137
254
170
noexchange
noexchange
1
1
-1000

INPUTBOX
51
332
137
392
p-generosity
100
1
0
Number

MONITOR
37
398
153
443
mean generosity
mean [generosity] of turtles 
2
1
11

TEXTBOX
925
30
1186
70
debug section for multiplayer version
14
0.0
1

OUTPUT
922
63
1189
412
14

SLIDER
10
296
182
329
P-osotua
P-osotua
0
agent_num
0
1
1
NIL
HORIZONTAL

PLOT
949
470
1149
620
generosity_distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 1 -16777216 true

MONITOR
988
418
1116
463
memory size
length [net_received_list] of one-of turtles
17
1
11

BUTTON
430
20
526
53
rewire one
rewire-one
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
431
65
518
98
rewire all
rewire-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
210
21
273
54
run
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

TEXTBOX
421
194
571
212
volatility_rate = 10
11
0.0
1

BUTTON
421
217
515
250
cr-PA-net
create-PA-network
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
422
259
525
292
pa-net
pa-net
0
1
-1000

SWITCH
416
299
530
332
small-world
small-world
1
1
-1000

SWITCH
416
375
530
408
random-ask
random-ask
1
1
-1000

SLIDER
31
463
203
496
synch_prob
synch_prob
0
1
1
0.1
1
NIL
HORIZONTAL

@#$#@#$#@
WHAT IS IT?
-----------
This section could give a general understanding of what the model is trying to show or explain.


HOW IT WORKS
------------
This section could explain what rules the agents use to create the overall behavior of the model.


HOW TO USE IT
-------------
This section could explain how to use the model, including a description of each of the items in the interface tab.


THINGS TO NOTICE
----------------
This section could give some ideas of things for the user to notice while running the model.


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


CREDITS AND REFERENCES
----------------------
This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.


NOTES
----------------------
Code for Calculating her size stuff

 ;print ticks
 ;ask turtles 
 ; [
  
  ;calculate running average of cattle
  ;set T_cattle (T_cattle + cattle)
  ;print T_cattle
  ;set M_cattle (T_cattle / ticks)
  ;print M_cattle
  ;]
  
 ;calculate running ave difference between max and min cattle
 ;let cur_cattle_diff (max [cattle] of turtles - min [cattle] of turtles)  
 ;print cur_cattle_diff
 ;set T_cattle_diff ( T_cattle_diff + cur_cattle_diff)
 ;print T_cattle_diff
 ;set M_cattle_diff ( T_cattle_diff / ticks)
 ;print M_cattle_diff
 
 ;calculate running average of sum of squares of difference
 ;print cur_cattle_diff
 ;let cur_S_cattle_diff (cur_cattle_diff ^ 2)
 ;print cur_S_cattle_diff
 ;set T_S_cattle_diff (T_S_cattle_diff + cur_S_cattle_diff)
 ;print T_S_cattle_diff
 ;set M_S_cattle_diff (T_S_cattle_diff / ticks)
 ;print M_S_cattle_diff


Update procedures for strategies based on memory info


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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.0.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="regularAndSmallWorld" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="30000"/>
    <exitCondition>count turtles with [active = 1] &lt; 1</exitCondition>
    <metric>mean [age] of turtles</metric>
    <metric>mean [t50survival] of turtles</metric>
    <metric>mean [t0cattle] of turtles</metric>
    <metric>mean [t5cattle] of turtles</metric>
    <metric>mean [t10cattle] of turtles</metric>
    <metric>mean [t15cattle] of turtles</metric>
    <metric>mean [t20cattle] of turtles</metric>
    <metric>mean [t25cattle] of turtles</metric>
    <metric>mean [t30cattle] of turtles</metric>
    <metric>mean [t35cattle] of turtles</metric>
    <metric>mean [t40cattle] of turtles</metric>
    <metric>mean [t45cattle] of turtles</metric>
    <metric>mean [t50cattle] of turtles</metric>
    <metric>mean [t55cattle] of turtles</metric>
    <metric>mean [t60cattle] of turtles</metric>
    <metric>mean [t65cattle] of turtles</metric>
    <metric>mean [t70cattle] of turtles</metric>
    <metric>mean [t75cattle] of turtles</metric>
    <metric>mean [t80cattle] of turtles</metric>
    <metric>mean [t85cattle] of turtles</metric>
    <metric>mean [t90cattle] of turtles</metric>
    <metric>mean [t95cattle] of turtles</metric>
    <metric>mean [t100cattle] of turtles</metric>
    <metric>mean [tsum_gives] of turtles</metric>
    <metric>mean [tcount_gives] of turtles</metric>
    <metric>sum_gives</metric>
    <metric>count_gives</metric>
    <metric>synch_prob</metric>
    <enumeratedValueSet variable="P2-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cattle_gr_sd">
      <value value="2.53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herd_min">
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_cattle">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P1-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cattle_growth_rate">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herd_max">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P1-osotua">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repay_prob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_num">
      <value value="6"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility_size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility_rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerated_delay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P2-osotua">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="noexchange">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P-osotua">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pa-net">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="small-world">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-ask">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="synch_prob" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="preferential" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="30000"/>
    <exitCondition>count turtles with [active = 1] &lt; 1</exitCondition>
    <metric>mean [age] of turtles</metric>
    <metric>mean [t50survival] of turtles</metric>
    <metric>mean [t0cattle] of turtles</metric>
    <metric>mean [t5cattle] of turtles</metric>
    <metric>mean [t10cattle] of turtles</metric>
    <metric>mean [t15cattle] of turtles</metric>
    <metric>mean [t20cattle] of turtles</metric>
    <metric>mean [t25cattle] of turtles</metric>
    <metric>mean [t30cattle] of turtles</metric>
    <metric>mean [t35cattle] of turtles</metric>
    <metric>mean [t40cattle] of turtles</metric>
    <metric>mean [t45cattle] of turtles</metric>
    <metric>mean [t50cattle] of turtles</metric>
    <metric>mean [t55cattle] of turtles</metric>
    <metric>mean [t60cattle] of turtles</metric>
    <metric>mean [t65cattle] of turtles</metric>
    <metric>mean [t70cattle] of turtles</metric>
    <metric>mean [t75cattle] of turtles</metric>
    <metric>mean [t80cattle] of turtles</metric>
    <metric>mean [t85cattle] of turtles</metric>
    <metric>mean [t90cattle] of turtles</metric>
    <metric>mean [t95cattle] of turtles</metric>
    <metric>mean [t100cattle] of turtles</metric>
    <metric>mean [tsum_gives] of turtles</metric>
    <metric>mean [tcount_gives] of turtles</metric>
    <metric>sum_gives</metric>
    <metric>count_gives</metric>
    <metric>synch_prob</metric>
    <enumeratedValueSet variable="P2-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cattle_gr_sd">
      <value value="2.53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herd_min">
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_cattle">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P1-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cattle_growth_rate">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herd_max">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P1-osotua">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repay_prob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_num">
      <value value="6"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility_size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility_rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerated_delay">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P2-osotua">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="noexchange">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P-osotua">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pa-net">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="small-world">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-ask">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="synch_prob" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="regularAndSmallWorld_varyingdelay_DBT" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="30000"/>
    <exitCondition>count turtles with [active = 1] &lt; 1</exitCondition>
    <metric>mean [age] of turtles</metric>
    <metric>mean [t50survival] of turtles</metric>
    <metric>mean [t0cattle] of turtles</metric>
    <metric>mean [t5cattle] of turtles</metric>
    <metric>mean [t10cattle] of turtles</metric>
    <metric>mean [t15cattle] of turtles</metric>
    <metric>mean [t20cattle] of turtles</metric>
    <metric>mean [t25cattle] of turtles</metric>
    <metric>mean [t30cattle] of turtles</metric>
    <metric>mean [t35cattle] of turtles</metric>
    <metric>mean [t40cattle] of turtles</metric>
    <metric>mean [t45cattle] of turtles</metric>
    <metric>mean [t50cattle] of turtles</metric>
    <metric>mean [t55cattle] of turtles</metric>
    <metric>mean [t60cattle] of turtles</metric>
    <metric>mean [t65cattle] of turtles</metric>
    <metric>mean [t70cattle] of turtles</metric>
    <metric>mean [t75cattle] of turtles</metric>
    <metric>mean [t80cattle] of turtles</metric>
    <metric>mean [t85cattle] of turtles</metric>
    <metric>mean [t90cattle] of turtles</metric>
    <metric>mean [t95cattle] of turtles</metric>
    <metric>mean [t100cattle] of turtles</metric>
    <metric>mean [tsum_gives] of turtles</metric>
    <metric>mean [tcount_gives] of turtles</metric>
    <metric>sum_gives</metric>
    <metric>count_gives</metric>
    <metric>synch_prob</metric>
    <enumeratedValueSet variable="P2-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cattle_gr_sd">
      <value value="2.53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herd_min">
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_cattle">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P1-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cattle_growth_rate">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herd_max">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P1-osotua">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repay_prob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_num">
      <value value="6"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility_size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility_rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerated_delay">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P2-osotua">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="noexchange">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P-osotua">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pa-net">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="small-world">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-ask">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="synch_prob" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="preferential_varyingdelay_DBT" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="30000"/>
    <exitCondition>count turtles with [active = 1] &lt; 1</exitCondition>
    <metric>mean [age] of turtles</metric>
    <metric>mean [t50survival] of turtles</metric>
    <metric>mean [t0cattle] of turtles</metric>
    <metric>mean [t5cattle] of turtles</metric>
    <metric>mean [t10cattle] of turtles</metric>
    <metric>mean [t15cattle] of turtles</metric>
    <metric>mean [t20cattle] of turtles</metric>
    <metric>mean [t25cattle] of turtles</metric>
    <metric>mean [t30cattle] of turtles</metric>
    <metric>mean [t35cattle] of turtles</metric>
    <metric>mean [t40cattle] of turtles</metric>
    <metric>mean [t45cattle] of turtles</metric>
    <metric>mean [t50cattle] of turtles</metric>
    <metric>mean [t55cattle] of turtles</metric>
    <metric>mean [t60cattle] of turtles</metric>
    <metric>mean [t65cattle] of turtles</metric>
    <metric>mean [t70cattle] of turtles</metric>
    <metric>mean [t75cattle] of turtles</metric>
    <metric>mean [t80cattle] of turtles</metric>
    <metric>mean [t85cattle] of turtles</metric>
    <metric>mean [t90cattle] of turtles</metric>
    <metric>mean [t95cattle] of turtles</metric>
    <metric>mean [t100cattle] of turtles</metric>
    <metric>mean [tsum_gives] of turtles</metric>
    <metric>mean [tcount_gives] of turtles</metric>
    <metric>sum_gives</metric>
    <metric>count_gives</metric>
    <metric>synch_prob</metric>
    <enumeratedValueSet variable="P2-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="credit_size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cattle_gr_sd">
      <value value="2.53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herd_min">
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_cattle">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P1-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cattle_growth_rate">
      <value value="3.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="herd_max">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P1-osotua">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="repay_prob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="agent_num">
      <value value="6"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility_size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="volatility_rate">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tolerated_delay">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P2-osotua">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="noexchange">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="P-osotua">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-generosity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pa-net">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="small-world">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-ask">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="synch_prob" first="0" step="0.1" last="1"/>
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

extensions[stats]
;extensions["C:\\Users\\laurens\\Document\\Master EPA\\Thesis\\Master thesis\\NetLogo implementation\\stats\\stats.jar"]

globals [
  region-boundaries ; a list of regions definitions, where each region is a list of its min pxcor and max pxcor
  reporting-cycle
  total-reported-accidents
  total-unreported-accidents
  accidents-in-safe-region
  fake-news-actions-global
]


patches-own [
  region ; the number of the region that the patch is in, patches outside all regions have region = 0
  info-on-patch
  recent-accidents
  streak-clean
  streak-kill
  surrounding-NGOs
  ticks-with-NGOs
  ticks-without-NGOs
  streak-clean-in-reporting-cycle
  streak-kill-in-reporting-cycle
  streak-unknown-in-reporting-cycle
  days-without-informers
]

breed [POCgroups POCgroup]
breed [NGOS NGO]
undirected-link-breed [aid-links aid-link]
undirected-link-breed [violence-links violence-link]

POCgroups-own [
  money-needed
  threat
  region-POC
  total-links
  willingness-to-travel
  humanitarians-killed
  money-received-last-tick
  context-trust
  surrounding-trust
  NGO-size-trust
  general-region-safety
  NGO-in-perimeter-trust
  money-trust
  total-trust
  priority
  humanitarian-incidents-this-cycle
  humanitarian-incidents-last-cycle
]

NGOS-own [
  link-max
  size-of-organisation
  cluster
  money
  money-funded
  risk-willingness
  information-level
  region-now
  regions-possible
  info-region1
  info-region2
  info-region3
  money-spent-last-tick
  initial-money-needed-in-camp
  money-needed-in-camp
  known-money-needed-in-camp
  reported-money-needed-in-camp
  cluster-reported-money-needed-in-camp
  accidents
  reported-accidents
  reported-accidents-in-reporting-cycle
  reporting
  location
  reported-location
  cluster-reporting
  total-money-spent
  time-in-camp
  fake-news-actions
  full-camp-knowledge
  cycle-accidents-previous-cycle
  cycle-accidents
  funding-delivered
  funding-delivered-previous-cycle
  funding-converted
  funding-converted-previous-cycle
  money-spent-SS
  money-spent-to-POC
  reporting_no
  cluster-reporting_no
  level-of-experience
  initial-turnover-rate
  info-progress
  total-time-waiting-for-info
  info-process-time-needed
  total-processing-time
  project-duration
  project-day
  false-information-reaction-high-risk-NGO
  false-information-reaction-low-risk-NGO
  false-information-reaction-medium-risk-NGO
]

to go
  clear-links
;  create-link-with-POC
  specify-POC-needs ;1
  decide-reporting ;2
  move-POCgroups ;3
  decide-movement ;4
  transfer-aid ;5
  redecide-region ;6
  color-regions ;7
  transfer-info ;8
  decide-trust ;9
  create-accidents ;10
  check-end ;11
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;SETUP PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all

  ; First, create the desired number of regions.
  ; If you want to use many regions in your own
  ; model, this is the crucial part.
  setup-regions number-of-regions
  move-NGOS-in-random-direction
  ; Color all the regions differently. In your own
  ; model, things would probably look different
  color-regions

  ; Finally, distribute the turtles in the different regions
  ;setup-turtles
  setup-NGOs
  setup-POCs
  reset-ticks
end

to color-regions
  ; The patches with region = 0 act as dividers and are
  ; not part of any region. All other patches get colored
  ; according to the region they're in.
  ask patches with [ region != 0 ] [
    set pcolor 2 + region * 10
    set plabel-color pcolor + 1
    set plabel region
  ]
end

to check-end
    if sum [money-needed-in-camp] of NGOS <= 0[
      ;show "no money needed"
      ;show ticks
    ]

end

to setup-POCs
  ; This procedure simply creates turtles in the different regions.
  ; The `foreach` pattern shown can be used whenever you
  ; need to do something for each different region.
  foreach (range 1 (length region-boundaries + 1)) [ [region-number] ->
    let region-patches patches with [ region = region-number ]
    create-POCgroups number-of-POCs-per-region [
      move-to one-of region-patches
      set region-POC region
      set color pcolor + 3
      set shape "face sad"
      set willingness-to-travel random 3 + displacement-rate
      setup-POC-needs
      setup-trust
      set priority FALSE
    ]
  ]

  ask NGOs[
    let temp-reg regions-possible
      move-to one-of POCgroups with [region = one-of temp-reg]
      set full-camp-knowledge TRUE
  ]

end

to setup-NGOs
  foreach (range 1 (length region-boundaries + 1)) [ [region-number] ->
    let region-patches patches with [ region = 1 ]
    create-NGOs number-of-ngos [
      move-to one-of region-patches
      set color 95
      set size 1
      set shape "house"
      set size-of-organisation 1 + random 4
      set money 0
      if risk-distribution = "uniform"
        [set risk-willingness random 10 / 10]
      if risk-distribution = "risk avoiding"
        [set risk-willingness (stats:beta 2 6)]
      if risk-distribution = "risk taking"
        [set risk-willingness (stats:beta 5 2)]

      set risk-willingness random 100 / 100
      set link-max random 3
      set cluster random int 7
      set reporting FALSE
      set cluster-reporting FALSE
      set full-camp-knowledge FALSE
      set initial-turnover-rate initial-turn-over-rate-input
      let minmum-project-duration minimum-project-duration
      let maxmum-project-duration maximum-project-duration
      set project-duration random (maxmum-project-duration - minmum-project-duration) + minmum-project-duration
      set money setup-funding self
      set money-funded money
      set false-information-reaction-high-risk-NGO false-information-reaction-high-risk-NGO1
      set false-information-reaction-low-risk-NGO false-information-reaction-low-risk-NGO1
      set false-information-reaction-medium-risk-NGO false-information-reaction-medium-risk-NGO1

    ]
  ]
  setup-possible-regions
  move-NGOS-in-random-direction
  setup-turnover-rate

end

to setup-turnover-rate
    ask NGOs[
    let wrf-initial-turnover-rate 0
    if risk-willingness >= 0.67 [
      set wrf-initial-turnover-rate initial-turnover-rate * (0.75 * -1)]
    if risk-willingness <= 0.67 and risk-willingness >= 0.33[
      set wrf-initial-turnover-rate initial-turnover-rate * (0.75 * 0)]
    if risk-willingness <= 0.33[
      set wrf-initial-turnover-rate initial-turnover-rate * (0.75 * 1)]

    let exf-initial-turnover-rate initial-turnover-rate * (0.37 * effectiveness-policy-turnover-rate)
    let pf-initial-turnover-rate initial-turnover-rate * (0.66 * (random 10 / 10))
    set initial-turnover-rate initial-turnover-rate + exf-initial-turnover-rate + pf-initial-turnover-rate + wrf-initial-turnover-rate

    set info-process-time-needed information-process-time

    if initial-turnover-rate >= 0.75 [
      set info-process-time-needed info-process-time-needed + 4]

    if initial-turnover-rate <= 0.75 and initial-turnover-rate >= 0.5[
      set info-process-time-needed info-process-time-needed + 2 ]

    if initial-turnover-rate <= 0.5 and initial-turnover-rate >= 0.25[
      set info-process-time-needed info-process-time-needed + 1 ]

    if initial-turnover-rate <= 0.25[
      set info-process-time-needed info-process-time-needed ]

    if size-of-organisation = 4 [
      set info-process-time-needed info-process-time-needed + 2]

    if size-of-organisation = 3 [
      set info-process-time-needed info-process-time-needed + 1]

    if size-of-organisation = 2 [
      set info-process-time-needed info-process-time-needed - 1]

    if size-of-organisation = 1 [
      set info-process-time-needed info-process-time-needed - 2]


  set info-process-time-needed max (list (0) (info-process-time-needed))
  ]
end

to setup-POC-needs
  let initial-needs ((7700000000 / 2) / (number-of-POCs-per-region * 3)) / 365 ; This is the money needed in Syria according to OCHA, / 2 for safety & security, / number of POCgroups / amount of days (* 3 cause they appear per region)

  if region-POC = 1 [
    set money-needed initial-needs * 0.8
  ]

  if region-POC = 2 [
    set money-needed initial-needs
  ]

  if region-POC = 3 [
    set money-needed initial-needs * 1.2
  ]

end


to setup-trust
  ask POCgroups[
    ;Here there has been chosen to use 1.5 as basic trust level, which then increases or decreases depending on the type of war
    if war-type = "Civil war" [set context-trust 1.5 + (1.5 * .44)]
    if war-type = "International War" [set context-trust 1.5 - (1.5 * 0.47)]
    if war-type = "Civil Violence" [set context-trust 1.5 + (1.5 * 0.36)]
    if war-type = "No violence" [set context-trust 1.5 + (1.5 * 0.39)]
    if war-type = "Transition" [set context-trust 1.5 + (1.5 * 0.23)]
    if war-type = "Peace keeping mission"[set context-trust 1.5 + (1.5 * 0.15)]

    ifelse any? NGOs in-radius 2
      [set NGO-in-perimeter-trust 1.5]
      [set NGO-in-perimeter-trust 0.5]

    ifelse any? NGOs in-radius 3 with [(size-of-organisation) > 2]
      [set NGO-size-trust 0]
      [set NGO-size-trust 1]

    if (count patches with [region = 3] in-radius 5 > count patches with [region = 2] in-radius 5) and (count patches with [region = 3] in-radius 5 > count patches with [region = 1] in-radius 5)
      [set general-region-safety 0]
    if (count patches with [region = 2] in-radius 5 > count patches with [region = 3] in-radius 5) and (count patches with [region = 2] in-radius 5 > count patches with [region = 1] in-radius 5)
      [set general-region-safety 0.5]
    if (count patches with [region = 1] in-radius 5 > count patches with [region = 2] in-radius 5) and (count patches with [region = 1] in-radius 5 > count patches with [region = 3] in-radius 5)
      [set general-region-safety 1]


    set money-trust 2  ;The extra two is given since money received is assumed to give max trust in the beginning

    set total-trust context-trust + NGO-size-trust + general-region-safety + NGO-in-perimeter-trust

    ifelse any? POCgroups in-radius 3 with [total-trust > [total-trust] of myself]
      [set surrounding-trust 1 + 0.5]
      [set surrounding-trust 1 - 0.5]

    set total-trust context-trust + surrounding-trust + NGO-size-trust + general-region-safety + NGO-in-perimeter-trust + money-trust
  ]


end

to-report setup-funding [x]
  let initial-funding-setup 0
  let avg-size mean [size-of-organisation] of NGOs
  ;show avg-size

  set initial-funding-setup ((initial-funding / 365) * project-duration) / ((number-of-NGOs * 3)/ avg-size) ;Total amount of funding received (OCHA), divided by average size/total number of NGO's * 3 for amount of regions


  if size-of-organisation = 2 [
    set initial-funding-setup initial-funding-setup * 2
  ]

  if size-of-organisation = 3 [
    set initial-funding-setup initial-funding-setup * 3
  ]

  if size-of-organisation = 4 [
    set initial-funding-setup initial-funding-setup * 4
  ]


  show initial-funding-setup
  report initial-funding-setup

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;GO PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to move-POCgroups
  ask POCgroups[
    if [money-needed] of self < item 0 [money-needed] of POCgroups with-max [money-needed]
    [let target one-of POCgroups in-radius willingness-to-travel with [(money-needed) < [money-needed] of myself]
      if target != nobody [move-to target]
      if target != nobody [rt random 90 fd 0.5]
    ]
    let reg 0
    ask patch-here[
      ifelse region = 1 [set reg 1]
        [ifelse region = 2[set reg 2][set reg 3]]]
    set region-POC reg]
end



to-report renew-funding [x]
  let temp 0

  ask x [
    show ticks
    show "we zijn hier"
    set project-day 0

    if money < funding-delivered[

      let accident-decider 0
      let funding-delivered-decider 0
      let reporting-decider 0
      let funding-converted-to-aid-decider 0
      let size-decider 0
      let funding-delivered-decider2 0

      ifelse reporting = TRUE [
        set reporting-decider 5
      ]
      [set reporting-decider -5]

      ifelse cycle-accidents-previous-cycle > cycle-accidents [
        set accident-decider -5
      ]
      [set accident-decider 5]

      ifelse size-of-organisation >= 2 [
        set size-decider 5
      ]
      [set size-decider 0]

      ifelse funding-delivered < money-funded [
        set funding-delivered-decider -5
      ]
      [set funding-delivered-decider 5]

      ifelse funding-delivered < funding-delivered-previous-cycle [
        set funding-delivered-decider2 -5
      ]
      [set funding-delivered-decider2 5]

      let final-percentage (accident-decider + funding-delivered-decider + reporting-decider + funding-converted-to-aid-decider + size-decider + funding-delivered-decider2) * 0.01

      ;show final-percentage

      set temp money-funded * (1 + final-percentage)
     ]]
  show temp
        report temp
  ; Funding depends on:
  ;1. Ability to deliver funds
  ;2. Percentage of funds delivered vs. spent on safety & security
  ;3. Funding cycle every 30 days
end


to decide-trust
  ask POCgroups[
    ifelse any? NGOs in-radius 2
      [set NGO-in-perimeter-trust (1 + (2 / 3))]
      [set NGO-in-perimeter-trust 0.5]

    ifelse any? NGOs in-radius 3 with [(size-of-organisation) > 2 or any? NGOs in-radius 3 = FALSE]
    [set NGO-size-trust 0]
    [set NGO-size-trust (1 + (2 / 3))]

    if (count patches with [region = 3] in-radius 5 > count patches with [region = 2] in-radius 5) and (count patches with [region = 3] in-radius 5 > count patches with [region = 1] in-radius 5)
    [set general-region-safety 0]
    if (count patches with [region = 2] in-radius 5 > count patches with [region = 3] in-radius 5) and (count patches with [region = 2] in-radius 5 > count patches with [region = 1] in-radius 5)
    [set general-region-safety (5 / 6)]
    if (count patches with [region = 1] in-radius 5 > count patches with [region = 2] in-radius 5) and (count patches with [region = 1] in-radius 5 > count patches with [region = 3] in-radius 5)
    [set general-region-safety (1 + (2 / 3))]

    let temp (- (1 + (2 / 3)))
    if money-needed != 0[
      set temp (list ((money-received-last-tick * 1.5) / (money-needed)) (1.5))
    ]
    ifelse money-needed < 0 [
      set money-trust (1 + (2 / 3))][set money-trust MIN temp]



    set total-trust context-trust + NGO-size-trust + general-region-safety + NGO-in-perimeter-trust


    ifelse any? POCgroups in-radius 3 with [total-trust > [total-trust] of myself]
      [set surrounding-trust (1 + (2 / 3))]
      [set surrounding-trust (1 - (2 / 3))]

    set total-trust context-trust + surrounding-trust + NGO-size-trust + general-region-safety + NGO-in-perimeter-trust + money-trust
  ]
end


to create-accidents

  ask POCgroups[

    let chance chance-of-accident total-trust
    ;show chance
    let ref random 10 * 0.1
    ;show ref

    if ref < chance and any? NGOs in-radius 3 = TRUE[
      ;show "BAM"
      set humanitarians-killed humanitarians-killed + 1
      set humanitarian-incidents-this-cycle humanitarian-incidents-this-cycle + 1
      if reporting-cycle = duration-reporting-cycle[
        set humanitarian-incidents-last-cycle humanitarian-incidents-this-cycle
        set humanitarian-incidents-this-cycle 0
      ]

      let target one-of NGOs in-radius 2
      if target != nobody[create-violence-link-with target
        ask violence-links[
          set color red
          set shape "fire"
        ]
        ask target[
          set accidents accidents + 1
          ifelse reporting = TRUE[
          set cycle-accidents cycle-accidents + 1
          set total-reported-accidents total-reported-accidents + 1
          set reported-accidents-in-reporting-cycle reported-accidents-in-reporting-cycle + 1
          ]
          [set total-unreported-accidents total-unreported-accidents + 1
          ]
          if region = 1[
            set accidents-in-safe-region accidents-in-safe-region + 1]
        ]
      ]
    ]
  ]
end

to specify-POC-needs
  ask POCgroups[
    if region-POC = 1[
      set money-needed money-needed + 0.8 * ((7700000000 / 2) / (number-of-POCs-per-region * 3)) / 365 ; This is the money needed in Syria according to OCHA, / 2 for safety & security, / number of POCgroups / amount of days (* 3 cause they appear per region)
      ;show money-needed1 This is approximately 100.000 per day
    ]

    if region-POC = 2[
      set money-needed money-needed + ((7700000000 / 2) / (number-of-POCs-per-region * 3)) / 365 ; This is the money needed in Syria according to OCHA, / 2 for safety & security, / number of POCgroups / amount of days (* 3 cause they appear per region)
    ]

    if region-POC = 3[
      set money-needed money-needed + 1.2 * ((7700000000 / 2) / (number-of-POCs-per-region * 3)) / 365 ; This is the money needed in Syria according to OCHA, / 2 for safety & security, / number of POCgroups / amount of days (* 3 cause they appear per region)
    ]
  ]
end

to-report chance-of-accident [x]
  report  1 / (e ^ x) + 0.3 / x
end


to redecide-region
  ifelse reporting-cycle < duration-reporting-cycle[
  ][
    ask patches[
      let informers NGOs in-radius 5 with [reporting = TRUE]
      let confirmed-accidents sum [reported-accidents-in-reporting-cycle] of informers

    ifelse ticks-with-NGOs > days-with-NGOS-before-safe and confirmed-accidents < threshold-days-without-accident[
      let neighbors_NGO patches in-radius 5
      ask neighbors_NGO[
        set region 1
      ]]
    [
      if confirmed-accidents > threshold-danger-of-region and ticks-without-NGOs < threshold-danger-days-without-NGOs[
        set region 3]]

    ifelse count (informers) = 0[
      set days-without-informers days-without-informers + 1
    ]
    [
      set days-without-informers 0
    ]

    if days-without-informers > 10[
      set region 2
    ]

    ]


  ]


  ;This block decides how dangerous a patch and its surrounding has been for the past x ticks
  ask patches[
    let my-neighbours POCgroups in-radius 3
    let total-accidents 0
    ask my-neighbours[
      set total-accidents sum [humanitarian-incidents-last-cycle] of my-neighbours
    ]
    set recent-accidents total-accidents
    ifelse total-accidents > 0 [
      set streak-kill streak-kill + 1
      set streak-clean 0]
    [set streak-clean streak-clean + 1
      set streak-kill 0]
  ]

  ;This block decides how many NGO's are currently in the region
  ask patches[
    set surrounding-NGOS count NGOs in-radius 3
    ifelse surrounding-NGOS >= 2 [
      set ticks-with-NGOs ticks-with-NGOs + 1
      set ticks-without-NGOs 0
    ]
    [
      set ticks-without-NGOs ticks-without-NGOs + 1
      set ticks-with-NGOs 0
    ]

  ]

end


to decide-movement

  ask POCgroups[
    ifelse (count other POCgroups in-radius 3) > 5 and (count NGOs in-radius 3) = 0 and sum [money-needed] of POCgroups in-radius 3 > 0[
      set priority TRUE
    ]
    [set priority FALSE]]


ask NGOs[
  ifelse info-process-time-needed > info-progress[
    set info-progress info-progress + 1
    set total-processing-time total-processing-time + 1
  ]
  [
    if risk-willingness <= 0.33[
      ifelse (check-POC-radius self = FALSE or project-day >= project-duration) or (check-POC-radius self = FALSE and known-money-needed-in-camp = 0) [
        set color 15
        set full-camp-knowledge FALSE
        set time-in-camp 0
        let coordinates NGO-move-away self
        setxy (item 0 coordinates) (item 1 coordinates)
        set info-progress 0
        set known-money-needed-in-camp 0
        set initial-money-needed-in-camp 0
        set reported-money-needed-in-camp 0
        set cluster-reported-money-needed-in-camp 0
      ]
      [set project-day project-day + 1]
    ]

    if risk-willingness >= 0.33 and risk-willingness <= 0.66[
      ifelse (check-POC-radius self = FALSE or project-day >= project-duration) or (check-POC-radius self = FALSE and known-money-needed-in-camp = 0)[
        set color 15
        let coordinates NGO-move-away self
        set full-camp-knowledge FALSE
        set time-in-camp 0
        set known-money-needed-in-camp 0
        set info-progress 0
        set initial-money-needed-in-camp 0
        set reported-money-needed-in-camp 0
        set cluster-reported-money-needed-in-camp 0
        setxy (item 0 coordinates) (item 1 coordinates)
      ]
      [set project-day project-day + 1]
    ]

    if risk-willingness >= 0.67[
      ifelse (check-POC-radius self = FALSE or project-day >= project-duration)
      or (count POCgroups with [priority = TRUE] != 0 and known-money-needed-in-camp = 0)
      or (check-POC-radius self = FALSE and known-money-needed-in-camp != 0)[
        set color 15
        set info-progress 0
        let coordinates NGO-move-away self
        set initial-money-needed-in-camp 0
        ifelse count POCgroups with [priority = TRUE] != 0 and full-camp-knowledge = TRUE[
          let temp-target one-of POCgroups with [priority = TRUE]
          ask temp-target[
            set coordinates (list (xcor)(ycor))
          ]]

        [
          set full-camp-knowledge FALSE
          set time-in-camp 1
          set known-money-needed-in-camp 1
          set reported-money-needed-in-camp 1
          set cluster-reported-money-needed-in-camp 1]

        setxy (item 0 coordinates) (item 1 coordinates)
      ]
      [set project-day project-day + 1]
    ]]]

end


to-report NGO-move-away [x]; This reporter will return a location for the NGO which requests the move
                           ;1. Check other NGO locations with highest reported needs
                           ;2. Move to reported location of the highest reported needs NGO
                           ;3. Check if fake news, if so, add to tally



  if risk-willingness >= 0.67 [
    if false-information-reaction-high-risk-NGO >= random 100[
      set fake-news-actions fake-news-actions + 1
      report (list random-xcor random-ycor)
    ]]

  if risk-willingness <= 0.67 and risk-willingness >= 0.33[
    if false-information-reaction-medium-risk-NGO >= random 100[
      set fake-news-actions fake-news-actions + 1
      set known-money-needed-in-camp 0
      set reported-money-needed-in-camp 0
      set cluster-reported-money-needed-in-camp 0
      report (list random-xcor random-ycor)
    ]]

  if risk-willingness <= 0.33[
    if false-information-reaction-low-risk-NGO >= random 100[
      set fake-news-actions fake-news-actions + 1
      set reported-money-needed-in-camp 0
      set cluster-reported-money-needed-in-camp 0
      set known-money-needed-in-camp 0
      report (list random-xcor random-ycor)
    ]]


  ;This block combines all NGO's which are reporting to the asking NGO and which are in another camp
  let cluster-targets other NGOs with [cluster = [cluster] of myself and cluster-reporting = TRUE]
  let temp-targets other NGOs with [reporting = TRUE]
  let combinedset (turtle-set cluster-targets temp-targets)
  set combinedset combinedset with [(distance myself) > 3]
  set combinedset combinedset with [region = one-of regions-possible]

;  show who
;  show cluster-targets
;  show temp-targets
;  show combinedset

  let target max-one-of combinedset [reported-money-needed-in-camp]
    let xcortarget xcor
    let ycortarget ycor


 let reg regions-possible

  ifelse target != nobody
  [ask target[
    set xcortarget xcor
    set ycortarget ycor]]
  [move-to one-of patches with [region = one-of reg]]

  ;show (list xcortarget ycortarget)
  ;show target

  report  (list xcortarget ycortarget)

end


to decide-reporting
  ask NGOs[
    if project-day >= project-duration [
      set money-funded renew-funding self
      set money money + money-funded
      set cycle-accidents-previous-cycle cycle-accidents
      set cycle-accidents 0
      set funding-converted-previous-cycle funding-converted
      set funding-converted 0
      set funding-delivered-previous-cycle funding-delivered
      set funding-delivered 0
    ]]

  ifelse reporting-cycle < duration-reporting-cycle [
    set reporting-cycle reporting-cycle + 1]
  [set reporting-cycle 0
  ask NGOs[
    set reported-accidents-in-reporting-cycle 0]]

  ask NGOs[
    ifelse reporting = TRUE[
      set reporting_no 1]
    [set reporting_no 0]

    ifelse cluster-reporting = TRUE[
      set cluster-reporting_no 1]
    [set cluster-reporting_no 0]
  ]


  ask NGOs[
    ifelse cycle-accidents <= threshold-accidents-for-sharing [
      set reporting TRUE
      set cluster-reporting TRUE]
    [ifelse money >= known-money-needed-in-camp
      [set reporting FALSE
        set cluster-reporting FALSE]
      [ifelse risk-willingness < 0.33
        [let decider random 10 / 100
          if decider < chance-of-sharing-info-1
            [set reporting TRUE if decider < (0.2 + chance-of-sharing-info-1)  [set cluster-reporting TRUE]]]
      ;          ifelse random 10 < ] ; Assumption made here is that chance of information sharing is 20% higher for within cluster-sharing
        [ifelse risk-willingness  > 0.33 and risk-willingness < 0.66
          [let decider random 1.0
            if decider < chance-of-sharing-info-2
              [set reporting TRUE if decider < (0.2 + chance-of-sharing-info-2)  [set cluster-reporting TRUE]]]
          [let decider random 1.0
            if decider > chance-of-sharing-info-3 [set reporting TRUE if decider < (0.2 + chance-of-sharing-info-2) [set cluster-reporting TRUE]]]] ;Here the difference is made how willing organizations are to share information within cluster or with everyon
      ]]]
end


to transfer-aid

ask NGOs[
  ifelse info-process-time-needed > info-progress[
    set color 90
    ifelse money <= 0 [
     ; show "ran out of money"
      move-to one-of patches with [region = 1]
      set color 15
    ]

    [set money money - 0.01 * money-funded]
    ]
    [
    ifelse full-camp-knowledge = FALSE[
      gather-camp-info]
    [ ifelse money >= 0 [
      set color 95
      let money-available 0
      ifelse project-duration - project-day != 0[
        set money-available (money / (project-duration - project-day))]
      [set money-available money]
      let SS-money 0


      ;show duration-funding-cycle - funding-cycle
      ;show money-available
      ;show money

      ;This block represents the safety&security costs of NGO's

      if risk-willingness >= 0.67 [
        set money-available money-available * 0.9
        set SS-money money-available * 0.1
      ]

      if risk-willingness <= 0.67 and risk-willingness >= 0.33[
        if region-now = 1 [
          set money-available money-available * 0.75
          set SS-money money-available * 0.25
        ]

        if region-now = 2 [
          set money-available money-available * 0.5
          set SS-money money-available * 0.5
        ]

        if region-now = 3 [
          set money-available money-available * 0.25
          set SS-money money-available * 0.75
        ]
      ]

      if risk-willingness <= 0.33[
        if region-now = 1 [
          set money-available money-available * 0.8
          set SS-money money-available * 0.8
        ]

        if region-now = 2 [
          set money-available money-available * 0.7
          set SS-money money-available * 0.3
        ]

        if region-now = 3 [
          set money-available money-available * 0.5
          set SS-money money-available * 0.5
        ]
      ]

      set money-spent-SS money-spent-SS + SS-money
      set money-spent-to-POC money-spent-to-POC + money-available
      set funding-converted money-spent-to-POC / (money-spent-to-POC + money-spent-SS + 1)


      set time-in-camp time-in-camp + 1
      set money-needed-in-camp sum [money-needed] of POCgroups in-radius 3
      set known-money-needed-in-camp money-needed-in-camp

      if reporting = TRUE [
        set money-needed-in-camp sum [money-needed] of POCgroups in-radius 3
        set reported-money-needed-in-camp known-money-needed-in-camp
      ]

      if cluster-reporting = TRUE [
        set money-needed-in-camp sum [money-needed] of POCgroups in-radius 3
        set cluster-reported-money-needed-in-camp known-money-needed-in-camp
      ]



      ifelse any? POCgroups in-radius 3 and money-needed-in-camp > 0[
        let money-received money-available
        let recipients POCgroups in-radius 3

        let final-recipients one-of recipients
        let capacity (size-of-organisation / 36) * count(POCgroups)


        ifelse count(recipients) < capacity
        [set final-recipients recipients]
        [set final-recipients n-of round capacity recipients]
        ;show recipients
        let size_og capacity

        ask final-recipients[
          set money-needed money-needed - money-received / capacity
          set money-received-last-tick money-received / capacity
        ]
        set money money - money-available
        set funding-delivered funding-delivered + money-available
        set total-money-spent total-money-spent +  money-spent-SS + money-spent-to-POC
        set money-spent-last-tick money-spent-SS + money-spent-to-POC
      ]
      [set money-spent-last-tick 0

        ifelse risk-willingness > 0.66[
        move-to max-one-of POCgroups [money-needed]
        ]

        [move-to one-of patches with [region = 1]
          set color 124]]]
    [
      ;show "ran out of money"
      move-to one-of patches with [region = 1]
      set color 15
    ]]]]

  ask POCgroups[
    ifelse money-needed <= 500000 [
      set shape "face happy"
      set color green
    ]
    [
     set shape "face sad"
      set color red]
  ]
end


to-report check-POC-radius [x]
  let res TRUE

  ask x[
    if not any? POCgroups in-radius 3[
      set res FALSE
    ]
  ]
  report res
end


to move-NGOS-in-random-direction

  ask NGOS[
    ifelse risk-willingness < 0.33 or (risk-willingness > 0.33 and risk-willingness < 0.66 and info-region2 < 10)[
      set regions-possible [1]]
    [ifelse (risk-willingness > 0.33 and risk-willingness < 0.66 and info-region2 > 10) [set regions-possible [1 2]]
      [set regions-possible (list 1 2 3)]]
    if risk-willingness > 0.66 [
      set regions-possible (list 1 2 3)]

    let reg regions-possible

    move-to one-of patches with [region = one-of reg]

  ]

end


to transfer-info
  ask NGOS with [risk-willingness > 0.66][
    if info-region3 > 2 and random 1 > chance-of-sharing-info-3[
      ask NGOS in-radius 3 with [risk-willingness > 0.66] [
        set info-region3 info-region3 + 1
      ]]]

  ask NGOS with [risk-willingness > 0.66 and info-region2 > 10][
    if random 1 > chance-of-sharing-info-3[
      ask NGOS with [risk-willingness > 0.33 and risk-willingness < 0.66][
        set info-region2 info-region2 + 10
      ]]]


  ask NGOs[
   if full-camp-knowledge = TRUE[
   let neighbours other NGOs in-radius 6
   let temp known-money-needed-in-camp
   ask neighbours[
     set known-money-needed-in-camp temp
     set full-camp-knowledge TRUE
     set initial-money-needed-in-camp known-money-needed-in-camp
   ]]]
end


to gather-camp-info
  set region-now region
  let money-in-camp []
  set money-in-camp [money-needed] of POCgroups in-radius 3
  set money-needed-in-camp sum money-in-camp
  set color 45
  let days-needed 7 + random 23 ;Making a camp assessment takes between 7 and 30 days. There is no relation assumed between the type of NGO and the time it takes to make the assessment.


  if money-needed-in-camp > 0 and time-in-camp > 0.25 * days-needed[
    set known-money-needed-in-camp 0.25 * money-needed-in-camp
    set money money - 0.01 * money-funded ;costs of making an assessment is 1% of money per day. Since money is awarded monthly, there is no large saving in any of the NGO's
    set initial-money-needed-in-camp known-money-needed-in-camp
  ]

  if money-needed-in-camp > 0 and time-in-camp > 0.5 * days-needed[
    set known-money-needed-in-camp 0.5 * money-needed-in-camp
    set money money - 0.01 * money-funded
    set initial-money-needed-in-camp known-money-needed-in-camp
  ]

  if money-needed-in-camp > 0 and time-in-camp > 0.75 * days-needed[
    set known-money-needed-in-camp 0.75 * money-needed-in-camp
    set money money - 0.01 * money-funded
    set initial-money-needed-in-camp known-money-needed-in-camp
  ]

  if money-needed-in-camp > 0 and time-in-camp > days-needed[
    set known-money-needed-in-camp money-needed-in-camp
    set initial-money-needed-in-camp money-needed-in-camp
    set full-camp-knowledge TRUE
  ]

    set time-in-camp time-in-camp + 1

    if reporting = TRUE [
      set reported-money-needed-in-camp known-money-needed-in-camp ;here a delay needs to be put into place
    ]
end

to setup-regions [ num-regions ]
  ; First, draw some dividers at the intervals reported by `region-divisions`:
  foreach region-divisions num-regions draw-region-division
  ; Store our region definitions globally for faster access:
  set region-boundaries calculate-region-boundaries num-regions
  ; Set the `region` variable for all patches included in regions:
  let region-numbers (range 1 (num-regions + 1))
  (foreach region-boundaries region-numbers [ [boundaries region-number] ->
    ask patches with [ pxcor >= first boundaries and pxcor <= last boundaries ] [
      set region region-number
    ]
  ])
end

to-report calculate-region-boundaries [ num-regions ]
  ; The region definitions are built from the region divisions:
  let divisions region-divisions num-regions
  ; Each region definition lists the min-pxcor and max-pxcor of the region.
  ; To get those, we use `map` on two "shifted" copies of the division list,
  ; which allow us to scan through all pairs of dividers
  ; and built our list of definitions from those pairs:
  report (map [ [d1 d2] -> list (d1 + 1) (d2 - 1) ] (but-last divisions) (but-first divisions))
end

to-report region-divisions [ num-regions ]
  ; This procedure reports a list of pxcor that should be outside every region.
  ; Patches with these pxcor will act as "dividers" between regions.
  report n-values (num-regions + 1) [ [n] ->
    [ pxcor ] of patch (min-pxcor + (n * ((max-pxcor - min-pxcor) / num-regions))) 0
  ]
end

to draw-region-division [ x ]
  ; This procedure makes the division patches grey
  ; and draw a vertical line in the middle. This is
  ; arbitrary and could be modified to your liking.
  ask patches with [ pxcor = x ] [
    set pcolor grey + 1.5
  ]
  create-turtles 1 [
    ; use a temporary turtle to draw a line in the middle of our division
    setxy x max-pycor + 0.5
    set heading 0
    set color grey - 3
    pen-down
    forward world-height
    set xcor xcor + 1 / patch-size
    right 180
    set color grey + 3
    forward world-height
    die ; our turtle has done its job and is no longer needed
  ]
end

to keep-in-region [ which-region ] ; turtle procedure

  ; This is the procedure that make sure that turtles don't leave the region they're
  ; supposed to be in. It is your responsibility to call this whenever a turtle moves.
  if region != which-region [
    ; Get our region boundaries from the global region list:
    let region-min-pxcor first item (which-region - 1) region-boundaries
    let region-max-pxcor last item (which-region - 1) region-boundaries
    ; The total width is (min - max) + 1 because `pxcor`s are in the middle of patches:
    let region-width (region-max-pxcor - region-min-pxcor) + 1
    ifelse xcor < region-min-pxcor [ ; if we crossed to the left,
      set xcor xcor + region-width   ; jump to the right boundary
    ] [
      if xcor > region-max-pxcor [   ; if we crossed to the right,
        set xcor xcor - region-width ; jump to the left boundary
      ]
    ]
  ]

end

to setup-possible-regions ;reuse this piece of code to allow more regions possible if there is more information on a region

  ask NGOS[
    set regions-possible 1

    ifelse risk-willingness < 0.33 or (risk-willingness > 0.33 and risk-willingness < 0.66 and info-region2 < 10)[
      set regions-possible [1]]
    [ifelse (risk-willingness > 0.33 and risk-willingness < 0.66 and info-region2 > 10) [set regions-possible [1 2]]
      [set regions-possible (list 1 2 3)]]
    if risk-willingness > 0.66 [
      set regions-possible (list 1 2 3)]
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
1376
753
1832
1210
-1
-1
13.6
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

CHOOSER
42
38
180
83
number-of-regions
number-of-regions
1 2 3
2

BUTTON
1259
757
1323
790
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1185
756
1248
789
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
43
107
281
140
number-of-POCs-per-region
number-of-POCs-per-region
0
50
20.0
1
1
POCs
HORIZONTAL

SLIDER
43
148
248
181
number-of-NGOs
number-of-NGOs
0
10
3.0
1
1
NIL
HORIZONTAL

CHOOSER
36
190
174
235
risk-distribution
risk-distribution
"uniform" "risk avoiding" "risk taking"
1

PLOT
1508
175
1762
325
Total money needed by POCS in region 3
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [money-needed] of POCgroups with [region = 3]"

PLOT
1246
172
1498
322
Money needed by POCS in region 2
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [money-needed] of POCgroups with [region = 2]"

PLOT
1249
12
1502
162
Money needed by POCS in region 1
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [money-needed] of POCgroups with [region = 1]"

SLIDER
31
382
319
415
chance-of-sharing-info-1
chance-of-sharing-info-1
0
0.8
0.51
0.01
1
NIL
HORIZONTAL

SLIDER
32
338
212
371
displacement-rate
displacement-rate
0
10
8.0
1
1
NIL
HORIZONTAL

SLIDER
32
300
218
333
threshold-dangerous
threshold-dangerous
0
30
10.0
1
1
ticks
HORIZONTAL

PLOT
1509
10
1709
160
Humanitarian accidents
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Accidents" 1.0 0 -16777216 true "" "plot (sum [accidents] of NGOS / ticks)"

SLIDER
29
517
301
550
threshold-accidents-for-sharing
threshold-accidents-for-sharing
0
20
10.0
1
1
accidents
HORIZONTAL

SLIDER
30
561
259
594
threshold-money-for-sharing
threshold-money-for-sharing
0
200000
16000.0
1000
1
NIL
HORIZONTAL

SLIDER
30
421
318
454
chance-of-sharing-info-2
chance-of-sharing-info-2
0
0.8
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
31
461
319
494
chance-of-sharing-info-3
chance-of-sharing-info-3
0
0.8
0.4
0.1
1
NIL
HORIZONTAL

CHOOSER
37
245
204
290
War-type
War-type
"Civil war" "International War" "Civil Violence" "No violence" "Peacekeeping mission"
2

SLIDER
334
451
623
484
false-information-reaction-high-risk-NGO1
false-information-reaction-high-risk-NGO1
1
15
15.0
1
1
%
HORIZONTAL

SLIDER
345
525
653
558
false-information-reaction-medium-risk-NGO1
false-information-reaction-medium-risk-NGO1
5
30
10.0
1
1
%
HORIZONTAL

SLIDER
345
565
630
598
false-information-reaction-low-risk-NGO1
false-information-reaction-low-risk-NGO1
0
30
8.0
1
1
%
HORIZONTAL

INPUTBOX
336
109
491
169
info-lowering-speed
3.0
1
0
Number

SLIDER
396
643
602
676
initial-funding
initial-funding
4000000000
8000000000
6.2E9
10000000
1
Dollar
HORIZONTAL

INPUTBOX
222
189
377
249
duration-reporting-cycle
15.0
1
0
Number

MONITOR
811
78
907
123
NIL
reporting-cycle
17
1
11

SLIDER
147
643
341
676
threshold-danger-of-region
threshold-danger-of-region
0
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
63
708
322
741
threshold-days-without-accident
threshold-days-without-accident
30
150
30.0
1
1
days
HORIZONTAL

SLIDER
65
751
299
784
days-with-NGOS-before-safe
days-with-NGOS-before-safe
10
60
15.0
1
1
days
HORIZONTAL

PLOT
1368
496
1628
718
Reporting
true/false
No. reporting NGO's
0.0
3.0
0.0
50.0
true
true
"set-plot-x-range 0 2\nset-plot-y-range 0 count NGOs\nset-histogram-num-bars 2" ""
PENS
"Number of NGO's" 1.0 1 -2139308 true "" "histogram [reporting_no] of NGOs"

PLOT
1639
496
1868
718
Cluster reporting
true/false
No. reporting NGO's
0.0
10.0
0.0
10.0
true
true
"set-plot-x-range 0 2\nset-plot-y-range 0 count NGOs\nset-histogram-num-bars 2" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [reporting_no] of NGOs"

PLOT
701
327
1014
559
Total money needed
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [money-needed] of POCgroups"

PLOT
1022
327
1359
563
Total money NGO's
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [money] of NGOs"

PLOT
1372
339
1572
489
Fake news actions
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [fake-news-actions] of NGOs"

TEXTBOX
606
21
756
91
Legend NGO's:\nYellow = Gathering camp info\nBlue = Transferring aid\nPurple = Idle\nRed = No spendable money
11
0.0
0

MONITOR
721
191
905
236
Percentage reported accidents
(total-reported-accidents / ((total-reported-accidents + total-unreported-accidents + 1)))
17
1
11

MONITOR
551
143
699
188
NIL
total-reported-accidents
17
1
11

MONITOR
544
199
706
244
NIL
total-unreported-accidents
17
1
11

MONITOR
549
247
766
292
Accidents in safe region
accidents-in-safe-region
0
1
11

SLIDER
27
603
315
636
threshold-danger-days-without-NGOs
threshold-danger-days-without-NGOs
5
20
10.0
1
1
days
HORIZONTAL

INPUTBOX
368
714
523
774
initial-turn-over-rate-input
1.0
1
0
Number

CHOOSER
11
840
227
885
effectiveness-policy-turnover-rate
effectiveness-policy-turnover-rate
-1 -0.5 0 0.5 1
0

PLOT
1106
573
1306
723
Turn-over-rates
Turnover rate
Number of NGO's
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 1\nset-plot-y-range 0 1.5\nset-histogram-num-bars count NGOs" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [initial-turnover-rate] of NGOs"

INPUTBOX
511
829
666
889
information-process-time
4.0
1
0
Number

PLOT
892
572
1092
722
Information processing time needed
Days
NGO's
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 10\nset-plot-y-range 0 7\nset-histogram-num-bars count NGOs" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [info-process-time-needed] of NGOs"

PLOT
1581
334
1781
484
Total time spent processing information
Days
Total days spent processing
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [total-processing-time] of NGOs"

MONITOR
788
258
988
303
Percentage time spent processing
sum [total-processing-time] of NGOs / count NGOS / ticks
2
1
11

MONITOR
742
131
897
176
Average daily incidents
sum [humanitarians-killed] of POCgroups / ticks
2
1
11

BUTTON
1115
756
1178
789
Go
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

PLOT
916
26
1223
240
Needs met
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [money] of NGOs / sum [money-needed] of POCgroups"

MONITOR
808
27
909
72
Percentage met
sum [money] of NGOs / sum [money-needed] of POCgroups
3
1
11

INPUTBOX
696
613
851
673
minimum-project-duration
292.0
1
0
Number

INPUTBOX
714
709
869
769
maximum-project-duration
438.0
1
0
Number

MONITOR
531
372
633
417
Project duration
mean [project-duration] of NGOs
0
1
11

MONITOR
328
366
514
411
average risk level of NGOs
mean [risk-willingness] of NGOs
17
1
11

@#$#@#$#@
## WHAT IS IT?

This model represents how NGO's deliver aid to POC's (Persons of Concern) in crisis situation. 

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

Partly based on Uri Wilensky's division of parts model. 
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

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

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
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="S1P1" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk avoiding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="292"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="438"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs-per-region">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S1P2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-taking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S1P3" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S1P4" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-taking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="292"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="438"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S2P4" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-taking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="292"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="438"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S2P3" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S2P2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-taking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S2P1" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-avoiding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="292"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="438"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S3P1" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-avoiding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="292"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="438"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S3P2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-taking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S3P3" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S3P4" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-taking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="292"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="438"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S4P1" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-avoiding&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="292"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="438"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S4P2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-taking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S4P3" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="S4P4" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1825"/>
    <metric>sum [funding-delivered] of NGOs</metric>
    <metric>count POCgroups with [any? NGOs in-radius 3]</metric>
    <metric>sum [money-spent-to-poc] of NGOs / (sum [money-spent-to-poc] of NGOs + sum [money-spent-ss] of NGOs + 1)</metric>
    <metric>sum [total-processing-time] of NGOs</metric>
    <metric>sum [fake-news-actions] of NGOs</metric>
    <metric>sum [accidents] of NGOs</metric>
    <metric>((total-reported-accidents) / (total-reported-accidents + total-unreported-accidents + 1))</metric>
    <metric>accidents-in-safe-region</metric>
    <metric>sum [money-needed] of POCgroups</metric>
    <metric>sum [money] of NGOs</metric>
    <enumeratedValueSet variable="information-process-time">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-distribution">
      <value value="&quot;risk-taking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-reporting-cycle">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-project-duration">
      <value value="292"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-project-duration">
      <value value="438"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="effectiveness-policy-turnover-rate">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-POCs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-turn-over-rate-input">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="displacement-rate">
      <value value="1"/>
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

fire
1.0
-0.2 1 1.0 0.0
0.0 1 1.0 0.0
0.2 1 1.0 0.0
link direction
true
1
Line -7500403 false 150 150 90 180
Line -7500403 false 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@

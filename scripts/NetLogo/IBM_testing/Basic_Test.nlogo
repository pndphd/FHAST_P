;; This is the altha program for basic testing of loading data, fish movement,
;; making basic outputs.

;; Load Extenshions
extensions [
  csv      ; The extenshion the allows for csv IO
  palette  ; Allows the use of more palets like ColorBrewer
]

;-------------------------------------
; Define veriabels
;-------------------------------------
;; Make global and agent variables
globals [
  resolution_factor   ; A factor used to control how large the display can get
  x_flow              ; The location index for the x position of a cell read in from the flow CSV
  y_flow              ; The location index for the y position of a cell read in from the flow CSV
  x_shape             ; The location index for the x position of a cell read in from the shape CSV
  y_shape             ; The location index for the y position of a cell read in from the shape CSV
  area_column         ; The location index for area of a cell read in from the flow CSV
  value_column        ; The location index for value of a cell read in from the flow CSV
  cover_column        ; The location index for cover of a cell read in from the flow CSV
  zone_column         ; The location index for zone of a cell read in from the flow CSV
  input_flow_csv      ; The variabel to hole the flow csv
  input_shape_csv     ; The variabel to hole the shape csv
  resolution          ; The grid resolution in meters
  flow_values         ; The list of flow values for which we have rasters
  today_index         ; The flow index for each day
  wet_patches         ; A list of patches that have water in them each day
  max_depth           ; The max depth listed in any raster
  reach_start         ; The upper most reach distance
  reach_end           ; The bottom reach distance
  top_patches         ; The patches that are closest to the top and are wet
  exits               ; A list of fish exiting the reach
  strands             ; A list of fish stranding
]

;; Defien variabels that the patches own
patches-own [
  area                ; The true area of the patch (not the area displayed here)
  today_depth         ; The depth of the patch on this day
  yesterday_depth     ; The depth of the patch on the previous day.
  depths              ; A list of all the depths at the set flows in the cell
  cover               ; The cover value for the cell
  zone                ; The zone value for the cell (just a made up thing)
  value               ; The value value for the cell (just a made up thing)
  ;; Parameters used as part of the valid patch logic for fish
  visited?            ; Used to identify if the pathfinding has visited the patch
  path_to_here_cost   ; Used to store the cost to move to this patch for pathfinding
  previous_patch      ; The previous patch in the path from pathfinding
  ;; Example values to show how parameters can be calculated and used in the pathfinding
  depth_to_here_cost  ; Example parameter that could be used to have fish avoid deep water. Something similar could be done for predation risk for example.
]

;; Define variabels that the fish own
turtles-own [
  exit_status          ; If the fish is exiting this turn
  strand_status        ; If the fish is getting stranded this turn
  max_move_dist        ; The max distance a fish can move in m
]

;-------------------------------------
; Setup Actions
;-------------------------------------
;; The setup button actions
to setup
  clear-all                           ; Clear the entire space
  set-default-shape turtles "dot"     ; Set teh default turtel shape to dot
  set resolution 20                   ; Set the grid resolution in m
  set resolution_factor 600           ; Set the resolution factor
  set input_flow_csv csv:from-file "../../../temporary/NetLogo/Depth_Data_Input_20_500.csv"   ; Read in the depth CSV
  set input_shape_csv csv:from-file "../../../temporary/NetLogo/Shape_Data_Input_20_500.csv"  ; Read in the shape file
  ;; Get the indexes for the columns in each file
  set x_flow (position "x" (item 0 input_flow_csv))
  set y_flow (position "y" (item 0 input_flow_csv))
  set area_column (position "area" (item 0 input_flow_csv))
  set value_column (position "Value" (item 0 input_shape_csv))
  set zone_column (position "Zones" (item 0 input_shape_csv))
  set cover_column (position "Cover" (item 0 input_shape_csv))
  set x_shape (position "lat_dist" (item 0 input_shape_csv))
  set y_shape (position "distance" (item 0 input_shape_csv))
  ;; Set the size of the world
  set_world_size
  set_flow_values
  set_patch_flow_values
  set_patch_shape_values
  ; initialize patch data for pathfinding
  ask patches [
    set visited? false
    set previous_patch nobody
    set path_to_here_cost -1
    set depth_to_here_cost -1
  ]
  reset-ticks
end

;; Establish a world size based the input files
to set_world_size
  ;; get necessary information to set up workld size
  let x_values (map [n -> item x_flow n ] input_flow_csv)  ; Read in all the x positions
  let y_values (map [n -> item y_flow n ] input_flow_csv)  ; Read in all the y positions
  let x_max max x_values                                   ; Get the max x value
  let x_min min x_values                                   ; Get the min x value
  let y_max max y_values                                   ; Get the max y value
  ;; Resize the workld and patchs
  resize-world (x_min / (resolution)) (x_max / (resolution)) 0 (y_max / (resolution))
  ; set the patch size to be 1 or larger (if resonable)
  set-patch-size max (list 1 ((resolution / y_max) * resolution_factor))
end

;; Get the flow values form the depth file
to set_flow_values
  let flow_header item 0 input_flow_csv
  ; Get the first row of the CSV
  let flow_columns filter [ s -> member? "mean.D" s ] flow_header    ; Get only column headders with mean in the name
  let flow_values_str map [ s -> remove "mean.D" s ] flow_columns  ; Take out the prefix
  set flow_values map [ s -> read-from-string s ] flow_values_str  ; Covnert the strings to number values
end

;; Assign each of the cells all it's flow values
to set_patch_flow_values
  ;; Assign all the patches list of depths to have -9999 to rep no data
  ask patches [
    set depths (map [n -> -9999] flow_values)]
  ;; Populate each patch with depth data
  ;; Take each line in the csv
  foreach (but-first input_flow_csv) [n ->
    ; move over each patch using its location and value
    ask patch ((item x_flow n) / (resolution)) ((item y_flow n) / (resolution)) [
      ; set the area value
      set area (item area_column n)
      let counter 0
      ; assign all the flow values and an index for them
      foreach flow_values [m ->
        ; Use the flow values combined with the data type to get the depth values
        let m_column (position (word "mean.D" m) (item 0 input_flow_csv))
        let depth_input (item m_column n)
        set depths (replace-item counter depths depth_input) ; replace the depth place holder with the actual depth
        set max_depth max (list depth_input max_depth)       ; Check to see if this is a new max depth
        set counter counter + 1]
    ]
  ]
  set today_index floor (length flow_values / 2)     ; Set the index to a starting value
end

;; Do a simmilar procedure for the shape file inputs
to set_patch_shape_values
  ;; Now do the same but for the shapes
  foreach (but-first input_shape_csv) [n ->
    ask patch ((item x_shape n) / (resolution)) ((item y_shape n) / (resolution)) [
      set cover (item cover_column n)
      set zone (item zone_column n)
      set value (item value_column n)
    ]
  ]
end

;-------------------------------------
; Run Actions
;-------------------------------------
;; Set waht teh "go" button does
to go
  set_flow
  set_boundaries
  hatch_fish
  move_fish
  exit_fish
  strand_fish
  tick
end

;; Select a flow and set the depths and later velocity
to set_flow
  ; Select a flow index close to the previous one
  let new_index (ifelse-value
    today_index = 0 [today_index + 1]                       ; If previous was min make 1 larger
    today_index = length(flow_values) - 1 [today_index - 1] ; If max make on smaller
    [random 3 - 1 + today_index])                           ; Else add between -1 and 1 to it
  set today_index new_index

  ;; Color the patches based on user selection
  ;; Only color patches with depth
  ask patches [
    set pcolor black
    set yesterday_depth today_depth
    set today_depth (item today_index depths)
    (ifelse
      background_display = "Depth" [
        if (today_depth >= 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "Blues" 9 today_depth 0 (max_depth)]
      ]
      background_display = "Cover" [
        if (today_depth >= 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "YlGnBu" 9 cover 0 1]
      ]
      background_display = "Zone" [
        if (today_depth >= 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "PuRd" 9 zone 0 1]
      ]
      background_display = "Value" [
        if (today_depth >= 0)[
          set pcolor palette:scale-gradient palette:scheme-colors "Sequential" "BuGn" 9 value 0 1]
      ]
    )
  ]

end

;; This will set some boundaries of the reach based on flow
to set_boundaries
  set wet_patches patches with [today_depth > 0]         ; Select patches with water
  set reach_start max [pycor] of wet_patches             ; Get the highest patch distance value
  set reach_end min [pycor] of wet_patches               ; Get the lowest patch distance value
  set top_patches wet_patches with [pycor = reach_start] ; Get the highest patchs that are wet
end

;; Add new fish to the reach
to hatch_fish
  create-turtles (random 2) [set size 10
      move-to one-of top_patches     ; Place the turtle in a top patch
      set color red
      set max_move_dist 200          ; set a max move distance in meters
      set exit_status 0]
end

;; Move fish to a new down stream wet cell
to move_fish
  clear-drawing
  ask turtles [
    let move_dist max_move_dist / resolution ; get the max move distance in pixil units

    let open-patches (ifelse-value
    today_depth > 0 [find_possible_destinations self move_dist] ; Normal move logic
    [find_possible_destinations_avoid_stranding self move_dist]) ; On dry land, move to avoid stranding

    ; todo- destination selection logic. If needed we can calculate additional information
    ; during the destination search (like the example depth_to_here_cost which I added because
    ; it was easily calculated as an example).
    let destination one-of open-patches

    ; Draw the path taken to get to the destination.
    let curpatch destination
    ; The patches know the path the fish took to this location in reverse, so we
    ; actually start at the destination and draw backwards before jumping back to
    ; the end.
    move-to destination
    pen-down
    while [curpatch != nobody]
    [
      move-to curpatch
      set curpatch [previous_patch] of curpatch
    ]
    pen-up

    ; Move to the destination.
    move-to destination

    let location [pycor] of patch-here
    ; if you are at the farthest downstream patch
    if (location - reach_end < move_dist) [set exit_status 1]]
  set exits turtles with [exit_status = 1]
  set strands turtles with [strand_status = 1]
end

to exit_fish
  ask exits [die]
end

to strand_fish
  ask strands [die]
end

; Clear all of the values used in the pathfinding algorithm
to clear_pathfinding_data
  ask patches with [visited?]
  [
    set visited? false
    set previous_patch nobody
    set path_to_here_cost -1
    set depth_to_here_cost -1
  ]
end

; Calculage the cost to move to a new patch.
to-report calculate_move_cost [from_patch]
  ; assume the cost == the distance
  ; calculate distance instead of using 1 to account for diagonals
  ; todo - is this how cost should be calculated? maybe energy expenditure estimate?
  report distance from_patch
end

; Store values relevant for the pathfinding. Use the from_patch and the provided cost
; to calculate values for myself.
to store_pathfinding_patch_values [from_patch cost]
  ; Set the path data for this patch (costs, previous patch, visited)
  set previous_patch from_patch
  set path_to_here_cost cost
  set depth_to_here_cost [depth_to_here_cost] of from_patch + today_depth
  set visited? true
end

; Find possible destinations the fish can swim to within the given move_dist.
; Also, to mimic the logic this replaced, it only gives downstream patches as valid destinations.
; This downstream only logic can cause fish to get stuck when there is nothing immediately downstream of them.
to-report find_possible_destinations [fish move_dist]
  ; If a patch was visited, clear out the data so pathfinding can work.
  clear_pathfinding_data
  let destinations (list)
  ; We start looking for valid destinations with the fish's current location.
  let to_visit (list [patch-here] of fish)
  ask [patch-here] of fish
  [
    set visited? true
    set path_to_here_cost 0
    set depth_to_here_cost 0
  ]
  while [length to_visit > 0]
  [
    ; Pull the patch we're looking at off the to_visit list.
    let cur_patch first to_visit
    set to_visit but-first to_visit
    ask cur_patch [
      ask neighbors [
        ; validate neighbor is wet
        if (today_depth > 0)
        [
          let cost [path_to_here_cost] of cur_patch + calculate_move_cost cur_patch
          ; If this is the first time being visited or this path was cheaper than a previous path
          if (visited? = false) or (cost < path_to_here_cost)
          [
            store_pathfinding_patch_values cur_patch cost
            if (cost < move_dist)
            [
              ; If we've moved too far, don't bother adding to the to_visit list, but
              ; if we still have distance the fish can travel than keep going.
              set to_visit lput self to_visit
              ; Note that we allow fish to go upstream to try find a downstream option to avoid getting stuck.
              ; This is why we don't check if the new node is downstream until the end here.
              ; Also note- fish still get stuck, but this does help.
              ; todo- should we enforce downstream only, or should that be part of the destination selection process?
              if (pycor <= [ycor] of fish) and (not member? self destinations)
              [
                ; If this is downstream and all of the previous checks pass,
                ; then add as a valid destination
                set destinations lput self destinations
              ]
            ]
          ]
        ]
      ]
    ]
  ]

  if (empty? destinations)
  [
    ; If no destination was found, make staying in place an option.
    set destinations lput ([patch-here] of fish) destinations
  ]
  report destinations
end

; Find possible destinations the fish can swim to within the given move_dist.
; Find wet pathches that were possible to reach based on yesterday's
; water levels.
to-report find_possible_destinations_avoid_stranding [fish move_dist]
  ; If a patch was visited, clear out the data so pathfinding can work.
  clear_pathfinding_data
  let destinations (list)
  ; We start looking for valid destinations with the fish's current location.
  let to_visit (list [patch-here] of fish)
  ask [patch-here] of fish
  [
    set visited? true
    set path_to_here_cost 0
    set depth_to_here_cost 0
  ]
  while [length to_visit > 0]
  [
    ; Pull the patch we're looking at off the to_visit list.
    let cur_patch first to_visit
    set to_visit but-first to_visit
    ask cur_patch [
      ask neighbors [
        ; Check if the neighbor was wet
        if (yesterday_depth > 0)
        [
          let cost [path_to_here_cost] of cur_patch + calculate_move_cost cur_patch
          ; If this is the first time being visited or this path was cheaper than a previous path
          if (visited? = false) or (cost < path_to_here_cost)
          [
            store_pathfinding_patch_values cur_patch cost
            if (cost < move_dist)
            [
              ; If we've moved too far, don't bother adding to the to_visit list, but
              ; if we still have distance the fish can travel than keep going.
              set to_visit lput self to_visit
              ; Is the node wet today, if so, it's a valid destination.
              if (today_depth > 0) and (not member? self destinations)
              [
                set destinations lput self destinations
              ]
            ]
          ]
        ]
      ]
    ]
  ]

  if (empty? destinations)
  [
    ; This fish has nowhere to go and is stuck on dry land, strand them.
    ask fish [set strand_status 1]
    ; We still add a destination here, since the code that calls this expects one.
    set destinations lput ([patch-here] of fish) destinations
  ]
  report destinations
end
@#$#@#$#@
GRAPHICS-WINDOW
180
60
239
755
-1
-1
1.0
1
10
1
1
1
0
0
0
1
-25
25
0
685
1
1
1
ticks
30.0

BUTTON
35
60
100
93
SETUP
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
35
95
100
128
GO
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

BUTTON
35
130
100
163
STEP
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

CHOOSER
35
175
145
220
background_display
background_display
"Depth" "Cover" "Zone" "Value"
2

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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

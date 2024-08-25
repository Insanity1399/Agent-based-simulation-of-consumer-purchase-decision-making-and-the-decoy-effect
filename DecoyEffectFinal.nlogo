globals [
  brands            ;; list of brand names
  qualities         ;; list of qualities for each brand
  prices            ;; list of prices for each brand
  interaction-radius ;; current radius of interaction
]

turtles-own [
  price-sensitivity
  quality-sensitivity
  susceptibility
  follower-tendency
  motivation        ;; calculated motivation for each brand
  influ_out         ;; agent's influence on other agents
  influence        ;; influence perceived from other interacting agents
  k                ;; the value k is based on an agent's socio-economic attributes(income)
  L                ;;the value L is based on an agent's socio-economic attributes(income)
  Income
]

patches-own [
  advertising  ;; advertising intensity for each patch
]


;; Setup procedure
to setup
  clear-all
  clear-output
  setup-brands
  setup-patches
  create-turtles 1000 [
    setxy random-xcor random-ycor
  ]
  setup-turtle
  set interaction-radius 6  ;; Initialize the interaction radius
  reset-ticks
end

;;Creates the initial virtual market of consumer with respect to Brand A and Brand B (before the introduction of the decoy product)
to initial-market
  ask turtles [
    calculate-influ-in
    initial-choose-brand
  ]
  let red-color-turtles turtles with [ color = red ]
  output-print (word "red turtles" red-color-turtles)
  let yellow-color-turtles turtles with [ color = yellow ]
  output-print (word "yellow turtles" yellow-color-turtles)
end


to-report random-normal-in-range [M stddev minima maxima]
  let value random-normal M stddev
  while [value < minima or value > maxima] [
    set value random-normal M stddev
  ]
  report value
end

to-report random-float-between-range [minima maxima]
  report minima + (random-float (maxima - minima))
end

;; Procedure to calculate the Price sensitivity of each turtle
to-report Price_Sen [Price len]
  let p-ave ((item 0 prices + item 1 prices) / len)
  let PS  (-(alpha ^ (Price - p-ave))) + k
  report PS
end

;; Procedure to calculate the Quality sensitivity of each turtle
to-report Quality_Sen [Quality len]
  let Q_ave ((item 0 qualities + item 1 qualities) / len)
  let QS (beta ^ abs (Quality - Q_ave)) + L
  report QS
end

;; Initialize brands and their attributes
to setup-brands
  set brands ["BrandA" "BrandB"]
  set qualities (list QualityA QualityB)
  set prices (list PriceA PriceB)
end

;;Procedure to assign the advertising intensity for each region the agent is living in
to setup-patches
  ask patches [
    set advertising (list (random-float 100.0) (random-float 100.0))  ;; Assign random advertising intensities for each brand
  ]
end

;;Procedure to assign a constant to the agent's price sensitivity parameter according to the income
to-report map-k [inc]
  report -100 + (inc / 10)
end

;;Procedure to assign a constant to the agent's quality sensitivity parameter according to the income
to-report map-l [inc]
  report inc / 10
end


;; Initialize turtle properties
to setup-turtle
  ask turtles[
    set shape "person"  ;; Set the shape of the turtle to "person"
    set size 1  ;; Adjust size if necessary
    set color green
    set Income random-normal-in-range 600 75 100.0 1000.0
    set k map-k Income
    set L map-l Income
    let pA item 0 prices
    let pB item 1 prices
    let qA item 0 qualities
    let qB item 1 qualities
    set price-sensitivity (list Price_Sen pA length prices Price_Sen pB length prices)
    set quality-sensitivity (list Quality_Sen qA length prices Quality_Sen qB length qualities)
    set susceptibility random-normal-in-range 55 20 0.0 100.0
    set follower-tendency random-normal-in-range 55 20 0.0 100.0
    set influ_out random-normal-in-range 30 20 0.0 100.0
  ]
end


;; Each turtle initially chooses between Brand A and Brand B
to initial-choose-brand
  let pA item 0 prices
  let qA item 0 qualities
  let adA item 0 advertising  ;; Get the advertising intensity for Brand A of the current patch
  let influenceA influence

  let pB item 1 prices
  let qB item 1 qualities
  let adB item 1 advertising  ;; Get the advertising intensity for Brand B of the current patch

  let motivationA (item 0 price-sensitivity * pA) +
                    (item 0 quality-sensitivity * qA) +
                    (susceptibility * adA) +
                    (follower-tendency * influenceA)
  let motivationB (item 1 price-sensitivity * pB) +
                    (item 1 quality-sensitivity * qB) +
                    (susceptibility * adB) +
                    (follower-tendency * influenceA)

  set motivation (list motivationA motivationB)
  let highest-motivation max motivation
  let initial-choice highest-motivation
  if initial-choice = motivationA [
    set color red  ;; If the agent's choice is Brand A netlogo sets the agent's color to red
  ]
  if initial-choice = motivationB [
    set color yellow  ;; If the agent's choice is Brand B netlogo sets the agent's color to yellow
  ]
end


;; Calculate the influence of neighbouring agents for each turtle
to calculate-influ-in
    set influence sum [ influ_out ] of turtles in-radius interaction-radius
end

;; Introducing the new decoy product to the market
to decoy-effect
  set brands (list "BrandA" "BrandB" "Decoy Product")
  set qualities (list QualityA QualityB decoy-quality)
  set prices (list priceA priceB decoy-price)
  ask patches [
    set advertising (list (random-float 100.0) (random-float 100.0) (random-float 100.0))
  ]
  ask turtles [
    set price-sensitivity (list Price_Sen item 0 prices length prices Price_Sen item 1 prices length prices Price_Sen item 2 prices length prices)
    set quality-sensitivity (list Quality_Sen item 0 qualities length qualities Quality_Sen item 1 qualities  length qualities Quality_Sen item 2 qualities length qualities)
    set influ_out random-normal-in-range 30 20 0.0 100.0
    calculate-influ-in
    calculate-motivation-after-decoy
   ]
  output-print (word "After Decoy Product is introduced")
  let red-color-turtles turtles with [ color = red ]
  output-print (word "red turtles" red-color-turtles)
  let yellow-color-turtles turtles with [ color = yellow ]
  output-print (word "yellow turtles" yellow-color-turtles)
end

;;Calculating the new motivation for each turtle after the introduction of the decoy product to the market
to calculate-motivation-after-decoy
  let motivationA (item 0 price-sensitivity * item 0 prices ) + (item 0 quality-sensitivity + item 0 qualities) + (susceptibility * item 0 advertising) + (follower-tendency * influence)
  let motivationB (item 0 price-sensitivity * item 1 prices ) + (item 0 quality-sensitivity + item 1 qualities) + (susceptibility * item 1 advertising) + (follower-tendency * influence)
  let motivationDecoy (item 0 price-sensitivity * item 2 prices ) + (item 0 quality-sensitivity + item 1 qualities) + (susceptibility * item 2 advertising) + (follower-tendency * influence)
  set motivation (list motivationA motivationB motivationDecoy)
  let highest-motivation max motivation
  let new-choice highest-motivation
  if new-choice = motivationA [
    set color red
  ]
  if new-choice = motivationB [
    set color yellow
  ]
  if new-choice = motivationDecoy [
    set color blue       ;; If the agent's choice is the Decoy product netlogo sets the agent's color to blue
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
448
-1
-1
13.0
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

BUTTON
6
13
69
46
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

SLIDER
7
60
179
93
PriceA
PriceA
0
100
90.0
1
1
NIL
HORIZONTAL

SLIDER
7
107
179
140
PriceB
PriceB
0
100
60.0
1
1
NIL
HORIZONTAL

SLIDER
7
159
179
192
QualityA
QualityA
0
100
84.0
1
1
NIL
HORIZONTAL

SLIDER
7
209
179
242
QualityB
QualityB
0
100
55.0
1
1
NIL
HORIZONTAL

INPUTBOX
8
249
163
309
alpha
1.18
1
0
Number

INPUTBOX
8
314
162
374
beta
0.9
1
0
Number

BUTTON
662
38
776
71
Initialize Market
initial-market
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
664
101
778
134
Decoy Effect
decoy-effect
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
680
175
852
208
decoy-quality
decoy-quality
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
679
223
851
256
decoy-price
decoy-price
0
100
70.0
1
1
NIL
HORIZONTAL

OUTPUT
665
268
970
478
10

@#$#@#$#@
## WHAT IS IT?
This model mimics the consumer behavior in a market with two main products, **Brand A and Brand B**, and introduces **a decoy product** to observe how it influences consumer choices. The decoy effect is a phenomenon where the introduction of a third, the decoy product changes consumer preferences between the original two options. The model demonstrates how factors like **price sensitivity, quality sensitivity, advertising influence, and social influence** are the major determinant of consumer's decision-making.


## HOW IT WORKS

The model operates through a set of rules that govern the behavior of agents (turtles), which represent individual consumers:

* **Price Sensitivity**: Each turtle has a price sensitivity parameter, which determines how much the price of a product influences its choice.
* **Quality Sensitivity**: Each turtle also has a quality sensitivity parameter, influencing its preference based on the product's quality.
* **Advertising**: Patches (regions of the environment) have different levels of advertising intensity for each brand, affecting nearby turtles' susceptibility.
* **Social Influence**: Turtles are influenced by the choices of other turtles within a certain interaction radius, simulating peer influence.
* **Motivation Calculation**: Each turtle calculates its motivation to choose a brand based on price, quality, advertising, and social influence. Initially, turtles choose between Brand A and Brand B. When the decoy product is introduced, turtles recalculate their motivations, potentially shifting their choices


## HOW TO USE IT

1.**Setup**: Click the "Setup" button to initialize the model. This will create the brands, patches, and turtles, assigning each turtle its attributes such as price sensitivity, quality sensitivity, and income.
2.**Initial-Market**: Press the "Initial-Market" button to simulate the market without the decoy product. Turtles will choose between Brand A and Brand B based on their motivations.
3.**Decoy-Effect**: Click the "Decoy-Effect" button to introduce the decoy product into the market. Turtles will re-evaluate their choices, and you can observe how their preferences shift

### Interface Tab Items:


* Setup Button: Initializes the model.
* Initial-Market Button: Simulates consumer choices between Brand A and Brand B.
* Decoy-Effect Button: Introduces the decoy product and simulates its impact on consumer behavior.
* Monitors/Plots: You can add monitors or plots to track how the number of turtles choosing each brand changes over time.

## THINGS TO NOTICE

* Observe the distribution of turtle colors after the "Initial-Market" phase, representing their initial choices (red for Brand A, yellow for Brand B).
* After clicking "Decoy-Effect," notice how the introduction of the decoy product changes the distribution, particularly if some turtles switch to the new decoy product (blue).
* Pay attention to how different turtles respond based on their income, price sensitivity, and quality sensitivity.

## THINGS TO TRY

* Experiment with different advertising intensities by modifying the setup-patches procedure to see how it affects consumer choices.
* Try varying the income distribution of turtles to understand how socio-economic factors influence their decisions.
* Adjust the interaction radius to see how social influence changes the overall behavior of the market.

## EXTENDING THE MODEL
* Add more brands or products to increase the complexity of the market.
* Introduce new parameters like brand loyalty, discounts, or promotions to see how they impact consumer choices

## NETLOGO FEATURES

* The model utilizes NetLogo's random-normal and random-float functions to assign consumer attributes like income, susceptibility, and follower tendency.
* The in-radius function is used to calculate social influence by determining which turtles are within a specific distance from each other.
* The model demonstrates how to dynamically update lists and recalculate agent motivations based on changing market conditions.

## RELATED MODELS

* Market Share: Explore related models in the NetLogo Models Library that deal with market dynamics and consumer behavior.
* Segregation: Similar to consumer choice, this model deals with how individual preferences can lead to large-scale patterns.
* Opinion Dynamics: Models how social influence shapes public opinion, akin to how social influence affects consumer decisions in this model.

## CREDITS AND REFERENCES
This model was developed to demonstrate the decoy effect in consumer behavior using Agent-Based Modeling.We used research paper as a reference[1],
[1]Zhang, T. and Zhang, D., 2007. Agent-based simulation of consumer purchase decision-making and the decoy effect. Journal of business research, 60(8), pp.912-922.
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

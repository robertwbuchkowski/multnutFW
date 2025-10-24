
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multnutFW

<!-- badges: start -->
<!-- badges: end -->

The goal of multnutFW is to analyze food web models that contain a
user-defined number of essential chemical elements at fixed proportions
of the biomass of each node.

## Installation

You can install the development version of multnutFW from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("robertwbuchkowski/multnutFW@main")
```

## Example

This is a basic example analyzing the introductory food web:

``` r
library(multnutFW)

# Decide if you want to correct the feeding preferences based on the biomass proportions:
intro_comm = biomass_weight_preferences(intro_comm)

# Run the food web analysis of the introductory community:
intro_analysis = comana(intro_comm)

# The analysis is run on the food web included in the package with this feeding matrix:
intro_comm$imat
#>          Pred     Prey1      Prey2  Microbe1  Detritus     Plant
#> Pred        0 0.5714286 0.42857143 0.0000000 0.0000000 0.0000000
#> Prey1       0 0.0000000 0.01960784 0.0000000 0.3921569 0.5882353
#> Prey2       0 0.0000000 0.00000000 0.1666667 0.8333333 0.0000000
#> Microbe1    0 0.0000000 0.00000000 0.0000000 1.0000000 0.0000000
#> Detritus    0 0.0000000 0.00000000 0.0000000 0.0000000 0.0000000
#> Plant       0 0.0000000 0.00000000 0.0000000 0.0000000 0.0000000

# The introductory community has 4 chemical elements:
names(intro_comm$prop)
#> [1] "general"      "assimilation"
```

This introductory analysis outputs several important features of the
food web. First, it outputs the total consumption rate for each species
in units of carbon.

``` r
intro_analysis$consumption
#>        Pred       Prey1       Prey2    Microbe1    Detritus       Plant 
#>   0.1967213  44.3467023   8.2307829   7.9647464 -25.5162955  27.5862955
```

It also outputs matrices of carbon (and any other nutrient) flow
throughout the food web. To get the output matrix for another element,
simply put that element’s name in the place of “Carbon”.

``` r
intro_analysis$fmat$Carbon
#>          Pred     Prey1      Prey2 Microbe1  Detritus   Plant
#> Pred        0 0.1124122 0.08430913 0.000000  0.000000  0.0000
#> Prey1       0 0.0000000 0.86954318 0.000000 17.390864 26.0863
#> Prey2       0 0.0000000 0.00000000 1.371797  6.858986  0.0000
#> Microbe1    0 0.0000000 0.00000000 0.000000  7.964746  0.0000
#> Detritus    0 0.0000000 0.00000000 0.000000  0.000000  0.0000
#> Plant       0 0.0000000 0.00000000 0.000000  0.000000  0.0000
```

It also outputs the mineralization rate for each species and each
element. We can make a graph to show these data more clearly.

``` r
# Data on mineralization rates:
intro_analysis$mineralization
#> $Carbon
#>     Pred    Prey1    Prey2 Microbe1 Detritus    Plant 
#>     0.02     0.80     0.25     1.00     0.00     0.00 
#> 
#> $Nitrogen
#>         Pred        Prey1        Prey2     Microbe1     Detritus        Plant 
#>  0.005970492 -3.345858823 -0.258654360 -0.795593305  0.000000000  0.000000000 
#> 
#> $Phosphorus
#>          Pred         Prey1         Prey2      Microbe1      Detritus 
#> -0.0001222482  0.1166931005  0.0500923844  0.0160000000  0.0000000000 
#>         Plant 
#>  0.0000000000 
#> 
#> $Calcium
#>        Pred       Prey1       Prey2    Microbe1    Detritus       Plant 
#> 0.001192506 0.084641779 0.056353932 0.018000000 0.000000000 0.000000000
```

Notice how some of the mineralization rates are negative. Microbe1 can
immobilize nitrogen (canIMM = 1), but the other taxa cannot.

The package offers two options: correct diet or correct respiration.

To correct the diet, organisms shift their diet towards less abundant,
nutrient rich food. Notice the changes in feeding preferences.

``` r
# Correct the diet
intro_comm_diet = correct_diet(intro_comm)
#> Warning in correct_diet(intro_comm): Diet correction removes an item from the
#> diet of species 2 called Prey1. May get strange model behavior. Check outputs
#> for errors in diet proportions! This code just deletes the food item and
#> distirbutes evenly across the other food items.

intro_comm_diet$imat
#>          Pred    Prey1     Prey2  Microbe1  Detritus     Plant
#> Pred        0 0.765625 0.2343750 0.0000000 0.0000000 0.0000000
#> Prey1       0 0.000000 0.8826029 0.0000000 0.0000000 0.1173971
#> Prey2       0 0.000000 0.0000000 0.5181133 0.4818867 0.0000000
#> Microbe1    0 0.000000 0.0000000 0.0000000 1.0000000 0.0000000
#> Detritus    0 0.000000 0.0000000 0.0000000 0.0000000 0.0000000
#> Plant       0 0.000000 0.0000000 0.0000000 0.0000000 0.0000000
```

The other option is to correct respiration by increasing the overflow
respiration $\hat{E}_{C,i}$ term from 0 to whatever value is needed to
remove the excess carbon. This change has to occur simultaneously with
changes in feeding rate, because it often means that more volume needs
to be eaten to overcome nutrient limitation.

Whenever the respiration term is reduced, this means that the organism
is suffering elemental limitation from something other than carbon. The
function automatically prints the element that is most limiting for each
species.

``` r
# Correct the respiration
intro_comm_resp = correct_respiration(intro_comm)
#>         ID Limiting_nutrient
#> 1     Pred        Phosphorus
#> 2    Prey1          Nitrogen
#> 3    Prey2          Nitrogen
#> 4 Microbe1            Carbon
#> 5 Detritus            Carbon
#> 6    Plant            Carbon

# Look at the new values for overflow respiration:
intro_comm_resp$prop$general$Carbon
#>          ID    E   Q canIMM    d     B isDetritus isPlant FecalRecycling
#> 1      Pred 0.20 0.5      0 1.00   0.1          0       0              0
#> 4     Prey1 0.10 0.5      0 3.00   8.0          0       0              0
#> 7     Prey2 0.05 0.5      0 0.50   5.0          0       0              0
#> 10 Microbe1 0.05 0.5      0 0.20  20.0          0       0              0
#> 13 Detritus 0.00 0.5      0 0.00 100.0          1       0              1
#> 16    Plant 0.00 0.5      0 0.01 150.0          0       1              0
#>    NecromassRecycling p       Ehat
#> 1                   0 1 0.05097656
#> 4                   0 1 6.24206665
#> 7                   0 1 0.69294085
#> 10                  0 1 0.00000000
#> 13                  1 1 0.00000000
#> 16                  0 1 0.00000000
```

### Calculating effects on nutrient mineralization

You can calculate the direct and indirect effects of each organism on
mineralization using the built in function. This function allows you to
customize the output to include whichever food web nodes and elements
that are of interest.

``` r
# Calculate the mineralization rates for all elements using the community with corrected respiration rates:
whomineralizes(intro_comm_resp)
#>          ID     consump basal_C_consump  min_Carbon  min_Nitrogen
#> 1      Pred   0.2050781       0.0000000  0.02509766  7.167187e-03
#> 2     Prey1 133.2474609     130.6347656 50.73653320 -2.955858e-15
#> 3     Prey2  19.8117560      16.5097966  3.71470424 -5.329071e-16
#> 4  Microbe1  10.3774492      10.3774492  1.00000000 -1.097181e+00
#> 5  Detritus -24.4045243       0.0000000  0.00000000  0.000000e+00
#> 6     Plant  79.8808594       0.0000000  0.00000000  0.000000e+00
#> 7      Pred          NA       0.4174345  0.00000000 -2.950965e-02
#> 8     Prey1          NA     267.0461697  0.00000000 -3.517187e-01
#> 9     Prey2          NA      33.9575021  0.00000000 -2.459124e-01
#> 10 Microbe1          NA      17.4529390  0.00000000  3.467057e-01
#>    min_Phosphorus  min_Calcium          Effect
#> 1     0.000000000  0.001328125          Direct
#> 2     1.317280781  1.317642539          Direct
#> 3     0.170381101  0.191678739          Direct
#> 4     0.016000000  0.018000000          Direct
#> 5     0.000000000  0.000000000          Direct
#> 6     0.000000000  0.000000000          Direct
#> 7     0.001618310  0.001603096 Indirect static
#> 8     0.031430208  0.035312109 Indirect static
#> 9    -0.008203523 -0.004931873 Indirect static
#> 10    0.000000000  0.000000000 Indirect static
```

### Modifying respiration parameters

The model contains respiration parameters that either take respiration
as a function of biomass $E$ or as a function of consumption rate $p$.
The package contains functions to switch between these two parameters
during the calculations in $comana$. At equilibrium, the difference does
not matter, but it is important when simulating the food web.

``` r

# Take the introdutory community and switch respiration by E to p.
temp_comm = E_to_p(intro_comm)

temp_comm$prop$general$Carbon
#>          ID E   Q canIMM    d     B isDetritus isPlant FecalRecycling
#> 1      Pred 0 0.5      0 1.00   0.1          0       0              0
#> 4     Prey1 0 0.5      0 3.00   8.0          0       0              0
#> 7     Prey2 0 0.5      0 0.50   5.0          0       0              0
#> 10 Microbe1 0 0.5      0 0.20  20.0          0       0              0
#> 13 Detritus 0 0.5      0 0.00 100.0          1       0              1
#> 16    Plant 0 0.5      0 0.01 150.0          0       1              0
#>    NecromassRecycling         p Ehat
#> 1                   0 0.8333333    0
#> 4                   0 0.9678875    0
#> 7                   0 0.9325027    0
#> 10                  0 0.8430584    0
#> 13                  1 1.0000000    0
#> 16                  0 1.0000000    0

# Switch back
temp_comm = p_to_E(temp_comm)

temp_comm$prop$general$Carbon
#>          ID    E   Q canIMM    d     B isDetritus isPlant FecalRecycling
#> 1      Pred 0.20 0.5      0 1.00   0.1          0       0              0
#> 4     Prey1 0.10 0.5      0 3.00   8.0          0       0              0
#> 7     Prey2 0.05 0.5      0 0.50   5.0          0       0              0
#> 10 Microbe1 0.05 0.5      0 0.20  20.0          0       0              0
#> 13 Detritus 0.00 0.5      0 0.00 100.0          1       0              1
#> 16    Plant 0.00 0.5      0 0.01 150.0          0       1              0
#>    NecromassRecycling p Ehat
#> 1                   0 1    0
#> 4                   0 1    0
#> 7                   0 1    0
#> 10                  0 1    0
#> 13                  1 1    0
#> 16                  0 1    0

# Clean up environment
rm(temp_comm)
```

### Using your own food web

You can run the analyzes on your own food webs easily by using the
`build_foodweb` function to get them into the right format.

The function requires two inputs. First is a feeding list of
predator-prey relationships. This data frame also contains the rates of
assimilation efficiency for each trophic interaction coded with the
letter ‘a’ before each element included in the model. Sometimes, users
would like to use the same assimilation efficiency for each food source
or for each node. You can do this by just repeating the assimilation
efficiency for each feeding interaction. One easy way to implement this
is to join a table of node ID and assimilation efficiency with the
feeding list table by either Predator or Prey columns, respectively.

``` r
# Feeding list example for the introductory community:
feedinglist
#>   Predator     Prey Preference aCarbon aNitrogen aPhosphorus aCalcium
#> 1     Pred    Prey1        1.0    0.61       0.7         0.8     0.80
#> 2     Pred    Prey2        1.2    0.61       0.7         0.8     0.80
#> 3    Prey1    Prey2        1.0    0.65       0.7         0.8     0.80
#> 4    Prey2 Microbe1        1.0    0.45       0.7         0.8     0.80
#> 5    Prey2 Detritus        1.0    0.45       0.7         0.8     0.80
#> 6    Prey1 Detritus        1.0    0.65       0.7         0.8     0.80
#> 7 Microbe1 Detritus        1.0    0.80       0.7         0.8     0.80
#> 8    Prey1    Plant        1.0    0.50       0.6         0.7     0.75
#> 9    Prey1    Plant        1.0    0.50       0.6         0.7     0.75
```

The second is a properties dataframe listing the necessary parameters.

``` r
# Feeding list example for the introductory community:
head(properties_example1)
#>      ID Element Parameter Value
#> 1  Pred  Carbon         E   0.2
#> 2  Pred  Carbon         Q   0.5
#> 3  Pred  Carbon    canIMM   0.0
#> 4 Prey1  Carbon         E   0.1
#> 5 Prey1  Carbon         Q   0.5
#> 6 Prey1  Carbon    canIMM   0.0
# Only printing the head of this file as an example.
```

These two files can be put into the function to create your new food
web.

``` r
yourfoodweb = build_foodweb(feeding = feedinglist, properties = properties_example1)

str(yourfoodweb)
#> List of 2
#>  $ imat: num [1:6, 1:6] 0 0 0 0 0 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>  $ prop:List of 2
#>   ..$ general     :List of 4
#>   .. ..$ Carbon    :'data.frame':    6 obs. of  12 variables:
#>   .. .. ..$ ID                : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. ..$ E                 : num [1:6] 0.2 0.1 0.05 0.05 0 0
#>   .. .. ..$ Q                 : num [1:6] 0.5 0.5 0.5 0.5 0.5 0.5
#>   .. .. ..$ canIMM            : num [1:6] 0 0 0 0 0 0
#>   .. .. ..$ d                 : num [1:6] 1 3 0.5 0.2 0 0.01
#>   .. .. ..$ B                 : num [1:6] 0.1 8 5 20 100 150
#>   .. .. ..$ isDetritus        : num [1:6] 0 0 0 0 1 0
#>   .. .. ..$ isPlant           : num [1:6] 0 0 0 0 0 1
#>   .. .. ..$ FecalRecycling    : num [1:6] 0 0 0 0 1 0
#>   .. .. ..$ NecromassRecycling: num [1:6] 0 0 0 0 1 0
#>   .. .. ..$ p                 : num [1:6] 1 1 1 1 1 1
#>   .. .. ..$ Ehat              : num [1:6] 0 0 0 0 0 0
#>   .. .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. .. ..$ v.names: NULL
#>   .. .. .. ..$ timevar: chr "Parameter"
#>   .. .. .. ..$ idvar  : chr "ID"
#>   .. .. .. ..$ times  : chr [1:11] "E" "Q" "canIMM" "d" ...
#>   .. .. .. ..$ varying: chr [1, 1:11] "Value.E" "Value.Q" "Value.canIMM" "Value.d" ...
#>   .. ..$ Nitrogen  :'data.frame':    6 obs. of  5 variables:
#>   .. .. ..$ ID    : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. ..$ Q     : num [1:6] 0.111 0.104 0.1 0.1 0.025 0.03
#>   .. .. ..$ canIMM: num [1:6] 0 0 0 1 0 0
#>   .. .. ..$ p     : num [1:6] 1 1 1 1 1 1
#>   .. .. ..$ Emin  : num [1:6] 0 0 0 0 0 0
#>   .. .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. .. ..$ v.names: NULL
#>   .. .. .. ..$ timevar: chr "Parameter"
#>   .. .. .. ..$ idvar  : chr "ID"
#>   .. .. .. ..$ times  : chr [1:4] "Q" "canIMM" "p" "Emin"
#>   .. .. .. ..$ varying: chr [1, 1:4] "Value.Q" "Value.canIMM" "Value.p" "Value.Emin"
#>   .. ..$ Phosphorus:'data.frame':    6 obs. of  5 variables:
#>   .. .. ..$ ID    : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. ..$ Q     : num [1:6] 0.015 0.01 0.008 0.008 0.008 0.01
#>   .. .. ..$ canIMM: num [1:6] 0 0 0 1 0 0
#>   .. .. ..$ p     : num [1:6] 1 1 1 1 1 1
#>   .. .. ..$ Emin  : num [1:6] 0 0 0 0 0 0
#>   .. .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. .. ..$ v.names: NULL
#>   .. .. .. ..$ timevar: chr "Parameter"
#>   .. .. .. ..$ idvar  : chr "ID"
#>   .. .. .. ..$ times  : chr [1:4] "Q" "canIMM" "p" "Emin"
#>   .. .. .. ..$ varying: chr [1, 1:4] "Value.Q" "Value.canIMM" "Value.p" "Value.Emin"
#>   .. ..$ Calcium   :'data.frame':    6 obs. of  5 variables:
#>   .. .. ..$ ID    : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. ..$ Q     : num [1:6] 0.01 0.011 0.009 0.009 0.009 0.009
#>   .. .. ..$ canIMM: num [1:6] 0 0 0 0 0 0
#>   .. .. ..$ p     : num [1:6] 1 1 1 1 1 1
#>   .. .. ..$ Emin  : num [1:6] 0 0 0 0 0 0
#>   .. .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. .. ..$ v.names: NULL
#>   .. .. .. ..$ timevar: chr "Parameter"
#>   .. .. .. ..$ idvar  : chr "ID"
#>   .. .. .. ..$ times  : chr [1:4] "Q" "canIMM" "p" "Emin"
#>   .. .. .. ..$ varying: chr [1, 1:4] "Value.Q" "Value.canIMM" "Value.p" "Value.Emin"
#>   ..$ assimilation:List of 4
#>   .. ..$ Carbon    : num [1:6, 1:6] 0 0 0 0 0 0 0.61 0 0 0 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ Nitrogen  : num [1:6, 1:6] 0 0 0 0 0 0 0.7 0 0 0 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ Phosphorus: num [1:6, 1:6] 0 0 0 0 0 0 0.8 0 0 0 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ Calcium   : num [1:6, 1:6] 0 0 0 0 0 0 0.8 0 0 0 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. .. ..$ : chr [1:6] "Pred" "Prey1" "Prey2" "Microbe1" ...
```

The function creates an interaction matrix and also breaks out the
assimilation efficiency and properties by element into the appropriate
matrices for the package. If you look at the structure of this
community, you can see how to build it manually using lists.

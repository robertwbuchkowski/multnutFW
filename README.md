
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
# install.packages("pak")
pak::pak("robertwbuchkowski/multnutFW")
```

## Example

This is a basic example analyzing the introductory food web:

``` r
library(multnutFW)

# Run the food web analysis of the introductory community:
intro_analysis = comana(intro_comm)

# The analysis is run on the food web included in the package with this feeding matrix:
intro_comm$imat
#>          Pred Prey1 Prey2 Microbe1 Detritus
#> Pred        0     1   1.2        0        0
#> Prey1       0     0   1.0        0        1
#> Prey2       0     0   0.0        1        1
#> Microbe1    0     0   0.0        0        1
#> Detritus    0     0   0.0        0        0

# The introductory community has 4 chemical elements:
names(intro_comm$prop)
#> [1] "Carbon"     "Nitrogen"   "Phosphorus" "Calcium"
```

This introductory analysis outputs several important features of the
food web. First, it outputs the total consumption rate for each species
in units of carbon.

``` r
intro_analysis$consumption
#>       Pred      Prey1      Prey2   Microbe1   Detritus 
#>  0.1967213 38.3267880 10.3542095  8.4071270 32.6700000
```

It also outputs matrices of carbon (and any other nutrient) flow
throughout the food web. To get the output matrix for another element,
simply put that element’s name in the place of “Carbon”.

``` r
intro_analysis$fmat$Carbon
#>          Pred     Prey1      Prey2 Microbe1  Detritus
#> Pred        0 0.1124122 0.08430913 0.000000  0.000000
#> Prey1       0 0.0000000 1.82508514 0.000000 36.501703
#> Prey2       0 0.0000000 0.00000000 1.725702  8.628508
#> Microbe1    0 0.0000000 0.00000000 0.000000  8.407127
#> Detritus    0 0.0000000 0.00000000 0.000000  0.000000
```

It also outputs the mineralization rate for each species and each
element. We can make a graph to show these data more clearly.

``` r
# Data on mineralization rates:
intro_analysis$mineralization
#> $Carbon
#>     Pred    Prey1    Prey2 Microbe1 Detritus 
#>     0.02     0.80     0.25     1.00     0.00 
#> 
#> $Nitrogen
#>        Pred       Prey1       Prey2    Microbe1    Detritus 
#>   0.1211446 -80.4897886  -8.4570714 -21.2722718   0.0000000 
#> 
#> $Phosphorus
#>        Pred       Prey1       Prey2    Microbe1    Detritus 
#>  -0.1358314  20.8366060 242.1233328  62.5000000   0.0000000 
#> 
#> $Calcium
#>       Pred      Prey1      Prey2   Microbe1   Detritus 
#>   2.981265  44.282394 215.220740  55.555556   0.000000
```

Notice how some of the mineralization rates are negative. Microbe1 can
immobilize nitrogen (canIMM = 1), but the other taxa cannot.

The package offers two options: correct diet or correct respiration.

To correct the diet, organisms shift their diet towards less abundant,
nutrient rich food. Notice the changes in feeding preferences.

``` r
# Correct the diet
intro_comm_diet = correct_diet(intro_comm)

intro_comm_diet$imat
#>          Pred    Prey1   Prey2 Microbe1 Detritus
#> Pred        0 2.041667   1.000 0.000000        0
#> Prey1       0 0.000000 209.886 0.000000        1
#> Prey2       0 0.000000   0.000 5.376607        1
#> Microbe1    0 0.000000   0.000 0.000000        1
#> Detritus    0 0.000000   0.000 0.000000        0
```

The other option is to correct respiration by increasing the overflow
respiration $\hat{E}_{C,i}$ term from 0 to whatever value is needed to
remove the excess carbon. This change has to occur simultaneously with
changes in feeding rate, because it often means that more volume needs
to be eaten to overcome nutrient limitation.

Whenever the respiration term is reduced, this means that the organism
is suffering elemental limitaiton from something other than carbon. The
function automatically prints the element that is most limiting for each
species.

``` r
# Correct the diet
intro_comm_resp = correct_respiration(intro_comm)
#>         ID Limiting_nutrient
#> 1     Pred        Phosphorus
#> 2    Prey1          Nitrogen
#> 3    Prey2          Nitrogen
#> 4 Microbe1            Carbon
#> 5 Detritus            Carbon

# Look at the new values for overflow respiration:
intro_comm_resp$prop$Carbon
#>          ID    a    E   Q canIMM   d     B DetritusRecycling isDetritus isPlant
#> 1      Pred 0.61 0.20 0.5      0 1.0   0.1                 0          0       0
#> 5     Prey1 0.65 0.10 0.5      0 3.0   8.0                 0          0       0
#> 9     Prey2 0.45 0.05 0.5      0 0.5   5.0                 0          0       0
#> 13 Microbe1 0.80 0.05 0.5      0 0.2  20.0                 0          0       0
#> 17 Detritus 1.00 0.00 0.5      0 0.0 100.0                 1          1       0
#>    p       Ehat
#> 1  1 0.05097656
#> 5  1 7.07486328
#> 9  1 1.17282366
#> 13 1 0.00000000
#> 17 1 0.00000000
```

### Calculating effects on nutrient mineralization

You can calculate the direct and indirect effects of each organism on
mineralization using the built in function. This function allows you to
customize the output to include whichever food web nodes and elements
that are of interest.

``` r
# Calculate the mineralization rates for all elements using the community with corrected respiration rates:
whomineralizes(intro_comm_resp)
#>          ID    Element        Direct      Indirect
#> 1      Pred     Carbon  3.888811e-04  0.000000e+00
#> 2     Prey1     Carbon  8.893799e-01 -1.376207e-17
#> 3     Prey2     Carbon  9.473654e-02  0.000000e+00
#> 4  Microbe1     Carbon  1.549472e-02  0.000000e+00
#> 5  Detritus     Carbon  0.000000e+00  0.000000e+00
#> 6      Pred   Nitrogen -4.083241e-03  2.070124e-02
#> 7     Prey1   Nitrogen  0.000000e+00  5.799253e-01
#> 8     Prey2   Nitrogen  0.000000e+00  1.806213e-01
#> 9  Microbe1   Nitrogen  1.004083e+00 -4.005648e-01
#> 10 Detritus   Nitrogen  0.000000e+00  2.046118e+01
#> 11     Pred Phosphorus  5.833179e-20  1.160009e-03
#> 12    Prey1 Phosphorus  7.079718e-01  7.188048e-02
#> 13    Prey2 Phosphorus  2.762660e-01 -6.176048e-05
#> 14 Microbe1 Phosphorus  1.576218e-02 -1.146850e-16
#> 15 Detritus Phosphorus  0.000000e+00 -1.448479e+00
#> 16     Pred    Calcium  9.053197e-04  1.146800e-03
#> 17    Prey1    Calcium  7.184487e-01  6.923348e-02
#> 18    Prey2    Calcium  2.654982e-01 -1.993322e-04
#> 19 Microbe1    Calcium  1.514783e-02 -1.239919e-16
#> 20 Detritus    Calcium  0.000000e+00 -1.392023e+00
```

### Using your own food web

You can run the analyzes on your own food webs easily by using the
`build_foodweb` function to get them into the right format.

The function requires two inputs. First is a feeding list of
predator-prey relationships.

``` r
# Feeding list example for the introductory community:
feedinglist
#>   Predator     Prey Preference
#> 1     Pred    Prey1        1.0
#> 2     Pred    Prey2        1.2
#> 3    Prey1    Prey2        1.0
#> 4    Prey2 Microbe1        1.0
#> 5    Prey2 Detritus        1.0
#> 6    Prey1 Detritus        1.0
#> 7 Microbe1 Detritus        1.0
```

The second is a properties dataframe listing the necessary parameters.

``` r
# Feeding list example for the introductory community:
head(properties_example1)
#>      ID Element Parameter Value
#> 1  Pred  Carbon         a  0.61
#> 2  Pred  Carbon         E  0.20
#> 3  Pred  Carbon         Q  0.50
#> 4  Pred  Carbon    canIMM  0.00
#> 5 Prey1  Carbon         a  0.65
#> 6 Prey1  Carbon         E  0.10
# Only printing the head of this file as an example.
```

These two files can be put into the function to create your new food
web.

``` r
yourfoodweb = build_foodweb(feeding = feedinglist, properties = properties_example1)

str(yourfoodweb)
#> List of 2
#>  $ imat: num [1:5, 1:5] 0 0 0 0 0 1 0 0 0 0 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>  $ prop:List of 4
#>   ..$ Carbon    :'data.frame':   5 obs. of  12 variables:
#>   .. ..$ ID               : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ a                : num [1:5] 0.61 0.65 0.45 0.8 1
#>   .. ..$ E                : num [1:5] 0.2 0.1 0.05 0.05 0
#>   .. ..$ Q                : num [1:5] 0.5 0.5 0.5 0.5 0.5
#>   .. ..$ canIMM           : num [1:5] 0 0 0 0 0
#>   .. ..$ d                : num [1:5] 1 3 0.5 0.2 0
#>   .. ..$ B                : num [1:5] 0.1 8 5 20 100
#>   .. ..$ DetritusRecycling: num [1:5] 0 0 0 0 1
#>   .. ..$ isDetritus       : num [1:5] 0 0 0 0 1
#>   .. ..$ isPlant          : num [1:5] 0 0 0 0 0
#>   .. ..$ p                : num [1:5] 1 1 1 1 1
#>   .. ..$ Ehat             : num [1:5] 0 0 0 0 0
#>   .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. ..$ v.names: NULL
#>   .. .. ..$ timevar: chr "Parameter"
#>   .. .. ..$ idvar  : chr "ID"
#>   .. .. ..$ times  : chr [1:11] "a" "E" "Q" "canIMM" ...
#>   .. .. ..$ varying: chr [1, 1:11] "Value.a" "Value.E" "Value.Q" "Value.canIMM" ...
#>   ..$ Nitrogen  :'data.frame':   5 obs. of  6 variables:
#>   .. ..$ ID    : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ a     : num [1:5] 0.7 0.7 0.7 0.7 1
#>   .. ..$ Q     : num [1:5] 0.111 0.104 0.1 0.1 0.025
#>   .. ..$ canIMM: num [1:5] 0 0 0 1 0
#>   .. ..$ p     : num [1:5] 1 1 1 1 1
#>   .. ..$ Emin  : num [1:5] 0 0 0 0 0
#>   .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. ..$ v.names: NULL
#>   .. .. ..$ timevar: chr "Parameter"
#>   .. .. ..$ idvar  : chr "ID"
#>   .. .. ..$ times  : chr [1:5] "a" "Q" "canIMM" "p" ...
#>   .. .. ..$ varying: chr [1, 1:5] "Value.a" "Value.Q" "Value.canIMM" "Value.p" ...
#>   ..$ Phosphorus:'data.frame':   5 obs. of  6 variables:
#>   .. ..$ ID    : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ a     : num [1:5] 0.8 0.8 0.8 0.8 1
#>   .. ..$ Q     : num [1:5] 0.015 0.01 0.008 0.008 0.008
#>   .. ..$ canIMM: num [1:5] 0 0 0 1 0
#>   .. ..$ p     : num [1:5] 1 1 1 1 1
#>   .. ..$ Emin  : num [1:5] 0 0 0 0 0
#>   .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. ..$ v.names: NULL
#>   .. .. ..$ timevar: chr "Parameter"
#>   .. .. ..$ idvar  : chr "ID"
#>   .. .. ..$ times  : chr [1:5] "a" "Q" "canIMM" "p" ...
#>   .. .. ..$ varying: chr [1, 1:5] "Value.a" "Value.Q" "Value.canIMM" "Value.p" ...
#>   ..$ Calcium   :'data.frame':   5 obs. of  6 variables:
#>   .. ..$ ID    : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ a     : num [1:5] 0.8 0.8 0.8 0.8 1
#>   .. ..$ Q     : num [1:5] 0.01 0.011 0.009 0.009 0.009
#>   .. ..$ canIMM: num [1:5] 0 0 0 0 0
#>   .. ..$ p     : num [1:5] 1 1 1 1 1
#>   .. ..$ Emin  : num [1:5] 0 0 0 0 0
#>   .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. ..$ v.names: NULL
#>   .. .. ..$ timevar: chr "Parameter"
#>   .. .. ..$ idvar  : chr "ID"
#>   .. .. ..$ times  : chr [1:5] "a" "Q" "canIMM" "p" ...
#>   .. .. ..$ varying: chr [1, 1:5] "Value.a" "Value.Q" "Value.canIMM" "Value.p" ...
```

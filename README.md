
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
```

This introductory analyis outputs several important features of the food
web. First, it outputs the total consumption rate for each species in
units of carbon.

``` r
intro_analysis$consumption
#>       Pred      Prey1      Prey2   Microbe1   Detritus 
#>  0.1967213 38.3267880 10.3542095  8.4071270 32.6700000
```

It also outputs matricies of carbon (and any other nutrient) flow
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

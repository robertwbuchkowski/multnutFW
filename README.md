
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
devtools::install_github("robertwbuchkowski/multnutFW@development")
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
#> [1] "general"      "assimilation"
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
# Correct the diet NOT WORKING YET FOR THIS VERSION
#intro_comm_diet = correct_diet(intro_comm)

#intro_comm_diet$imat
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
# Correct the diet NOT WORKING YET FOR THIS VERSION
#intro_comm_resp = correct_respiration(intro_comm)

# Look at the new values for overflow respiration:
#intro_comm_resp$prop$Carbon
```

### Calculating effects on nutrient mineralization

You can calculate the direct and indirect effects of each organism on
mineralization using the built in function. This function allows you to
customize the output to include whichever food web nodes and elements
that are of interest.

``` r
# Calculate the mineralization rates for all elements using the community with uncorrected rates, because other not working yet:
whomineralizes(intro_comm)
#>          ID    Element        Direct      Indirect
#> 1      Pred     Carbon  0.0096618357  0.000000e+00
#> 2     Prey1     Carbon  0.3864734300 -1.072679e-16
#> 3     Prey2     Carbon  0.1207729469 -1.072679e-16
#> 4  Microbe1     Carbon  0.4830917874 -1.072679e-16
#> 5  Detritus     Carbon  0.0000000000  0.000000e+00
#> 6      Pred   Nitrogen -0.0011003346  6.423725e-03
#> 7     Prey1   Nitrogen  0.7310741155  5.479774e-02
#> 8     Prey2   Nitrogen  0.0768140419  1.835448e-02
#> 9  Microbe1   Nitrogen  0.1932121772 -4.114486e-02
#> 10 Detritus   Nitrogen  0.0000000000  2.124507e+00
#> 11     Pred Phosphorus -0.0004175263  1.356253e-02
#> 12    Prey1 Phosphorus  0.0640487610  2.571424e-01
#> 13    Prey2 Phosphorus  0.7442526615 -7.220880e-04
#> 14 Microbe1 Phosphorus  0.1921161038  3.494572e-16
#> 15 Detritus Phosphorus  0.0000000000 -5.262101e+00
#> 16     Pred    Calcium  0.0093738683  1.268572e-02
#> 17    Prey1    Calcium  0.1392353179  2.355166e-01
#> 18    Prey2    Calcium  0.6767097565 -2.204981e-03
#> 19 Microbe1    Calcium  0.1746810573  3.574609e-16
#> 20 Detritus    Calcium  0.0000000000 -4.784551e+00
```

### Using your own food web

You can run the analyzes on your own food webs easily by using the
`build_foodweb` function to get them into the right format.

The function requires two inputs. First is a feeding list of
predator-prey relationships.

``` r
# Feeding list example for the introductory community:
feedinglist
#>   Predator     Prey Preference aCarbon aNitrogen aPhosphorus aCalcium
#> 1     Pred    Prey1        1.0    0.61       0.7         0.8      0.8
#> 2     Pred    Prey2        1.2    0.61       0.7         0.8      0.8
#> 3    Prey1    Prey2        1.0    0.65       0.7         0.8      0.8
#> 4    Prey2 Microbe1        1.0    0.45       0.7         0.8      0.8
#> 5    Prey2 Detritus        1.0    0.45       0.7         0.8      0.8
#> 6    Prey1 Detritus        1.0    0.65       0.7         0.8      0.8
#> 7 Microbe1 Detritus        1.0    0.80       0.7         0.8      0.8
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
#>  $ imat: num [1:5, 1:5] 0 0 0 0 0 1 0 0 0 0 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>  $ prop:List of 2
#>   ..$ general     :List of 4
#>   .. ..$ Carbon    :'data.frame':    5 obs. of  11 variables:
#>   .. .. ..$ ID               : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. ..$ E                : num [1:5] 0.2 0.1 0.05 0.05 0
#>   .. .. ..$ Q                : num [1:5] 0.5 0.5 0.5 0.5 0.5
#>   .. .. ..$ canIMM           : num [1:5] 0 0 0 0 0
#>   .. .. ..$ d                : num [1:5] 1 3 0.5 0.2 0
#>   .. .. ..$ B                : num [1:5] 0.1 8 5 20 100
#>   .. .. ..$ DetritusRecycling: num [1:5] 0 0 0 0 1
#>   .. .. ..$ isDetritus       : num [1:5] 0 0 0 0 1
#>   .. .. ..$ isPlant          : num [1:5] 0 0 0 0 0
#>   .. .. ..$ p                : num [1:5] 1 1 1 1 1
#>   .. .. ..$ Ehat             : num [1:5] 0 0 0 0 0
#>   .. .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. .. ..$ v.names: NULL
#>   .. .. .. ..$ timevar: chr "Parameter"
#>   .. .. .. ..$ idvar  : chr "ID"
#>   .. .. .. ..$ times  : chr [1:10] "E" "Q" "canIMM" "d" ...
#>   .. .. .. ..$ varying: chr [1, 1:10] "Value.E" "Value.Q" "Value.canIMM" "Value.d" ...
#>   .. ..$ Nitrogen  :'data.frame':    5 obs. of  5 variables:
#>   .. .. ..$ ID    : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. ..$ Q     : num [1:5] 0.111 0.104 0.1 0.1 0.025
#>   .. .. ..$ canIMM: num [1:5] 0 0 0 1 0
#>   .. .. ..$ p     : num [1:5] 1 1 1 1 1
#>   .. .. ..$ Emin  : num [1:5] 0 0 0 0 0
#>   .. .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. .. ..$ v.names: NULL
#>   .. .. .. ..$ timevar: chr "Parameter"
#>   .. .. .. ..$ idvar  : chr "ID"
#>   .. .. .. ..$ times  : chr [1:4] "Q" "canIMM" "p" "Emin"
#>   .. .. .. ..$ varying: chr [1, 1:4] "Value.Q" "Value.canIMM" "Value.p" "Value.Emin"
#>   .. ..$ Phosphorus:'data.frame':    5 obs. of  5 variables:
#>   .. .. ..$ ID    : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. ..$ Q     : num [1:5] 0.015 0.01 0.008 0.008 0.008
#>   .. .. ..$ canIMM: num [1:5] 0 0 0 1 0
#>   .. .. ..$ p     : num [1:5] 1 1 1 1 1
#>   .. .. ..$ Emin  : num [1:5] 0 0 0 0 0
#>   .. .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. .. ..$ v.names: NULL
#>   .. .. .. ..$ timevar: chr "Parameter"
#>   .. .. .. ..$ idvar  : chr "ID"
#>   .. .. .. ..$ times  : chr [1:4] "Q" "canIMM" "p" "Emin"
#>   .. .. .. ..$ varying: chr [1, 1:4] "Value.Q" "Value.canIMM" "Value.p" "Value.Emin"
#>   .. ..$ Calcium   :'data.frame':    5 obs. of  5 variables:
#>   .. .. ..$ ID    : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. ..$ Q     : num [1:5] 0.01 0.011 0.009 0.009 0.009
#>   .. .. ..$ canIMM: num [1:5] 0 0 0 0 0
#>   .. .. ..$ p     : num [1:5] 1 1 1 1 1
#>   .. .. ..$ Emin  : num [1:5] 0 0 0 0 0
#>   .. .. ..- attr(*, "reshapeWide")=List of 5
#>   .. .. .. ..$ v.names: NULL
#>   .. .. .. ..$ timevar: chr "Parameter"
#>   .. .. .. ..$ idvar  : chr "ID"
#>   .. .. .. ..$ times  : chr [1:4] "Q" "canIMM" "p" "Emin"
#>   .. .. .. ..$ varying: chr [1, 1:4] "Value.Q" "Value.canIMM" "Value.p" "Value.Emin"
#>   ..$ assimilation:List of 4
#>   .. ..$ Carbon    : num [1:5, 1:5] 1 1 1 1 1 0.61 1 1 1 1 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ Nitrogen  : num [1:5, 1:5] 1 1 1 1 1 0.7 1 1 1 1 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ Phosphorus: num [1:5, 1:5] 1 1 1 1 1 0.8 1 1 1 1 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. ..$ Calcium   : num [1:5, 1:5] 1 1 1 1 1 0.8 1 1 1 1 ...
#>   .. .. ..- attr(*, "dimnames")=List of 2
#>   .. .. .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
#>   .. .. .. ..$ : chr [1:5] "Pred" "Prey1" "Prey2" "Microbe1" ...
```

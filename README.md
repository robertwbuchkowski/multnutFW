
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

This is a basic example which shows you how to solve a common problem:

``` r
library(multnutFW)

# Run the food web analysis of the introductory community:
comana(intro_comm)
#> $fmat
#> $fmat$Carbon
#>          Pred     Prey1      Prey2 Microbe1  Detritus
#> Pred        0 0.1124122 0.08430913 0.000000  0.000000
#> Prey1       0 0.0000000 1.82508514 0.000000 36.501703
#> Prey2       0 0.0000000 0.00000000 1.725702  8.628508
#> Microbe1    0 0.0000000 0.00000000 0.000000  8.407127
#> Detritus    0 0.0000000 0.00000000 0.000000  0.000000
#> 
#> $fmat$Nitrogen
#>          Pred      Prey1      Prey2  Microbe1  Detritus
#> Pred        0 0.02338173 0.01686183 0.0000000 0.0000000
#> Prey1       0 0.00000000 0.36501703 0.0000000 1.8250851
#> Prey2       0 0.00000000 0.00000000 0.3451403 0.4314254
#> Microbe1    0 0.00000000 0.00000000 0.0000000 0.4203563
#> Detritus    0 0.00000000 0.00000000 0.0000000 0.0000000
#> 
#> $fmat$Phosphorus
#>          Pred       Prey1       Prey2   Microbe1  Detritus
#> Pred        0 0.002248244 0.001348946 0.00000000 0.0000000
#> Prey1       0 0.000000000 0.029201362 0.00000000 0.5840272
#> Prey2       0 0.000000000 0.000000000 0.02761123 0.1380561
#> Microbe1    0 0.000000000 0.000000000 0.00000000 0.1345140
#> Detritus    0 0.000000000 0.000000000 0.00000000 0.0000000
#> 
#> $fmat$Calcium
#>          Pred       Prey1       Prey2   Microbe1  Detritus
#> Pred        0 0.002473068 0.001517564 0.00000000 0.0000000
#> Prey1       0 0.000000000 0.032851533 0.00000000 0.6570307
#> Prey2       0 0.000000000 0.000000000 0.03106263 0.1553131
#> Microbe1    0 0.000000000 0.000000000 0.00000000 0.1513283
#> Detritus    0 0.000000000 0.000000000 0.00000000 0.0000000
#> 
#> 
#> $consumption
#>       Pred      Prey1      Prey2   Microbe1   Detritus 
#>  0.1967213 38.3267880 10.3542095  8.4071270 32.6700000 
#> 
#> $AIJ
#> $AIJ$Carbon
#> NULL
#> 
#> $AIJ$Nitrogen
#>             [,1]        [,2]        [,3]        [,4]       [,5]
#> [1,]  0.09000000  0.04585586  0.02063063  0.02063063 -0.4523423
#> [2,]  0.09711538  0.05000000  0.02307692  0.02307692 -0.4817308
#> [3,]  0.32700000  0.27800000  0.25000000  0.25000000 -0.2750000
#> [4,] -0.02300000 -0.07200000 -0.10000000 -0.10000000 -0.6250000
#> [5,]  3.44000000  3.16000000  3.00000000  3.00000000  0.0000000
#> 
#> $AIJ$Phosphorus
#>       [,1]        [,2]       [,3]       [,4]       [,5]
#> [1,] 0.190 -0.07666667 -0.1833333 -0.1833333 -0.1833333
#> [2,] 0.550  0.15000000 -0.0100000 -0.0100000 -0.0100000
#> [3,] 1.050  0.55000000  0.3500000  0.3500000  0.3500000
#> [4,] 0.700  0.20000000  0.0000000  0.0000000  0.0000000
#> [5,] 0.875  0.25000000  0.0000000  0.0000000  0.0000000
#> 
#> $AIJ$Calcium
#>            [,1]      [,2]        [,3]        [,4]        [,5]
#> [1,] 0.19000000 0.2700000 0.110000000 0.110000000 0.110000000
#> [2,] 0.07727273 0.1500000 0.004545455 0.004545455 0.004545455
#> [3,] 0.43888889 0.5277778 0.350000000 0.350000000 0.350000000
#> [4,] 0.08888889 0.1777778 0.000000000 0.000000000 0.000000000
#> [5,] 0.11111111 0.2222222 0.000000000 0.000000000 0.000000000
#> 
#> 
#> $mineralization
#> $mineralization$Carbon
#>     Pred    Prey1    Prey2 Microbe1 Detritus 
#>     0.02     0.80     0.25     1.00     0.00 
#> 
#> $mineralization$Nitrogen
#>        Pred       Prey1       Prey2    Microbe1    Detritus 
#>   0.1211446 -80.4897886  -8.4570714 -21.2722718   0.0000000 
#> 
#> $mineralization$Phosphorus
#>        Pred       Prey1       Prey2    Microbe1    Detritus 
#>  -0.1358314  20.8366060 242.1233328  62.5000000   0.0000000 
#> 
#> $mineralization$Calcium
#>       Pred      Prey1      Prey2   Microbe1   Detritus 
#>   2.981265  44.282394 215.220740  55.555556   0.000000
```

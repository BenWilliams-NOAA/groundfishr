
# groundfishr

<!-- badges: start -->
<!-- badges: end -->

The goal of groundfishr is to create a clear workflow for pulling and cleaning data for fishery stock assessments at AFSC. 
It utilizes a "project oriented workflow" via RStudio (base R is ok, though you must use `here::here()`). 
You must be able to have a connection to the AFSC & AKFIN (Answers) data servers (e.g., VPN if offsite), and have usernames/passwords setup.

## Installation

You can install the released version of groundfishr from [github](https://github.com/BenWilliams-NOAA/rockfishr) with:

``` r
install.packages("groundfishr")
```

## Example

There are a suite of "global" items that are used repeatedly in the `rockfishr` package. 
Currently the package is setup to deal with northern rockfish "NORK" or dusky rockfish "DUSK", other species can be added as needed. 


``` r
library(groundfishr)
## basic example code
```


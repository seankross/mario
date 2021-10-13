
# mario

<!-- badges: start -->
[![R-CMD-check](https://github.com/seankross/mario/workflows/R-CMD-check/badge.svg)](https://github.com/seankross/mario/actions)
<!-- badges: end -->

Explore, deconstruct, and analyze pipes.

## Installation

You can install Mario using the `remotes` package:

``` r
remotes::install_github("seankross/mario@main")
```

## Getting Started

``` r
library(tidyverse)
library(mario)

(Formaldehyde %>% 
  slice(1:3) %>% 
  parse_pipeline() -> pipeline_call)

#> Formaldehyde %>% slice(1:3)

pipeline_call %>% 
  get_verbs()

#> [[1]]
#> Formaldehyde
#>
#> [[2]]
#> slice(1:3)

pipeline_call %>% 
  get_verbs() %>% 
  get_data_steps()

#> [[1]]
#> # A tibble: 6 × 2
#>    carb optden
#>   <dbl>  <dbl>
#> 1   0.1  0.086
#> 2   0.3  0.269
#> 3   0.5  0.446
#> 4   0.6  0.538
#> 5   0.7  0.626
#> 6   0.9  0.782
#> 
#> [[2]]
#> # A tibble: 3 × 2
#>    carb optden
#>   <dbl>  <dbl>
#> 1   0.1  0.086
#> 2   0.3  0.269
#> 3   0.5  0.446
```

## Related Work

- [tidylog](https://github.com/elbersb/tidylog)
- [Fix Leaky Pipes in R](https://www.rostrum.blog/2019/04/07/fix-leaky-pipes/)
- [Unravel](https://github.com/nischalshrestha/DataTutor)
- [Datamations](https://github.com/microsoft/datamations)

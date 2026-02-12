# Calculates confidence interval for partial eta-squared in a fixed-effects ANOVA

Calculates confidence interval for partial eta-squared in a
fixed-effects ANOVA

## Usage

``` r
get.ci.partial.eta.squared(F.value, df1, df2, conf.level = 0.9)
```

## Arguments

- F.value:

  The F-value for the fixed-effect

- df1:

  Degrees of freedom for the fixed-effect

- df2:

  Degrees of freedom error

- conf.level:

  Confidence level (0 to 1). For partial eta-squared a confidence level
  of .90 is traditionally used rather than .95.

## Value

List with confidence interval values (LL and UL)

## Examples

``` r
# Smithson (2001) p. 619
get.ci.partial.eta.squared(F.value=6.00, df1=1, df2=42, conf.level=.90)
#> $LL
#> [1] 0.01170243
#> 
#> $UL
#> [1] 0.2801166
#> 
get.ci.partial.eta.squared(F.value=2.65, df1=6, df2=42, conf.level=.90)
#> $LL
#> [1] 0.01744483
#> 
#> $UL
#> [1] 0.3577174
#> 
get.ci.partial.eta.squared(F.value=2.60, df1=6, df2=42, conf.level=.90)
#> $LL
#> [1] 0.01473858
#> 
#> $UL
#> [1] 0.3537137
#> 

# Fidler & Thompson (2001) Fixed Effects 2x4 p. 594 (Table 6) / p. 596 (Table 8)
get.ci.partial.eta.squared(F.value=1.50, df1=1, df2=16, conf.level=.90)
#> $LL
#> [1] 0
#> 
#> $UL
#> [1] 0.3166901
#> 
get.ci.partial.eta.squared(F.value=4.00, df1=3, df2=16, conf.level=.90)
#> $LL
#> [1] 0.03574041
#> 
#> $UL
#> [1] 0.5670909
#> 
get.ci.partial.eta.squared(F.value=1.50, df1=3, df2=16, conf.level=.90)
#> $LL
#> [1] 0
#> 
#> $UL
#> [1] 0.3777952
#> 
```

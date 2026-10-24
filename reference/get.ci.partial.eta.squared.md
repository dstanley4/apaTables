# Calculates confidence interval for partial eta-squared in a fixed-effects ANOVA

`get.ci.partial.eta.squared()` calculates the confidence interval for
partial eta-squared from an F-value and the degrees of freedom for it,
for an effect in a fixed-effects ANOVA.
[`apa.aov.table`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
uses it for the intervals in the ANOVA table.

## Usage

``` r
get.ci.partial.eta.squared(F.value, df1, df2, conf.level = 0.9)
```

## Arguments

- F.value:

  The F-value for the fixed effect.

- df1:

  Degrees of freedom for the fixed effect.

- df2:

  Degrees of freedom for error.

- conf.level:

  Confidence level (0 to 1). For partial eta-squared a confidence level
  of .90 is traditionally used rather than .95.

## Value

A list with the lower (`LL`) and upper (`UL`) limits of the interval.

## Details

The interval is found for the noncentrality parameter (lambda) of the F
distribution with
[`MBESS::conf.limits.ncf()`](https://rdrr.io/pkg/MBESS/man/conf.limits.ncf.html),
and each limit is converted to partial eta-squared as lambda / (lambda +
df1 + df2 + 1), the approach described by Smithson (2001) and Fidler and
Thompson (2001). A lower limit that cannot be found is set to 0, and an
upper limit to 1.

## References

Fidler, F., & Thompson, B. (2001). Computing correct confidence
intervals for ANOVA fixed- and random-effects effect sizes. Educational
and Psychological Measurement, 61(4), 575-604.

Smithson, M. (2001). Correct confidence intervals for various regression
effect sizes and parameters: The importance of noncentral distributions
in computing intervals. Educational and Psychological Measurement,
61(4), 605-632.

## Examples

``` r
# Smithson (2001) p. 619, with the published 90% intervals
# row effect: published [.0117, .2801]
get.ci.partial.eta.squared(F.value = 6.00, df1 = 1, df2 = 42, conf.level = .90)
#> $LL
#> [1] 0.01170243
#> 
#> $UL
#> [1] 0.2801166
#> 
# column effect: published [.0174, .3577]
get.ci.partial.eta.squared(F.value = 2.65, df1 = 6, df2 = 42, conf.level = .90)
#> $LL
#> [1] 0.01744483
#> 
#> $UL
#> [1] 0.3577174
#> 
# interaction: published [.0093, .3455]; this gives [.0094, .3455], the
# difference being rounding in the fourth decimal
get.ci.partial.eta.squared(F.value = 2.50, df1 = 6, df2 = 42, conf.level = .90)
#> $LL
#> [1] 0.009356531
#> 
#> $UL
#> [1] 0.3455225
#> 

# Fidler & Thompson (2001) Fixed Effects 2x4 p. 594 (Table 6) / p. 596 (Table 8)
get.ci.partial.eta.squared(F.value = 1.50, df1 = 1, df2 = 16, conf.level = .90)
#> $LL
#> [1] 0
#> 
#> $UL
#> [1] 0.3166901
#> 
get.ci.partial.eta.squared(F.value = 4.00, df1 = 3, df2 = 16, conf.level = .90)
#> $LL
#> [1] 0.03574041
#> 
#> $UL
#> [1] 0.5670909
#> 
get.ci.partial.eta.squared(F.value = 1.50, df1 = 3, df2 = 16, conf.level = .90)
#> $LL
#> [1] 0
#> 
#> $UL
#> [1] 0.3777952
#> 
```

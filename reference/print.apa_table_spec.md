# Print an apa_table_spec to the console

Renders a render-agnostic table spec as plain text: "Table N" (when a
number is set), the flattened title, a fixed-width body (multi-line
cells stacked on continuation rows), then notes prefixed with "Note. ".

## Usage

``` r
# S3 method for class 'apa_table_spec'
print(x, ...)
```

## Arguments

- x:

  an \`apa_table_spec\` object.

- ...:

  unused; present for S3 method consistency.

## Value

\`x\` invisibly.

## Examples

``` r
spec <- apa.cor.table(data = attitude)$spec
print(spec)
#> 
#> Table 1
#> 
#> Descriptive Statistics and Correlations
#> 
#> Variable        N      M     SD            1            2            3            4           5            6
#> 1. rating      30  64.63  12.17                                                                             
#> 2. complaints  30  66.60  13.31        .83**                                                                
#>                                   [.66, .91]                                                                
#>                                     p < .001                                                                
#> 3. privileges  30  53.13  12.24         .43*        .56**                                                   
#>                                   [.08, .68]   [.25, .76]                                                   
#>                                     p = .019     p = .001                                                   
#> 4. learning    30  56.37  11.74        .62**        .60**        .49**                                      
#>                                   [.34, .80]   [.30, .79]   [.16, .72]                                      
#>                                     p < .001     p < .001     p = .006                                      
#> 5. raises      30  64.63  10.40        .59**        .67**         .45*        .64**                         
#>                                   [.29, .78]   [.41, .83]   [.10, .69]   [.36, .81]                         
#>                                     p < .001     p < .001     p = .014     p < .001                         
#> 6. critical    30  74.77   9.89          .16          .19          .15          .12        .38*             
#>                                  [-.22, .49]  [-.19, .51]  [-.22, .48]  [-.25, .46]  [.02, .65]             
#>                                     p = .409     p = .321     p = .438     p = .542    p = .040             
#> 7. advance     30  42.93  10.29          .16          .22          .34        .53**       .57**          .28
#>                                  [-.22, .49]  [-.15, .54]  [-.02, .63]   [.21, .75]  [.27, .77]  [-.09, .58]
#>                                     p = .413     p = .233     p = .063     p = .003    p < .001     p = .129
#> 
#> Note. N = number of cases. M = mean. SD = standard deviation. Values in square brackets indicate the 95% confidence interval.
#> Note. * indicates p < .05. ** indicates p < .01.
```

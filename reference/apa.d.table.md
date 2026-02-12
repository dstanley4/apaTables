# Creates a d-values for all pairwise (two-sample) comparisons in APA style

Creates a d-values for all pairwise (two-sample) comparisons in APA
style

## Usage

``` r
apa.d.table(
  iv,
  dv,
  data,
  filename = NA,
  table.number = 0,
  show.conf.interval = TRUE,
  landscape = TRUE
)
```

## Arguments

- iv:

  Name of independent variable column in data frame for all paired
  comparisons

- dv:

  Name of dependent variable column in data frame for all paired
  comparisons

- data:

  Project data frame name

- filename:

  (optional) Output filename document filename (must end in .rtf or .doc
  only)

- table.number:

  Integer to use in table number output line

- show.conf.interval:

  (TRUE/FALSE) Display confidence intervals in table. This argument is
  deprecated and will be removed from later versions.

- landscape:

  (TRUE/FALSE) Make RTF file landscape

## Value

APA table object

## Examples

``` r
# View top few rows of viagra data set from Discovering Statistics Using R
head(viagra)
#>       dose libido
#> 1  Placebo      3
#> 2  Placebo      2
#> 3  Placebo      1
#> 4  Placebo      1
#> 5  Placebo      4
#> 6 Low Dose      5

# Use apa.d.table function
table1 <- apa.d.table(iv = dose, dv = libido, data = viagra)


# Save Table 1 in a .doc document
apa.save(filename = file.path(tempdir(), "table1.doc"), table1)


# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)
#> \renewcommand{\arraystretch}{1}
#> \begin{landscape}\begin{table}
#> 
#> \caption{Means, Standard Deviations, And $D$-Values With Confidence Intervals}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lcccc}
#> \toprule
#> Variable & $M$ & $SD$ & 1 & 2\\
#> \midrule
#> 1. Placebo & 2.20 & 1.30 &  & \\
#>  &  &  &  & \\
#> 2. Low Dose & 3.20 & 1.30 & 0.77 & \\
#>  &  &  & {}[-0.55, 2.04] & \\
#>  &  &  &  & \\
#> 3. High Dose & 5.00 & 1.58 & 1.93 & 1.24\\
#>  &  &  & {}[0.34, 3.44] & {}[-0.17, 2.59]\\
#>  &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $M$ indicates mean. $SD$ indicates standard deviation. $d$-values are estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, \& Rothstein (2009). $d$-values not calculated if unequal variances prevented pooling. Values in square brackets indicate the 95\% confidence interval for each $d$-value. 
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \end{landscape}
#> \renewcommand{\arraystretch}{1}
#>  

# delete demo file
unlink(file.path(tempdir(), "table1.doc"))
```

# Creates a correlation table in APA style with means and standard deviations

Creates a correlation table in APA style with means and standard
deviations

## Usage

``` r
apa.cor.table(
  data,
  filename = NA,
  table.number = 0,
  show.conf.interval = TRUE,
  show.sig.stars = TRUE,
  show.pvalue = TRUE,
  landscape = TRUE
)
```

## Arguments

- data:

  Project data frame

- filename:

  (optional) Output filename document filename (must end in .rtf or .doc
  only)

- table.number:

  Integer to use in table number output line

- show.conf.interval:

  (TRUE/FALSE) Display confidence intervals in table. This argument is
  deprecated and will be removed from later versions.

- show.sig.stars:

  (TRUE/FALSE) Display stars for significance in table.

- show.pvalue:

  (TRUE/FALSE) Display p-value in table.

- landscape:

  (TRUE/FALSE) Make RTF file landscape

## Value

APA table object

## Examples

``` r
# View top few rows of attitude data set
head(attitude)
#>   rating complaints privileges learning raises critical advance
#> 1     43         51         30       39     61       92      45
#> 2     63         64         51       54     63       73      47
#> 3     71         70         68       69     76       86      48
#> 4     61         63         45       47     54       84      35
#> 5     81         78         56       66     71       83      47
#> 6     43         55         49       44     54       49      34

# Use apa.cor.table function
table1 <- apa.cor.table(attitude)


# Save Table 1 in a .doc document
apa.save(filename = file.path(tempdir(), "table1.doc"), table1)


# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)
#> \renewcommand{\arraystretch}{1}
#> \begin{landscape}\begin{table}
#> 
#> \caption{Descriptive Statistics And Correlations}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrccccccc}
#> \toprule
#> Variable & $N$ & $M$ & $SD$ & 1 & 2 & 3 & 4 & 5 & 6\\
#> \midrule
#> 1. rating & 30 & 64.63 & 12.17 &  &  &  &  &  & \\
#>  &  &  &  &  &  &  &  &  & \\
#>  &  &  &  &  &  &  &  &  & \\
#>  &  &  &  &  &  &  &  &  & \\
#> 2. complaints & 30 & 66.60 & 13.31 & .83$^{**}$ &  &  &  &  & \\
#>  &  &  &  & {}[.66, .91] &  &  &  &  & \\
#>  &  &  &  & $p$ < .001 &  &  &  &  & \\
#>  &  &  &  &  &  &  &  &  & \\
#> 3. privileges & 30 & 53.13 & 12.24 & .43$^{*}$ & .56$^{**}$ &  &  &  & \\
#>  &  &  &  & {}[.08, .68] & {}[.25, .76] &  &  &  & \\
#>  &  &  &  & $p$ = .019 & $p$ = .001 &  &  &  & \\
#>  &  &  &  &  &  &  &  &  & \\
#> 4. learning & 30 & 56.37 & 11.74 & .62$^{**}$ & .60$^{**}$ & .49$^{**}$ &  &  & \\
#>  &  &  &  & {}[.34, .80] & {}[.30, .79] & {}[.16, .72] &  &  & \\
#>  &  &  &  & $p$ < .001 & $p$ < .001 & $p$ = .006 &  &  & \\
#>  &  &  &  &  &  &  &  &  & \\
#> 5. raises & 30 & 64.63 & 10.40 & .59$^{**}$ & .67$^{**}$ & .45$^{*}$ & .64$^{**}$ &  & \\
#>  &  &  &  & {}[.29, .78] & {}[.41, .83] & {}[.10, .69] & {}[.36, .81] &  & \\
#>  &  &  &  & $p$ < .001 & $p$ < .001 & $p$ = .014 & $p$ < .001 &  & \\
#>  &  &  &  &  &  &  &  &  & \\
#> 6. critical & 30 & 74.77 & 9.89 & .16 & .19 & .15 & .12 & .38$^{*}$ & \\
#>  &  &  &  & {}[-.22, .49] & {}[-.19, .51] & {}[-.22, .48] & {}[-.25, .46] & {}[.02, .65] & \\
#>  &  &  &  & $p$ = .409 & $p$ = .321 & $p$ = .438 & $p$ = .542 & $p$ = .040 & \\
#>  &  &  &  &  &  &  &  &  & \\
#> 7. advance & 30 & 42.93 & 10.29 & .16 & .22 & .34 & .53$^{**}$ & .57$^{**}$ & .28\\
#>  &  &  &  & {}[-.22, .49] & {}[-.15, .54] & {}[-.02, .63] & {}[.21, .75] & {}[.27, .77] & {}[-.09, .58]\\
#>  &  &  &  & $p$ = .413 & $p$ = .233 & $p$ = .063 & $p$ = .003 & $p$ < .001 & $p$ = .129\\
#>  &  &  &  &  &  &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. \textit{N} = number of cases. \textit{M} = mean. \textit{SD} = standard deviation. Square brackets = 95\% confidence interval. \newline  * indicates $p$ < .05. ** indicates $p$ < .01.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \end{landscape}
#> \renewcommand{\arraystretch}{1}
#>  

# delete demo file
unlink(file.path(tempdir(), "table1.doc"))
```

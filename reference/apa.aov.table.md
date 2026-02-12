# Creates a fixed-effects ANOVA table in APA style

Creates a fixed-effects ANOVA table in APA style

## Usage

``` r
apa.aov.table(
  lm_output,
  filename,
  table.number = 0,
  conf.level = 0.9,
  type = 3
)
```

## Arguments

- lm_output:

  Regression (i.e., lm) result objects. Typically, one for each block in
  the regression.

- filename:

  (optional) Output filename document filename (must end in .rtf or .doc
  only)

- table.number:

  Integer to use in table number output line

- conf.level:

  Level of confidence for interval around partial eta-squared (.90 or
  .95). A value of .90 is the default, this helps to create consistency
  between the CI overlapping with zero and conclusions based on the
  p-value.

- type:

  Sum of Squares Type. Type II or Type III; specify, 2 or 3,
  respectively. Default value is 3.

## Value

APA table object

## References

Smithson, M. (2001). Correct confidence intervals for various regression
effect sizes and parameters: The importance of noncentral distributions
in computing intervals. Educational and Psychological Measurement,
61(4), 605-632.

Fidler, F., & Thompson, B. (2001). Computing correct confidence
intervals for ANOVA fixed-and random-effects effect sizes. Educational
and Psychological Measurement, 61(4), 575-604.

## Examples

``` r
#Example 1: 1-way from Field et al. (2012) Discovery Statistics Using R
op <- options(contrasts = c("contr.helmert", "contr.poly"))
lm_output <- lm(libido ~ dose, data = viagra)
table1 <- apa.aov.table(lm_output, table.number = 4)


# Example 2: 2-way from Fidler & Thompson (2001)
# You must set these contrasts to ensure values match SPSS
lm_output <- lm(dv ~ a*b, data = fidler_thompson)
table2 <- apa.aov.table(lm_output, table.number = 5)


#Example 3: 2-way from Field et al. (2012) Discovery Statistics Using R
# You must set these contrasts to ensure values match SPSS
lm_output <- lm(attractiveness ~ gender*alcohol, data = goggles)
table3 <- apa.aov.table(lm_output, table.number = 6)


# Save all three table in the same .doc document
apa.save(filename = file.path(tempdir(), "my_tables.doc"), table1, table2, table3)


# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)
#> \renewcommand{\arraystretch}{1}\begin{table}
#> 
#> \caption{Fixed-Effects Anova Results For Libido
#> }
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrrrrrc}
#> \toprule
#> Predictor & $SS$ & $df$ & $MS$ & $F$ & $p$ & $\eta_{partial}^2$ & 90\% CI\\
#> \midrule
#> (Intercept) & 180.27 & 1 & 180.27 & 91.66 & <.001 &  & \\
#> dose & 20.13 & 2 & 10.06 & 5.12 & .025 & .46 & {}[.04, .62]\\
#> Error & 23.60 & 12 & 1.97 &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $SS$ = Sum of squares. $df$ = degrees of freedom. $MS$ = mean square. CI indicates the confidence interval for $\eta_{partial}^2$.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \renewcommand{\arraystretch}{1}
#>  
apa.knit.table.for.pdf(table2)
#> \renewcommand{\arraystretch}{1}\begin{table}
#> 
#> \caption{Fixed-Effects Anova Results For Dv
#> }
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrrrrrc}
#> \toprule
#> Predictor & $SS$ & $df$ & $MS$ & $F$ & $p$ & $\eta_{partial}^2$ & 90\% CI\\
#> \midrule
#> (Intercept) & 150.00 & 1 & 150.00 & 150.00 & <.001 &  & \\
#> a & 1.50 & 1 & 1.50 & 1.50 & .238 & .09 & {}[.00, .32]\\
#> b & 12.00 & 3 & 4.00 & 4.00 & .027 & .43 & {}[.04, .57]\\
#> a x b & 4.50 & 3 & 1.50 & 1.50 & .253 & .22 & {}[.00, .38]\\
#> Error & 16.00 & 16 & 1.00 &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $SS$ = Sum of squares. $df$ = degrees of freedom. $MS$ = mean square. CI indicates the confidence interval for $\eta_{partial}^2$.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \renewcommand{\arraystretch}{1}
#>  
apa.knit.table.for.pdf(table3)
#> \renewcommand{\arraystretch}{1}\begin{table}
#> 
#> \caption{Fixed-Effects Anova Results For Attractiveness
#> }
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrrrrrc}
#> \toprule
#> Predictor & $SS$ & $df$ & $MS$ & $F$ & $p$ & $\eta_{partial}^2$ & 90\% CI\\
#> \midrule
#> (Intercept) & 163333.33 & 1 & 163333.33 & 1967.03 & <.001 &  & \\
#> gender & 168.75 & 1 & 168.75 & 2.03 & .161 & .05 & {}[.00, .18]\\
#> alcohol & 3332.29 & 2 & 1666.14 & 20.07 & <.001 & .49 & {}[.28, .60]\\
#> gender x alcohol & 1978.12 & 2 & 989.06 & 11.91 & <.001 & .36 & {}[.15, .49]\\
#> Error & 3487.50 & 42 & 83.04 &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $SS$ = Sum of squares. $df$ = degrees of freedom. $MS$ = mean square. CI indicates the confidence interval for $\eta_{partial}^2$.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \renewcommand{\arraystretch}{1}
#>  


# delete demo file
unlink(file.path(tempdir(), "my_tables.doc"))
options(op)
```

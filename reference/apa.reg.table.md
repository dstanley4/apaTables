# Creates a regresion table in APA style

Creates a regresion table in APA style

## Usage

``` r
apa.reg.table(..., filename = NA, table.number = 0, prop.var.conf.level = 0.95)
```

## Arguments

- ...:

  Regression (i.e., lm) result objects. Typically, one for each block in
  the regression.

- filename:

  (optional) Output filename document filename (must end in .rtf or .doc
  only)

- table.number:

  Integer to use in table number output line

- prop.var.conf.level:

  Level of confidence (.90 or .95, default .95) for interval around sr2,
  R2, and Delta R2. Use of .90 confidence level helps to create
  consistency between the CI overlapping with zero and conclusions based
  on the p-value for that block (or block difference).

## Value

APA table object

## References

sr2 and delta R2 confidence intervals calculated via:

Alf Jr, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for
the difference between two squared multiple correlations: A simplified
approach. Psychological Methods, 4(1), 70.

Note that Algina, Keselman, & Penfield (2008) found this approach can
under some circumstances lead to inaccurate CIs on proportion of
variance values. You might consider using the Algina, Keselman, &
Penfield (2008) approach via the apa.reg.boot.table function

## Examples

``` r
# View top few rows of goggles data set
# from Discovering Statistics Using R
head(album)
#>    adverts sales airplay attract
#> 1   10.256   330      43      10
#> 2  985.685   120      28       7
#> 3 1445.563   360      35       7
#> 4 1188.193   270      33       7
#> 5  574.513   220      44       5
#> 6  568.954   170      19       5

# Single block example
blk1 <- lm(sales ~ adverts + airplay, data=album)
apa.reg.table(blk1)
#> 
#> 
#> Table 0 
#> 
#> Regression Predicting Sales
#>  
#> 
#>    Predictor       b       b_95%_CI beta  beta_95%_CI Unique_R2 Unique_95%_CI
#>  (Intercept) 41.12** [22.72, 59.53]                                          
#>      adverts  0.09**   [0.07, 0.10] 0.52 [0.44, 0.61]     .27**    [.18, .36]
#>      airplay  3.59**   [3.02, 4.15] 0.55 [0.46, 0.63]     .29**    [.20, .38]
#>                                                                              
#>                                                                              
#>                                                                              
#>      r             Fit
#>                       
#>  .58**                
#>  .60**                
#>            R2 = .629**
#>        95% CI[.55,.69]
#>                       
#> 
#> Note. N = 200. b = unstandardized regression weight. beta = standardized regression weight. Unique R2 = semipartial correlation squared. r = zero-order correlation. CI = confidence interval.  
#> * indicates p < .05. ** indicates p < .01.
#>  
#> 
table1 <- apa.reg.table(blk1,table.number = 1)

# \donttest{
# Two block example, more than two blocks can be used
blk1 <- lm(sales ~ adverts, data=album)
blk2 <- lm(sales ~ adverts + airplay + attract, data=album)
table2 <- apa.reg.table(blk1, blk2, table.number = 2)

# Interaction product-term test with blocks
blk1 <- lm(sales ~ adverts + airplay, data=album)
blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
table3 <- apa.reg.table(blk1, blk2, table.number = 3)

# Interaction product-term test with blocks and additional product terms
blk1<-lm(sales ~ adverts + airplay, data=album)
blk2<-lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay), data=album)
blk3<-lm(sales~adverts+airplay+I(adverts*adverts)+I(airplay*airplay)+I(adverts*airplay),data=album)
table4 <- apa.reg.table(blk1,blk2,blk3, table.number = 4)

#Interaction product-term test with single regression (i.e., semi-partial correlation focus)
blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
table5 <- apa.reg.table(blk1, table.number = 5)

# Save Table 1 in a .doc document
apa.save(filename = file.path(tempdir(), "regression_tables.doc"),
         table1,
         table2,
         table3,
         table4,
         table5)

# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)
#> \renewcommand{\arraystretch}{1}
#> \begin{landscape}\begin{table}
#> 
#> \caption{Regression Predicting Sales}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrcrcrcc}
#> \toprule
#> Predictor & $b$ & 95\% CI & $beta$ & 95\% CI & Unique $R^2$ & 95\% CI & $r$ & Fit\\
#> \midrule
#> (Intercept) & 41.12** & {}[22.72, 59.53] &  &  &  &  &  & \\
#> adverts & 0.09** & {}[0.07, 0.10] & 0.52 & {}[0.44, 0.61] & .27** & {}[.18, .36] & .58** & \\
#> airplay & 3.59** & {}[3.02, 4.15] & 0.55 & {}[0.46, 0.63] & .29** & {}[.20, .38] & .60** & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .629**\\
#>  &  &  &  &  &  &  &  & 95\% CI[.55,.69]\\
#>  &  &  &  &  &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $N$ = 200. $b$ = unstandardized regression weight. $beta$ = standardized regression weight. Unique $R^2$ = semipartial correlation squared. $r$ = zero-order correlation. CI = confidence interval. \newline  * indicates $p$ < .05. ** indicates $p$ < .01.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \end{landscape}
#> \renewcommand{\arraystretch}{1}
#>  
apa.knit.table.for.pdf(table2)
#> \renewcommand{\arraystretch}{1}
#> \begin{landscape}\begin{table}
#> 
#> \caption{Hierarchical Multiple Regression Predicting Sales}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrcrcrccc}
#> \toprule
#> Predictor & $b$ & 95\% CI & $beta$ & 95\% CI & Unique $R^2$ & 95\% CI & $r$ & Fit & $\Delta$ Fit\\
#> \midrule
#> Model 1 &  &  &  &  &  &  &  &  & \\
#> (Intercept) & 134.14** & {}[119.28, 149.00] &  &  &  &  &  &  & \\
#> adverts & 0.10** & {}[0.08, 0.12] & 0.58 & {}[0.46, 0.69] & .33** & {}[.23, .43] & .58** &  & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .335** & \\
#>  &  &  &  &  &  &  &  & 95\% CI[.23,.43] & \\
#>  &  &  &  &  &  &  &  &  & \\
#> Model 2 &  &  &  &  &  &  &  &  & \\
#> (Intercept) & -26.61 & {}[-60.83, 7.60] &  &  &  &  &  &  & \\
#> adverts & 0.08** & {}[0.07, 0.10] & 0.51 & {}[0.43, 0.59] & .26** & {}[.17, .34] & .58** &  & \\
#> airplay & 3.37** & {}[2.82, 3.92] & 0.51 & {}[0.43, 0.60] & .25** & {}[.17, .33] & .60** &  & \\
#> attract & 11.09** & {}[6.28, 15.89] & 0.19 & {}[0.11, 0.27] & .04** & {}[.00, .07] & .33** &  & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .665** & $\Delta R^2$  = .330**\\
#>  &  &  &  &  &  &  &  & 95\% CI[.59,.72] & 95\% CI[.24, .42]\\
#>  &  &  &  &  &  &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $N$ = 200. $b$ = unstandardized regression weight. $beta$ = standardized regression weight. Unique $R^2$ = semipartial correlation squared. $r$ = zero-order correlation. CI = confidence interval. \newline  * indicates $p$ < .05. ** indicates $p$ < .01.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \end{landscape}
#> \renewcommand{\arraystretch}{1}
#>  
apa.knit.table.for.pdf(table3)
#> \renewcommand{\arraystretch}{1}
#> \begin{landscape}\begin{table}
#> 
#> \caption{Hierarchical Multiple Regression Predicting Sales}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrcrcrccc}
#> \toprule
#> Predictor & $b$ & 95\% CI & $beta$ & 95\% CI & Unique $R^2$ & 95\% CI & $r$ & Fit & $\Delta$ Fit\\
#> \midrule
#> Model 1 &  &  &  &  &  &  &  &  & \\
#> (Intercept) & 41.12** & {}[22.72, 59.53] &  &  &  &  &  &  & \\
#> adverts & 0.09** & {}[0.07, 0.10] & 0.52 & {}[0.44, 0.61] & .27** & {}[.18, .36] & .58** &  & \\
#> airplay & 3.59** & {}[3.02, 4.15] & 0.55 & {}[0.46, 0.63] & .29** & {}[.20, .38] & .60** &  & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .629** & \\
#>  &  &  &  &  &  &  &  & 95\% CI[.55,.69] & \\
#>  &  &  &  &  &  &  &  &  & \\
#> Model 2 &  &  &  &  &  &  &  &  & \\
#> (Intercept) & 28.30* & {}[1.09, 55.50] &  &  &  &  &  &  & \\
#> adverts & 0.11** & {}[0.07, 0.16] & 0.69 & {}[0.42, 0.96] & .05** & {}[.01, .08] & .58** &  & \\
#> airplay & 4.02** & {}[3.14, 4.91] & 0.61 & {}[0.48, 0.75] & .15** & {}[.08, .22] & .60** &  & \\
#> adverts x airplay & -0.00 & {}[-0.00, 0.00] & -0.19 & {}[-0.49, 0.11] & .00 & {}[-.01, .01] &  &  & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .632** & $\Delta R^2$  = .003\\
#>  &  &  &  &  &  &  &  & 95\% CI[.55,.69] & 95\% CI[-.01, .01]\\
#>  &  &  &  &  &  &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $N$ = 200. $b$ = unstandardized regression weight. $beta$ = standardized regression weight. Unique $R^2$ = semipartial correlation squared. $r$ = zero-order correlation. CI = confidence interval. \newline  * indicates $p$ < .05. ** indicates $p$ < .01.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \end{landscape}
#> \renewcommand{\arraystretch}{1}
#>  
apa.knit.table.for.pdf(table4)
#> \renewcommand{\arraystretch}{1}
#> \begin{landscape}\begin{table}
#> 
#> \caption{Hierarchical Multiple Regression Predicting Sales}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrcrcrccc}
#> \toprule
#> Predictor & $b$ & 95\% CI & $beta$ & 95\% CI & Unique $R^2$ & 95\% CI & $r$ & Fit & $\Delta$ Fit\\
#> \midrule
#> Model 1 &  &  &  &  &  &  &  &  & \\
#> (Intercept) & 41.12** & {}[22.72, 59.53] &  &  &  &  &  &  & \\
#> adverts & 0.09** & {}[0.07, 0.10] & 0.52 & {}[0.44, 0.61] & .27** & {}[.18, .36] & .58** &  & \\
#> airplay & 3.59** & {}[3.02, 4.15] & 0.55 & {}[0.46, 0.63] & .29** & {}[.20, .38] & .60** &  & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .629** & \\
#>  &  &  &  &  &  &  &  & 95\% CI[.55,.69] & \\
#>  &  &  &  &  &  &  &  &  & \\
#> Model 2 &  &  &  &  &  &  &  &  & \\
#> (Intercept) & 48.77** & {}[19.73, 77.82] &  &  &  &  &  &  & \\
#> adverts & 0.10** & {}[0.05, 0.14] & 0.58 & {}[0.33, 0.83] & .04** & {}[.01, .07] & .58** &  & \\
#> airplay & 2.65** & {}[0.67, 4.63] & 0.40 & {}[0.10, 0.70] & .01** & {}[-.01, .03] & .60** &  & \\
#> adverts x adverts & -0.00 & {}[-0.00, 0.00] & -0.05 & {}[-0.31, 0.20] & .00 & {}[-.00, .00] &  &  & \\
#> airplay x airplay & 0.02 & {}[-0.02, 0.05] & 0.15 & {}[-0.15, 0.45] & .00 & {}[-.01, .01] &  &  & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .631** & $\Delta R^2$  = .002\\
#>  &  &  &  &  &  &  &  & 95\% CI[.55,.69] & 95\% CI[-.01, .01]\\
#>  &  &  &  &  &  &  &  &  & \\
#> Model 3 &  &  &  &  &  &  &  &  & \\
#> (Intercept) & 37.30* & {}[2.84, 71.77] &  &  &  &  &  &  & \\
#> adverts & 0.12** & {}[0.06, 0.18] & 0.72 & {}[0.38, 1.06] & .03** & {}[.00, .06] & .58** &  & \\
#> airplay & 3.07** & {}[0.98, 5.15] & 0.47 & {}[0.15, 0.78] & .02** & {}[-.01, .04] & .60** &  & \\
#> adverts x adverts & -0.00 & {}[-0.00, 0.00] & -0.03 & {}[-0.29, 0.23] & .00 & {}[-.00, .00] &  &  & \\
#> airplay x airplay & 0.02 & {}[-0.02, 0.05] & 0.15 & {}[-0.15, 0.45] & .00 & {}[-.01, .01] &  &  & \\
#> adverts x airplay & -0.00 & {}[-0.00, 0.00] & -0.19 & {}[-0.49, 0.12] & .00 & {}[-.01, .01] &  &  & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .634** & $\Delta R^2$  = .003\\
#>  &  &  &  &  &  &  &  & 95\% CI[.55,.69] & 95\% CI[-.01, .01]\\
#>  &  &  &  &  &  &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $N$ = 200. $b$ = unstandardized regression weight. $beta$ = standardized regression weight. Unique $R^2$ = semipartial correlation squared. $r$ = zero-order correlation. CI = confidence interval. \newline  * indicates $p$ < .05. ** indicates $p$ < .01.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \end{landscape}
#> \renewcommand{\arraystretch}{1}
#>  
apa.knit.table.for.pdf(table5)
#> \renewcommand{\arraystretch}{1}
#> \begin{landscape}\begin{table}
#> 
#> \caption{Regression Predicting Sales}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrcrcrcc}
#> \toprule
#> Predictor & $b$ & 95\% CI & $beta$ & 95\% CI & Unique $R^2$ & 95\% CI & $r$ & Fit\\
#> \midrule
#> (Intercept) & 28.30* & {}[1.09, 55.50] &  &  &  &  &  & \\
#> adverts & 0.11** & {}[0.07, 0.16] & 0.69 & {}[0.42, 0.96] & .05** & {}[.01, .08] & .58** & \\
#> airplay & 4.02** & {}[3.14, 4.91] & 0.61 & {}[0.48, 0.75] & .15** & {}[.08, .22] & .60** & \\
#> adverts x airplay & -0.00 & {}[-0.00, 0.00] & -0.19 & {}[-0.49, 0.11] & .00 & {}[-.01, .01] &  & \\
#>  &  &  &  &  &  &  &  & $R^2$ = .632**\\
#>  &  &  &  &  &  &  &  & 95\% CI[.55,.69]\\
#>  &  &  &  &  &  &  &  & \\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. $N$ = 200. $b$ = unstandardized regression weight. $beta$ = standardized regression weight. Unique $R^2$ = semipartial correlation squared. $r$ = zero-order correlation. CI = confidence interval. \newline  * indicates $p$ < .05. ** indicates $p$ < .01.
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \end{landscape}
#> \renewcommand{\arraystretch}{1}
#>  

# delete demo file
unlink(file.path(tempdir(), "regression_tables.doc"))
# }
```

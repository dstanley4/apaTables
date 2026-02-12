# Creates a table of means and standard deviations for a 2-way ANOVA design in APA style

Creates a table of means and standard deviations for a 2-way ANOVA
design in APA style

## Usage

``` r
apa.2way.table(
  iv1,
  iv2,
  dv,
  data,
  filename = NA,
  table.number = 0,
  show.conf.interval = FALSE,
  show.marginal.means = FALSE,
  landscape = TRUE
)
```

## Arguments

- iv1:

  Name of independent variable 1 column in data frame

- iv2:

  Name of independent variable 2 column in data frame

- dv:

  Name of dependent variable column in data frame

- data:

  Project data frame name

- filename:

  (optional) Output filename document filename (must end in .rtf or .doc
  only)

- table.number:

  Integer to use in table number output line

- show.conf.interval:

  (TRUE/FALSE) Display confidence intervals in table. Negates
  show.marginal.means = TRUE.

- show.marginal.means:

  (TRUE/FALSE) Show marginal means in output. Only used if
  show.conf.interval = FALSE.

- landscape:

  (TRUE/FALSE) Make RTF file landscape

## Value

APA table object

## Examples

``` r
# Example 2: 2-way from Fidler & Thompson (2001)

table2 <- apa.2way.table(iv1 = a, iv2 = b, dv = dv,
                          data = fidler_thompson,
                          landscape = TRUE,
                          table.number = 2)


# Example 3: 2-way from Field et al. (2012) Discovery Statistics Using R

table3 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                         data = goggles, table.number = 3)


# Save both Table 2 and Table 3 in a single .doc document
apa.save(filename = file.path(tempdir(), "my_tables.doc"), table2, table3)

# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table2)
#> \renewcommand{\arraystretch}{1}
#> \begin{landscape}\begin{table}
#> 
#> \caption{Descriptive Statistics For Dv In A 2(A) X 4(B) Design}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrrrrrrr}
#> \toprule
#> \multicolumn{1}{c}{ } & \multicolumn{8}{c}{b} \\
#> \cmidrule(l{3pt}r{3pt}){2-9}
#> \multicolumn{1}{c}{ } & \multicolumn{2}{c}{1} & \multicolumn{2}{c}{2} & \multicolumn{2}{c}{3} & \multicolumn{2}{c}{4} \\
#> \cmidrule(l{3pt}r{3pt}){2-3} \cmidrule(l{3pt}r{3pt}){4-5} \cmidrule(l{3pt}r{3pt}){6-7} \cmidrule(l{3pt}r{3pt}){8-9}
#> a & $M$ & $SD$ & $M$ & $SD$ & $M$ & $SD$ & $M$ & $SD$\\
#> \midrule
#> 1 & 1.00 & 1.00 & 2.00 & 1.00 & 3.00 & 1.00 & 3.00 & 1.00\\
#> 2 & 2.00 & 1.00 & 3.00 & 1.00 & 2.00 & 1.00 & 4.00 & 1.00\\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. \textit{M} = mean. \textit{SD} = standard deviation. 
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
#> \caption{Descriptive Statistics For Attractiveness In A 2(Gender) X 3(Alcohol) Design}
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrrrrrr}
#> \toprule
#> \multicolumn{1}{c}{ } & \multicolumn{6}{c}{alcohol} \\
#> \cmidrule(l{3pt}r{3pt}){2-7}
#> \multicolumn{1}{c}{ } & \multicolumn{2}{c}{None} & \multicolumn{2}{c}{2 Pints} & \multicolumn{2}{c}{4 Pints} \\
#> \cmidrule(l{3pt}r{3pt}){2-3} \cmidrule(l{3pt}r{3pt}){4-5} \cmidrule(l{3pt}r{3pt}){6-7}
#> gender & $M$ & $SD$ & $M$ & $SD$ & $M$ & $SD$\\
#> \midrule
#> Female & 60.62 & 4.96 & 62.50 & 6.55 & 57.50 & 7.07\\
#> Male & 66.88 & 10.33 & 66.88 & 12.52 & 35.62 & 10.84\\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. \textit{M} = mean. \textit{SD} = standard deviation. 
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \end{landscape}
#> \renewcommand{\arraystretch}{1}
#>  

# delete demo file
unlink(file.path(tempdir(), "my_tables.doc"))
```

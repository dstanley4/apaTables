# Creates a table of means and standard deviations for a 1-way ANOVA design in APA style

Creates a table of means and standard deviations for a 1-way ANOVA
design in APA style

## Usage

``` r
apa.1way.table(
  iv,
  dv,
  data,
  filename = NA,
  table.number = 0,
  show.conf.interval = FALSE,
  landscape = FALSE
)
```

## Arguments

- iv:

  Name of independent variable column in data frame

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

  (TRUE/FALSE) Display confidence intervals in table.

- landscape:

  (TRUE/FALSE) Make RTF file landscape

## Value

APA table object

## Examples

``` r
# Example 1: 1-way from Field et al. (2012) Discovery Statistics Using R

table1 <- apa.1way.table(iv = dose, dv = libido,
               data = viagra, table.number = 1)

apa.save(filename = file.path(tempdir(), "table1.doc"), table1)

# Create a table for your PDF
# Include the line below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)
#> \renewcommand{\arraystretch}{1}\begin{table}
#> 
#> \caption{Descriptive Statistics For Libido For Each Level Of Dose. }
#> \fontsize{10}{12}\selectfont
#> \begin{threeparttable}
#> \begin{tabular}[t]{lrr}
#> \toprule
#> dose & $M$ & $SD$\\
#> \midrule
#> Placebo & 2.20 & 1.30\\
#> Low Dose & 3.20 & 1.30\\
#> High Dose & 5.00 & 1.58\\
#> \bottomrule
#> \end{tabular}
#> \begin{tablenotes}
#> \item \textit{Note}. \textit{M} = mean. \textit{SD} = standard deviation. 
#> \end{tablenotes}
#> \end{threeparttable}
#> \end{table}
#> \renewcommand{\arraystretch}{1}
#>  

# delete demo file
unlink(file.path(tempdir(), "table1.doc"))
```

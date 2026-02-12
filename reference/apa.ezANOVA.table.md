# Creates an ANOVA table in APA style based output of ezANOVA command from ez package

Deprecated. The ez package has been archived on CRAN. Please use
\[apa.afex.table()\] with output from \[afex::aov_ez()\] instead.

## Usage

``` r
apa.ezANOVA.table(
  ez.output,
  correction = "GG",
  table.title = "",
  filename,
  table.number = 0
)
```

## Arguments

- ez.output:

  Output object from ezANOVA command from ez package

- correction:

  Type of sphercity correction: "none", "GG", or "HF" corresponding to
  none, Greenhouse-Geisser and Huynh-Feldt, respectively.

- table.title:

  String containing text for table title

- filename:

  (optional) Output filename document filename (must end in .rtf or .doc
  only)

- table.number:

  Integer to use in table number output line

## Value

APA table object

## Examples

``` r
if (FALSE) { # \dontrun{
# Note: ez package has been archived on CRAN.
# Use apa.afex.table() with afex::aov_ez() instead.

#
# ** Example 1: Between Participant Predictors
#

goggles <- apaTables::goggles

# Use ezANOVA
# Be sure use the options command, as below, to ensure sufficient digits

op <- options(digits = 10)
goggles_results <- ez::ezANOVA(data = goggles,
                          dv = attractiveness,
                          between = .(gender, alcohol),
                          participant ,
                          detailed = TRUE)

# Make APA table
goggles_table <- apa.ezANOVA.table(goggles_results)

# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(goggles_table)
options(op)
} # }
```

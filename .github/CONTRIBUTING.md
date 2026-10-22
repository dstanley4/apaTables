# Contributing to apaTables

Thank you for helping to improve apaTables.

## Reporting a problem or suggesting a change

Please [open an issue](https://github.com/dstanley4/apaTables/issues). A
problem is easiest to fix when the issue includes:

- a short piece of R code that shows it, ideally using a data set that
  comes with R or apaTables (`attitude`, `mtcars`, `goggles`, `viagra`), so
  that it can be run as it is;
- what you expected to see, and what you saw instead (a screenshot helps
  for anything about how a table looks in a Word, PDF or HTML file);
- the output of `sessionInfo()`, run after `library(apaTables)`, which lists
  your R version, your apaTables version and the other packages loaded.

If the table is in a Quarto document, please also give:

- your Quarto version: type `quarto --version` in a terminal (in Positron,
  the Terminal tab -- not the R console). If that says the command is not
  found, give your Positron version instead, from About Positron;
- the document's `format:` line (for example `apaquarto-pdf`);
- the `version:` line of `_extensions/wjschne/apaquarto/_extension.yml`,
  in the folder that holds your document.

Suggestions for new tables or options are welcome as issues too.

## Code contributions

apaTables does not accept pull requests. If you have found a problem or
have an idea, please describe it in an issue instead -- that is the most
useful way to help.

## Code of conduct

Please note that apaTables is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By taking part in this
project you agree to abide by its terms.

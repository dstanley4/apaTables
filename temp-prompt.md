Create a new function `apa.afex.table()` in a new file `R/apaAFEX.R` that produces an
APA-style ANOVA table from `afex::aov_ez()` output (class `afex_aov`). This is a fresh
function — not a refactor of the existing `apa.ezANOVA.table()`.

## Scope

Support all three design types that `aov_ez()` handles:
- Between-subjects designs
- Within-subjects (repeated measures) designs
- Mixed (between + within) designs

Do NOT support `afex::mixed()` output — that uses lmer-based mixed-effects modeling,
which is a different statistical framework and should be a separate function if needed
later.

## Table contents

Use what `afex` natively provides via `anova(afex_obj)`:
- Effect names (from row names of the anova table)
- Numerator and denominator df (corrected for sphericity when applicable)
- MSE (this replaces SSn/SSd from the ez version — afex provides MSE natively)
- F statistic
- p value (corrected for sphericity when applicable)
- Generalized eta-squared (ges)
- Epsilon column for within-subjects effects (extract from `summary()`)

## Key parameters

- `afex.output`: The `afex_aov` object from `aov_ez()`
- `correction`: "GG", "HF", or "none" — controls sphericity correction
  (extract corrected values by calling `anova(afex_obj, correction = ...)`)
- `table.title`, `table.number`, `filename`: Same as existing ANOVA table functions

## Implementation notes

- Follow the same output structure as other apaTables functions (return `apa_table` class
  with `table_body`, `table_note`, `rtf.body`, `latex.body`, etc.)
- Reuse existing helpers: `strip.leading.zero()`, `get_rtf_column_names_anova()`,
  `get_txt_column_names_anova()`, `RtfTable`, `write.rtf.table()`
- Extract epsilon values from `summary(afex_obj)$pval.adjustments` — columns are
  `"GG eps"` and `"HF eps"`, with effect names as row names
- SS values (if ever needed) are in `summary(afex_obj)$univariate.tests` — columns
  `"Sum Sq"` and `"Error SS"`
- Separate between vs. within effects using `names(attr(obj, "within"))` and
  `names(attr(obj, "between"))` — these are named lists of factor levels
- Add `afex` to Suggests in DESCRIPTION (same treatment as `ez`)
- Create tests in `tests/testthat/test-apa-afex-table.R` using the same datasets
  (goggles, drink_attitude_wide, dating_wide)
- Update the vignette with `aov_ez()` examples alongside the existing `ezANOVA` examples

Go into planning mode to make this plan.
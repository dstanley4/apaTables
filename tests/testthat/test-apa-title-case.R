test_that("apa_title_case follows the APA rules", {
  # words of four letters or more capitalize, even prepositions
  expect_equal(apa_title_case("means with confidence intervals"),
               "Means With Confidence Intervals")
  # short conjunctions, articles and prepositions of three letters or fewer
  expect_equal(apa_title_case("statistics for libido by dose"),
               "Statistics for Libido by Dose")
  # the first word capitalizes even when it is a minor word
  expect_equal(apa_title_case("a design"), "A Design")
  # a capital the user typed on a function word is corrected
  expect_equal(apa_title_case("Score For Test"), "Score for Test")
})

test_that("apa_title_case never destroys an acronym", {
  expect_equal(apa_title_case("GPA and SES"), "GPA and SES")
  expect_equal(apa_title_case("predicting IQ"), "Predicting IQ")
})

test_that("apa_name_to_title separates, splits and title-cases", {
  expect_equal(apa_name_to_title("sales"),         "Sales")
  expect_equal(apa_name_to_title("GPA"),           "GPA")
  expect_equal(apa_name_to_title("self_esteem"),   "Self Esteem")
  expect_equal(apa_name_to_title("selfEsteem"),    "Self Esteem")
  expect_equal(apa_name_to_title("Score_For_Test"),"Score for Test")
  expect_equal(apa_name_to_title("job.satisfaction"), "Job Satisfaction")
})

test_that("a lowercase acronym cannot be recovered, and that is documented", {
  # no signal that iq is an acronym; tbl-cap is the remedy
  expect_equal(apa_name_to_title("iq_score"), "Iq Score")
  # the known residual: mRNA splits like selfEsteem
  expect_equal(apa_name_to_title("mRNA"), "M RNA")
})

test_that("an acronym that collides with a minor word survives", {
  # The acronym guard exists for THIS case, not for GPA: substr(w,1,1) <- toupper()
  # never lowercases, so an acronym that starts uppercase is safe with or without
  # the guard. What the guard actually prevents is a real acronym being demoted
  # because it matches the minor-word list case-insensitively. OR is the odds
  # ratio, AS is anxiety sensitivity, AN is anorexia nervosa, SO is significant
  # other, VIA is the Values in Action inventory -- all plausible column names.
  expect_equal(apa_title_case("odds OR value"),   "Odds OR Value")
  expect_equal(apa_title_case("time AS measured"),"Time AS Measured")
  expect_equal(apa_title_case("history of AN diagnosis"), "History of AN Diagnosis")
  expect_equal(apa_title_case("presence of SO support"),  "Presence of SO Support")
  expect_equal(apa_title_case("score on the VIA inventory"), "Score on the VIA Inventory")

  expect_equal(apa_name_to_title("OR_value"),    "OR Value")
  expect_equal(apa_name_to_title("AS_score"),    "AS Score")
  expect_equal(apa_name_to_title("AN_diagnosis"),"AN Diagnosis")
  expect_equal(apa_name_to_title("SO_support"),  "SO Support")
  expect_equal(apa_name_to_title("VIA_score"),   "VIA Score")
})

test_that("no generated name ever ends in a period or keeps a separator", {
  # generated sweep rather than a handful of examples
  names <- c("sales","GPA","self_esteem","selfEsteem","Score_For_Test",
             "a","x1","score2test","BMI_change","time_to_event",
             "job.satisfaction","PTSD","selfReportedSES")
  for (nm in names) {
    out <- apa_name_to_title(nm)
    expect_false(grepl("\\.$", out), info = nm)
    expect_false(grepl("[_.]", out), info = nm)
    expect_false(grepl("^\\s|\\s$", out), info = nm)
  }
})

# ---------------------------------------------------------------------------
# Builder-level: the titles the six generating builders actually produce.
# These read the SPEC title (what Quarto and every emitter consume), not the
# console string, because the spec is the single place a lost distinction can
# no longer be recovered downstream.
# ---------------------------------------------------------------------------

spec_title_text <- function(x) as_plaintext(as_spec(x)$title)

test_that("each builder's generated title is APA-correct", {
  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  data(album); data(viagra); data(goggles)
  ttl <- spec_title_text

  expect_equal(ttl(apa.cor.table(attitude)),
               "Descriptive Statistics and Correlations")
  expect_equal(ttl(apa.reg.table(stats::lm(sales ~ adverts, data = album))),
               "Regression Predicting Sales")
  expect_equal(ttl(apa.aov.table(stats::lm(libido ~ dose, data = viagra))),
               "ANOVA Results for Libido")
  expect_equal(ttl(apa.1way.table(iv = dose, dv = libido, data = viagra)),
               "Descriptive Statistics for Libido by Dose")
  expect_equal(ttl(apa.2way.table(iv1 = gender, iv2 = alcohol,
                                  dv = attractiveness, data = goggles)),
               "Descriptive Statistics for Attractiveness in a 2(Gender) by 3(Alcohol) Design")
  expect_equal(ttl(apa.d.table(iv = dose, dv = libido, data = viagra)),
               "Means, Standard Deviations, and d-Values for Libido by Dose")
})

test_that("no generated title ends in a period", {
  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  data(viagra); data(goggles)
  ttl <- spec_title_text
  for (tb in list(apa.cor.table(attitude),
                  apa.1way.table(iv = dose, dv = libido, data = viagra),
                  apa.2way.table(iv1 = gender, iv2 = alcohol,
                                 dv = attractiveness, data = goggles),
                  apa.d.table(iv = dose, dv = libido, data = viagra))) {
    expect_false(grepl("\\.$", ttl(tb)))
  }
})

test_that("an acronym variable survives into the title", {
  d <- data.frame(GPA = rnorm(30), adverts = rnorm(30))
  ttl <- spec_title_text(apa.reg.table(stats::lm(GPA ~ adverts, data = d)))
  expect_match(ttl, "GPA", fixed = TRUE)
  expect_false(grepl("Gpa", ttl, fixed = TRUE))
})

test_that("the d in the d-value title is a lowercase ITALIC statistic symbol", {
  # A plaintext-only assertion cannot observe the italic flag: the spec-level
  # defect fixed on 2026-08-01 (21 upright p values) was invisible to exactly
  # that kind of check. Assert on the RUNS.
  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  data(viagra)
  ttl <- as_spec(apa.d.table(iv = dose, dv = libido, data = viagra))$title
  expect_false(is.character(ttl))   # a plain string cannot carry the italic flag
  runs <- unclass(ttl)
  d_runs <- Filter(function(r) identical(r$text, "d"), runs)
  expect_length(d_runs, 1L)
  expect_true(isTRUE(d_runs[[1]]$italic))
  # and no run may carry a capital D-as-symbol
  expect_false(any(vapply(runs, function(r) identical(r$text, "D"), logical(1))))
  # the flag must SURVIVE into each destination that can express italics
  expect_equal(rt_to_latex(ttl),
               "Means, Standard Deviations, and \\textit{d}-Values for Libido by Dose")
  expect_equal(rt_to_markdown(ttl),
               "Means, Standard Deviations, and *d*-Values for Libido by Dose")
  # title casing must never reach the symbol: no "D-Value" anywhere
  expect_false(grepl("D-Value", as_plaintext(ttl), fixed = TRUE))
})

test_that("the d-table says the SAME thing on the console, in LaTeX and in the spec", {
  # These three paths must carry ONE title. Assert it rather than trusting
  # it: they are built in three different places and can drift apart.
  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  data(viagra)
  tbl <- apa.d.table(iv = dose, dv = libido, data = viagra)
  plain <- as_plaintext(as_spec(tbl)$title)
  expect_equal(plain, "Means, Standard Deviations, and d-Values for Libido by Dose")
  expect_equal(trimws(tbl$table_title), plain)                       # console
  expect_equal(gsub("[$]", "", tbl$latex.table.title), plain)        # LaTeX
  expect_false(grepl("Confidence Intervals", plain, fixed = TRUE))
})

test_that("apa.aov.table carries ONE title, on the console and in the spec", {
  # Until 2026-08-02 a second sprintf overwrote the console title with a
  # "Fixed-Effects" variant for the LaTeX and spec paths only.
  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  data(viagra)
  tbl <- apa.aov.table(stats::lm(libido ~ dose, data = viagra))
  plain <- as_plaintext(as_spec(tbl)$title)
  expect_equal(plain, "ANOVA Results for Libido")
  expect_equal(trimws(tbl$table_title), plain)
  expect_equal(trimws(tbl$latex.table.title), plain)
  expect_false(grepl("Fixed-Effects", plain, fixed = TRUE))
})

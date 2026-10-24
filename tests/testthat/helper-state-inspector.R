# A test that changes R's contrasts setting and leaves it changed alters
# every later test's models: sum-to-zero contrasts left by one file once
# renamed the factor rows of a regression table in another. testthat reports
# any test that ends with the setting changed, as a warning.
if (exists("set_state_inspector", envir = asNamespace("testthat"))) {
     testthat::set_state_inspector(function() list(contrasts = getOption("contrasts")))
}

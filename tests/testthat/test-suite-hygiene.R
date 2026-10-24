test_that("no test or helper file sets the contrasts option outside a test", {
     # The state inspector (helper-state-inspector.R) checks each test. A
     # setting at the top of a file runs outside every test, so it would
     # reach every later file unseen. Inside a test or a function definition
     # it is checked there or scoped to the call.
     sets_contrasts <- function(e) {
          if (!is.call(e)) return(FALSE)
          head <- e[[1]]
          fn <- if (is.name(head)) as.character(head) else
               if (is.call(head) && identical(head[[1]], as.name("::"))) as.character(head[[3]]) else ""
          if (fn %in% c("test_that", "function")) return(FALSE)
          if (fn %in% c("options", "local_options", "with_options") && "contrasts" %in% names(e)) return(TRUE)
          any(vapply(as.list(e)[-1], function(x) !missing(x) && sets_contrasts(x), logical(1)))
     }
     files <- list.files(test_path(), pattern = "^(test|helper)-.*[.]R$", full.names = TRUE)
     offenders <- Filter(function(f) any(vapply(as.list(parse(f, keep.source = FALSE)), sets_contrasts, logical(1))),
                         files)
     expect_identical(basename(offenders), character(0))
})

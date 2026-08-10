# tests/testthat/test-table-number.R
# table.number semantics: the default is 1; 0 (or NA, or NULL) means the table
# carries no number anywhere; apa.save() numbers default-numbered tables by
# their position in the document but keeps numbers the user typed.

library(apaTables)

# helper: the text of a written .docx, read out of the document XML
docx_xml <- function(tables, ...) {
     f <- tempfile(fileext = ".docx")
     do.call(apa.save, c(list(f), tables))
     d <- file.path(tempdir(), paste0("unzip-", basename(f)))
     utils::unzip(f, exdir = d)
     xml <- paste(readLines(file.path(d, "word", "document.xml"), warn = FALSE),
                  collapse = "")
     unlink(f)
     unlink(d, recursive = TRUE)
     xml
}

# one table of each type, built with the default number
default_tables <- function() {
     old <- options(contrasts = c("contr.sum", "contr.poly"))
     on.exit(options(old), add = TRUE)
     lm1 <- lm(libido ~ dose, data = viagra)
     blk1 <- lm(sales ~ adverts + airplay, data = album)
     list(
          cor = apa.cor.table(attitude),
          reg = apa.reg.table(blk1),
          aov = apa.aov.table(lm1),
          oneway = apa.1way.table(iv = dose, dv = libido, data = viagra),
          twoway = apa.2way.table(iv1 = gender, iv2 = alcohol,
                                  dv = attractiveness, data = goggles),
          twoway_ci = apa.2way.table(iv1 = gender, iv2 = alcohol,
                                     dv = attractiveness, data = goggles,
                                     show.conf.interval = TRUE),
          d = apa.d.table(iv = dose, dv = libido, data = viagra))
}

test_that("every builder numbers a table 1 by default", {
     for (tb in default_tables()) {
          expect_equal(tb$table_number, 1L)
          expect_equal(tb$spec$number, 1L)
          expect_false(tb$table_number_explicit)
     }
})

test_that("every builder keeps an explicitly supplied number", {
     old <- options(contrasts = c("contr.sum", "contr.poly"))
     on.exit(options(old), add = TRUE)
     lm1 <- lm(libido ~ dose, data = viagra)
     blk1 <- lm(sales ~ adverts + airplay, data = album)
     tbls <- list(
          apa.cor.table(attitude, table.number = 5),
          apa.reg.table(blk1, table.number = 5),
          apa.aov.table(lm1, table.number = 5),
          apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 5),
          apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                         data = goggles, table.number = 5),
          apa.d.table(iv = dose, dv = libido, data = viagra, table.number = 5))
     for (tb in tbls) {
          expect_equal(tb$table_number, 5L)
          expect_equal(tb$spec$number, 5L)
          expect_true(tb$table_number_explicit)
     }
})

test_that("a number equal to the default still counts as explicitly supplied", {
     tb <- apa.cor.table(attitude, table.number = 1)
     expect_equal(tb$table_number, 1L)
     expect_true(tb$table_number_explicit)
})

test_that("0, NA and NULL all suppress the number", {
     for (n in list(0, NA, NULL)) {
          tb <- apa.cor.table(attitude, table.number = n)
          expect_true(is.na(tb$table_number))
          expect_true(is.na(tb$spec$number))
          expect_true(tb$table_number_explicit)
     }
})

test_that("a suppressed number prints no number line at the console", {
     old <- options(contrasts = c("contr.sum", "contr.poly"))
     on.exit(options(old), add = TRUE)
     lm1 <- lm(libido ~ dose, data = viagra)
     blk1 <- lm(sales ~ adverts + airplay, data = album)
     tbls <- list(
          apa.cor.table(attitude, table.number = 0),
          apa.reg.table(blk1, table.number = 0),
          apa.aov.table(lm1, table.number = 0),
          apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 0),
          apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                         data = goggles, table.number = 0),
          apa.d.table(iv = dose, dv = libido, data = viagra, table.number = 0))
     for (tb in tbls) {
          out <- capture.output(print(tb))
          expect_false(any(grepl("Table", out)))
     }
})

test_that("the default number prints as Table 1 at the console", {
     out <- capture.output(print(apa.cor.table(attitude)))
     expect_true(any(grepl("^Table 1", out)))
})

test_that("an explicit number prints as that number at the console", {
     out <- capture.output(print(apa.cor.table(attitude, table.number = 7)))
     expect_true(any(grepl("^Table 7", out)))
})

test_that("table.number rejects values that are not a single number", {
     expect_error(apa.cor.table(attitude, table.number = "1"), "single number|must be a number")
     expect_error(apa.cor.table(attitude, table.number = c(1, 2)), "single number")
})

test_that("apa.save numbers three default tables 1, 2, 3", {
     t1 <- apa.cor.table(attitude)
     t2 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     t3 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     xml <- docx_xml(list(t1, t2, t3))
     expect_true(grepl("Table 1", xml, fixed = TRUE))
     expect_true(grepl("Table 2", xml, fixed = TRUE))
     expect_true(grepl("Table 3", xml, fixed = TRUE))
     expect_false(grepl("Table 4", xml, fixed = TRUE))
})

test_that("apa.save keeps explicit numbers and fills in the default ones", {
     t_expl <- apa.cor.table(attitude, table.number = 4)
     t_def1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     t_def2 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     expect_equal(apa_document_numbers(list(t_expl, t_def1, t_def2)),
                  c(4L, 5L, 6L))
     expect_equal(apa_document_numbers(list(t_def1, t_expl, t_def2)),
                  c(1L, 4L, 5L))
     xml <- docx_xml(list(t_def1, t_expl, t_def2))
     expect_true(grepl("Table 1", xml, fixed = TRUE))
     expect_true(grepl("Table 4", xml, fixed = TRUE))
     expect_true(grepl("Table 5", xml, fixed = TRUE))
     expect_false(grepl("Table 2", xml, fixed = TRUE))
})

test_that("a suppressed table is skipped by the document numbering", {
     t_sup <- apa.cor.table(attitude, table.number = 0)
     t_def <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     expect_equal(apa_document_numbers(list(t_sup, t_def)), c(NA_integer_, 1L))
})

test_that("a suppressed number writes no Table line to the .docx", {
     xml <- docx_xml(list(apa.cor.table(attitude, table.number = 0)))
     expect_false(grepl("Table", xml, fixed = TRUE))
     # the title is still written, so this is not an empty document
     expect_true(grepl("Descriptive Statistics and Correlations", xml, fixed = TRUE))
})

test_that("a table object with no explicit-number flag keeps its own number", {
     # objects built by hand (or by an older version) have no flag: whatever
     # number they carry is respected.
     tb <- apa.cor.table(attitude, table.number = 9)
     tb$table_number_explicit <- NULL
     expect_equal(apa_document_numbers(list(tb, tb)), c(9L, 9L))
     def <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     expect_equal(apa_document_numbers(list(tb, def)), c(9L, 10L))
})

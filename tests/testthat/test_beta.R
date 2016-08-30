library(apaTables)
context("beta value calculation")
test_that("Test values in Field et al Regression Chapter", {
     expect_equal(reg_test1(),0.5108462)
})

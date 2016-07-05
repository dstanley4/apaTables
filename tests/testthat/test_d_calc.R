library(apaTables)
context("d value calculation")
test_that("Test positive negative d-values", {
     expect_equal(round(d_value_test1(),7),0.9434933)
     expect_equal(round(d_value_test2(),7),0.9434933)
})

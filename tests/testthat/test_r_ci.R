library(apaTables)
context("r calculation")
test_that("Test r calculation and CI", {
     expect_equal(cor_test1()$r,".83**")
     expect_equal(cor_test1()$CI,"[.66, .91]")
     expect_equal(cor_test1()$m,"64.63")
     expect_equal(cor_test1()$sd,"12.17")
})

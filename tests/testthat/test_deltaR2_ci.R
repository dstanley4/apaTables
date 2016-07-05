library(apaTables)
context("Delta R2 CI")
test_that("Test example Alf & Graf (1999) p. 73", {
     expect_equal(round(get_deltaR2_ci(R2_2=.45^2,R2_1=.433^2,n=1415,conf_level=.95)$LL,4),.0037)
     expect_equal(round(get_deltaR2_ci(R2_2=.45^2,R2_1=.433^2,n=1415,conf_level=.95)$UL,4),.0263)
     expect_equal(round(get_deltaR2_ci(R2_2=.45^2,R2_1=.433^2,n=1415,conf_level=.90)$LL,4),.0055)
     expect_equal(round(get_deltaR2_ci(R2_2=.45^2,R2_1=.433^2,n=1415,conf_level=.90)$UL,4),.0245)

})

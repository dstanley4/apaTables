library(apaTables)
context("Smithson (2001) partial eta squared CI")
test_that("get.ci.partial.eta.squared is correct ci: Smithson (2001)", {
     #main effect a
     expect_equal(round(get.ci.partial.eta.squared(F.value=6, df1=1, df2=42, conf.level=.90)$LL,4),.0117)
     expect_equal(round(get.ci.partial.eta.squared(F.value=6, df1=1, df2=42, conf.level=.90)$UL,4),.2801)

     #main effect b
     expect_equal(round(get.ci.partial.eta.squared(F.value=2.65, df1=6, df2=42, conf.level=.90)$LL,4),.0174)
     expect_equal(round(get.ci.partial.eta.squared(F.value=2.65, df1=6, df2=42, conf.level=.90)$UL,4),.3577)

     #interaction ab
     expect_equal(round(get.ci.partial.eta.squared(F.value=2.50, df1=6, df2=42, conf.level=.90)$LL,3),.009)
     expect_equal(round(get.ci.partial.eta.squared(F.value=2.50, df1=6, df2=42, conf.level=.90)$UL,4),.3455)

})


context("Fidler & Thompson (2001) partial eta squared CI")
test_that("get.ci.partial.eta.squared is correct ci: Smithson (2001)", {
     #main effect a
     expect_equal(round(get.ci.partial.eta.squared(F.value=1.5, df1=1, df2=16, conf.level=.90)$LL,4),0)
     expect_equal(round(get.ci.partial.eta.squared(F.value=1.5, df1=1, df2=16, conf.level=.90)$UL,4),.3167)

     #main effect b
     expect_equal(round(get.ci.partial.eta.squared(F.value=4.00, df1=3, df2=16, conf.level=.90)$LL,4),.0357)
     expect_equal(round(get.ci.partial.eta.squared(F.value=4.00, df1=3, df2=16, conf.level=.90)$UL,4),.5671)

     #interaction ab
     expect_equal(round(get.ci.partial.eta.squared(F.value=1.5, df1=3, df2=16, conf.level=.90)$LL,4),0)
     expect_equal(round(get.ci.partial.eta.squared(F.value=1.5, df1=3, df2=16, conf.level=.90)$UL,4),.3778)

})

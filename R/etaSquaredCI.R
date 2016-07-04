#' Calculates confidence interval for partial eta-squared in a fixed-effects ANOVA
#' @param F.value The F-value for the fixed-effect
#' @param df1 Degrees of freedom for the fixed-effect
#' @param df2 Degrees of freedom error
#' @param conf.level Confidence level (0 to 1). For partial eta-squared a confidence level of .90 is traditionally used rather than .95.
#' @return List with confidence interval values (LL and UL)
#' @examples
#' # Smithson (2001) p. 619
#' get.ci.partial.eta.squared(F.value=6.00, df1=1, df2=42, conf.level=.90)
#' get.ci.partial.eta.squared(F.value=2.65, df1=6, df2=42, conf.level=.90)
#' get.ci.partial.eta.squared(F.value=2.60, df1=6, df2=42, conf.level=.90)
#'
#' # Fidler & Thompson (2001) Fixed Effects 2x4 p. 594 (Table 6) / p. 596 (Table 8)
#' get.ci.partial.eta.squared(F.value=1.50, df1=1, df2=16, conf.level=.90)
#' get.ci.partial.eta.squared(F.value=4.00, df1=3, df2=16, conf.level=.90)
#' get.ci.partial.eta.squared(F.value=1.50, df1=3, df2=16, conf.level=.90)
#' @export
get.ci.partial.eta.squared <- function(F.value, df1, df2, conf.level=.90) {
     F_value <- F.value

     conf_level <- conf.level

     F_limits <- MBESS::conf.limits.ncf(F=F_value, df.1=df1, df.2=df2, conf.level=conf_level)
     LL_lambda <- F_limits$Lower.Limit
     UL_lambda <- F_limits$Upper.Limit


     LL_partial_eta2 <- get_partial_eta2_from_lambda(lambda=LL_lambda, df1=df1, df2=df2)
     UL_partial_eta2 <- get_partial_eta2_from_lambda(lambda=UL_lambda, df1=df1, df2=df2)


     if (is.na(LL_partial_eta2)) {
          LL_partial_eta2 <- 0
     }

     if (is.na(UL_partial_eta2)) {
          UL_partial_eta2 <- 1
     }

     output <- list()
     output$LL <- LL_partial_eta2
     output$UL <- UL_partial_eta2
     return(output)
}


get_partial_eta2_from_lambda <- function(lambda, df1, df2)  {
     partial_eta2 <- lambda / (lambda + df1 + df2 + 1)
     return(partial_eta2)
}



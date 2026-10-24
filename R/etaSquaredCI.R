#' Calculates confidence interval for partial eta-squared in a fixed-effects ANOVA
#'
#' \code{get.ci.partial.eta.squared()} calculates the confidence interval for
#' partial eta-squared from an F-value and the degrees of freedom for it, for an
#' effect in a fixed-effects ANOVA. \code{\link{apa.aov.table}} uses it for
#' the intervals in the ANOVA table.
#'
#' @param F.value The F-value for the fixed effect.
#' @param df1 Degrees of freedom for the fixed effect.
#' @param df2 Degrees of freedom for error.
#' @param conf.level Confidence level (0 to 1). For partial eta-squared a
#'   confidence level of .90 is traditionally used rather than .95.
#' @details
#' The interval is found for the noncentrality parameter (lambda) of the F
#' distribution with \code{MBESS::conf.limits.ncf()}, and each limit is
#' converted to partial eta-squared as lambda / (lambda + df1 + df2 + 1),
#' the approach described by Smithson (2001) and Fidler and Thompson (2001).
#' A lower limit that cannot be found is set to 0, and an upper limit to 1.
#' @return A list with the lower (\code{LL}) and upper (\code{UL}) limits of
#'   the interval.
#' @references
#' Fidler, F., & Thompson, B. (2001). Computing correct confidence intervals
#' for ANOVA fixed- and random-effects effect sizes. Educational and
#' Psychological Measurement, 61(4), 575-604.
#'
#' Smithson, M. (2001). Correct confidence intervals for various regression
#' effect sizes and parameters: The importance of noncentral distributions
#' in computing intervals. Educational and Psychological Measurement, 61(4),
#' 605-632.
#' @examples
#' # Smithson (2001) p. 619, with the published 90% intervals
#' # row effect: published [.0117, .2801]
#' get.ci.partial.eta.squared(F.value = 6.00, df1 = 1, df2 = 42, conf.level = .90)
#' # column effect: published [.0174, .3577]
#' get.ci.partial.eta.squared(F.value = 2.65, df1 = 6, df2 = 42, conf.level = .90)
#' # interaction: published [.0093, .3455]; this gives [.0094, .3455], the
#' # difference being rounding in the fourth decimal
#' get.ci.partial.eta.squared(F.value = 2.50, df1 = 6, df2 = 42, conf.level = .90)
#'
#' # Fidler & Thompson (2001) Fixed Effects 2x4 p. 594 (Table 6) / p. 596 (Table 8)
#' get.ci.partial.eta.squared(F.value = 1.50, df1 = 1, df2 = 16, conf.level = .90)
#' get.ci.partial.eta.squared(F.value = 4.00, df1 = 3, df2 = 16, conf.level = .90)
#' get.ci.partial.eta.squared(F.value = 1.50, df1 = 3, df2 = 16, conf.level = .90)
#' @export
get.ci.partial.eta.squared <- function(F.value, df1, df2, conf.level=.90) {
     F_value <- F.value

     conf_level <- conf.level

     # Defensive: MBESS::conf.limits.nct() runs options(warn = -1) in its
     # own scope and never restores it, which silences every warning for
     # the rest of the session (or the rest of a Quarto render). As of
     # MBESS 5.0.0 conf.limits.ncf() is a bisection over pf() that never
     # calls nct() and leaves the option alone -- but DESCRIPTION pins no
     # MBESS version and this is not part of MBESS's documented
     # interface, so guard here too. Restore the caller's setting when
     # this function exits, including on error, and again right after the
     # call so later warnings here still show.
     old_warn_option <- getOption("warn")
     on.exit(options(warn = old_warn_option), add = TRUE)
     F_limits <- MBESS::conf.limits.ncf(F=F_value, df.1=df1, df.2=df2, conf.level=conf_level)
     options(warn = old_warn_option)
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



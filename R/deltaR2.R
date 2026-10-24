
get_sr2_ci <- function(sr2, R2, n, conf_level) {
     R2_2 <- R2
     R2_1 <- R2-sr2
     ci <- get_deltaR2_ci(R2_2 = R2_2,R2_1=R2_1,n=n, conf_level = conf_level)
     return(ci)
}


get_deltaR2_ci <- function(R2_2,R2_1,n,conf_level=.95){
     #Alf Jr, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for the difference between two squared multiple correlations: A simplified approach. Psychological Methods, 4(1), 70.
     #Case 2 from paper

     z <- qnorm((1-((1-conf_level)/2)))

     r20A <- R2_2
     r20B <- R2_1

     r0A  <- sqrt(r20A)
     r0B  <- sqrt(r20B)

     rAB  <- r0B/r0A
     r2AB <- rAB^2

     var_delta_r2 <- (4*r20A*(1-r20A)^2)/n + (4*r20B*(1-r20B)^2)/n - 8*r0A*r0B*(.5*(2*rAB-r0A*r0B)*(1-r20A - r20B - r2AB)+rAB^3)/n

     LL <- (r20A -r20B) - z*sqrt(var_delta_r2)
     UL <- (r20A -r20B) + z*sqrt(var_delta_r2)

     # A proportion of variance lies between 0 and 1, but this asymptotic
     # interval is symmetric and can reach past either end; the limits are
     # held to [0, 1], as the noncentral-F interval for R2 already is.
     LL <- max(0, LL)
     UL <- min(1, UL)

     output <- list()
     output$LL <- LL
     output$UL <- UL
     return(output)
}


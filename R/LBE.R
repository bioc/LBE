`LBE` <-
function (pval, a = NA, l = 0.05, ci.level = 0.95, qvalues = TRUE,
    plot.type = "main", FDR.level = 0.05, n.significant = NA)
{
    if (min(pval) < 0 | max(pval) > 1) {
        print("ERROR: p-values not in valid range.")
        return(0)
    }
    else {
        m <- length(pval)
        FDR <- NA
        if (is.na(a) == FALSE & a < 1) {
            a <- NA
            sdbound <- sqrt(1/(3 * m))
            pi0 <- min(1, mean(pval) * 2)
            icpi0 <- c(0, pi0 - qnorm((1 - ci.level), 0, sdbound))
        }
        else {
            if (is.na(a)) {
                a <- LBEa(m,l,fig=FALSE)
            }
            sdbound <- sqrt((1/(gamma(a + 1))^2) * ((gamma(2 *
                a + 1) - (gamma(a + 1))^2)/m))
            pi0 <- min(1, mean((-log(1 - pval))^a)/gamma(a +
                1))
            icpi0 <- c(0, min(1, pi0 - qnorm((1 - ci.level),
                0, sdbound)))
        }
        mat <- NA
        if (qvalues == TRUE) {
            qval <- rep(NA, m)
            sort.pval <- sort(pval)
            order.pval <- order(pval)
            rank.pval <- rank(pval)
            qval[m] <- (pi0 * m * sort.pval[m])/m
            for (i in 1:(m - 1)) {
                qval[m - i] <- min((pi0 * m * sort.pval[m - i])/(m -
                  i), qval[m - i + 1])
            }
            mat <- cbind(rank.pval, qval, sort.pval)
           
            if (is.na(n.significant) == FALSE) {
                FDR.level <- mat[n.significant, 2]
                FDR <- FDR.level
            }
            else {
                n.significant <- length(mat[mat[, 2] <= FDR.level,
                  2])
                FDR <- max(mat[mat[, 2] <= FDR.level, 2],0)
            }
        }
    
        if (sdbound>0.5){
            print(paste("WARNING: l = ",sdbound,". A smaller value is recommended for a (or l).",sep=""))
        }
        
        if (qvalues == TRUE) {
          significant<-(qval[rank.pval] <= FDR.level)
          r <- list(call = match.call(), FDR = FDR, pi0 = pi0, pi0.ci = icpi0, ci.level = ci.level, a = a, l = sdbound, qvalues = qval[rank.pval], pvalues = pval, significant = significant, n.significant = length(significant[significant==TRUE]))
          class(r) <- "LBE"
          if(plot.type!="none"&qvalues==TRUE){
            LBEplot(r,plot.type=plot.type)
          }
          invisible(r)
        }
        else {
          r <- list(call = match.call(), FDR = NA, pi0 = pi0, pi0.ci = icpi0, ci.level = ci.level, a = a, l = sdbound, qvalues = NA, pvalues = pvalues, significant = NA, n.significant = NA)
          class(r) <- "LBE"
          invisible(r)
        }
    }
}


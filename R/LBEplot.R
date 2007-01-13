`LBEplot` <-
function (LBEobj, rng = c(0, 0.1), plot.type = c("multiple","main"),legend=TRUE){
    if (class(LBEobj)=="LBE" ){    
      if (is.na(LBEobj$qvalues[1])) {
        print("ERROR: Estimated q-values not found. Apply the function LBE with the option: qvalues=TRUE.")
      }     
      else {
        q2 <- LBEobj$qval[order(LBEobj$pval)]
        if (min(q2) > rng[2]) {
            rng <- c(min(q2), quantile(q2, 0.1))
        }
        p2 <- LBEobj$pval[order(LBEobj$pval)]
        m <- length(q2)
        if (plot.type[1] == "main"){
          nf <- layout(cbind(rep(0, 12), matrix(c(rep(1,81), rep(2, 27)), ncol = 9, byrow = TRUE),rep(0, 12)), respect = FALSE)
          plot(p2, q2, xlab = "p-values", ylab = "q-values",xlim = c(0, 1), ylim = c(0, 1), col = "blue",pch = 16)
          par(new = TRUE)
          plot(p2[q2 <= LBEobj$FDR], q2[q2 <= LBEobj$FDR], xlab = "p-values", ylab = "q-values",xlim = c(0, 1), ylim = c(0, 1), col = "orange", pch = 16)
          if (legend == TRUE) {
            n.select<-length(LBEobj$significant[LBEobj$significant==TRUE])
            legend(1, 0, c(paste("FDR =", round(LBEobj$FDR, digits = 5)), paste(m - n.select, "non rejected null hypotheses"), paste(n.select, "rejected null hypotheses")), pch = rep(16, 3), col = c("white", "blue", "orange"), xjust = 1, yjust = 0, cex = 1.5, bty = "n")
          }
          hist(p2, xlim = c(0, 1), nclass = 100, ann = FALSE, xlab = "p-values", xaxt = "n", yaxt = "n", main = "")
        }
        if (plot.type[1] == "multiple"){
          par(mfrow=c(2,2))
          hist(p2,xlab = "p-value", ylab = "frequency",main="",col="blue")
          plot(p2[q2 >= rng[1] & q2 <= rng[2]], q2[q2 >= rng[1] & q2 <=
           rng[2]], type = "l", xlab = "p-value", ylab = "q-value",col="blue",lwd=2)
          plot(q2[q2 >= rng[1] & q2 <= rng[2]], (1 + sum(q2 < rng[1])):sum(q2 <=
            rng[2]), type = "l", xlab = "q-value cut-off", ylab = "significant tests",col="blue",lwd=2)
          plot((1 + sum(q2 < rng[1])):sum(q2 <= rng[2]), q2[q2 >= rng[1] &
            q2 <= rng[2]] * (1 + sum(q2 < rng[1])):sum(q2 <= rng[2]),
            type = "l", xlab = "significant tests", ylab = "expected false positives",col="blue",lwd=2)
        } 
      }
    }
    else {
            print("ERROR: An LBE object is required as input.")
    }            
}


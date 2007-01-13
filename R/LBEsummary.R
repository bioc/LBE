`LBEsummary` <-
function (LBEobj, cuts = c(1e-04, 0.001, 0.01, 0.025, 0.05, 0.1,1), digits = getOption("digits"), ...){
    cat("\nCall:\n", deparse(LBEobj$call), "\n\n", sep = "")
    cat("pi0:", format(LBEobj$pi0, digits = digits), "\n", sep = "\t")
    cat(paste("Confidence Interval (level=",LBEobj$ci.level,"): [",format(LBEobj$pi0.ci[1], digits = digits),",",format(LBEobj$pi0.ci[2],digits=digits),"]",sep=""),"\n")
    cat("\n")
    cat("Cumulative number of significant calls:\n")
    cat("\n")
    counts<-sapply(cuts, function(x) c("p-value" = sum(LBEobj$pvalues<x), "q-value" = sum(LBEobj$qvalues < x)))
    colnames(counts)<-paste("<", cuts, sep = "")
    print(counts)
    cat("\n")
    invisible(LBEobj)
}


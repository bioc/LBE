`LBEwrite` <-
function (LBEobj, filename = "LBE-results.txt"){
    cat(c("pi0:", LBEobj$pi0, "\n\n"), file = filename, append = FALSE)
    cat(c("ic.pi0:", LBEobj$ci.pi0, "\n\n"), file = filename, append = FALSE)   
    cat(c("FDR:", LBEobj$FDR, "\n\n"), file = filename, append = TRUE)
    cat(c("p-value q-value significant", "\n"), file = filename, append = TRUE)
    write(t(cbind(LBEobj$pval, LBEobj$qval, LBEobj$significant)), file = filename, ncolumns=3, sep="\t", append = TRUE)
}


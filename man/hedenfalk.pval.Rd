\name{hedenfalk.pval}
\alias{hedenfalk.pval}
\docType{data}
\title{ p-values corresponding to the gene expression data from Hedenfalk et al. (2001). }
\description{
  The aim of the study of Hedenfalk et al. (2001) was to examine breast-cancer tissues from patients with BRCA1-BRCA2 related cancer and cases of sporadic breast cancer to determine global gene expression patterns in the different classes of tumours. Here, we focus on the comparison of BRCA1 and BRCA2. The p-values provided here are the same as those provided with the package {qvalue}. They were otained from a two-sample t-test analysis on a subset of 3,170 genes, as described in Storey and Tibshirani (2003).
}
\usage{data(hedenfalk.pval)}
\format{
  The format is:
 num [1:3170] 0.0121 0.0750 0.9949 0.0418 0.8458 ...
}

\references{
  Hedenfalk I, Duggan D, Chen Y, Radmacher M, Bittner M, Simon R, Meltzer P, Guterson B, Esteller M, Kallioniemi OP et al. (2001) Gene-expression profiles in hereditary breast cancer. N Engl J Med, 22, 539-548.
  
  Storey JD and Tibshirani R. (2003). Statistical significance for genome-wide studies. Proc Natl Acad Sci, 100, 9440-9445.
}
\keyword{datasets}   
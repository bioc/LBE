\name{golub.pval}
\alias{golub.pval}
\docType{data}
\title{ p-values corresponding to the gene expression data from Golub et al. (1999). }
\description{
  The aim of the study of Golub et al. \cite{Golub} was to identify differentially expressed genes between acute myeloid leukemia (AML) and acute lymphoblastic leukemia (ALL). 
  The samples were assayed using Affymetrix Hgu6800 chips and the data on the expression of 7129 genes are available in the Bioconductor package {golubEsets}.
  The p-values provided here were calculated from a two-sample t-test analysis. The variance-stabilizing method included in the {vsn} package was applied for normalizing the data.
}
\usage{data(golub.pval)}
\format{
  The format is:
 num [1:3051] 0.0170 0.2552 0.9130 0.7867 0.2431 ...
}
\references{
Golub TR, Slonim DK, Tamayo P, Huard C, Gaasenbeek M, Mesirov JP, Coller H, Loh ML, Downing JR, Caligiuri MA, Bloomfield CD, Lander ES (1999). Molecular classification of cancer: class discovery and class prediction by gene expression monitoring. Science, 15, 286(5439), 531-7.
}
\keyword{datasets}
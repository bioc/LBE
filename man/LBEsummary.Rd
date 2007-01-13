\name{LBEsummary}
\alias{LBEsummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Display LBE object }
\description{
  Display summary information for an LBE object.
}
\usage{
LBEsummary(LBEobj, cuts = c(1e-04, 0.001, 0.01, 0.025, 0.05, 0.1, 1), digits = getOption("digits"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LBEobj}{ LBE object. }
  \item{cuts}{ Vector of significance value to use for table. }
  \item{digits}{ Significant digits to display. }
  \item{\dots}{ Any other arguments. }
}
\details{
  LBEsumary shows the original call, estimated proportion of true null hypotheses, confidence interval for the proportion of true null hypotheses and a table comparing the number of significant calls for the p-values and for the estimated q-values using a set of cutoffs given by the argument cuts.
}
\value{
Invisibly returns the original object.
}
\author{ Cyril Dalmasso }
\note{ 
  The function LBEsummary is analogous to the function qsummary from the package {qvalue}.
}
\seealso{ \code{\link{LBE}}, \code{\link{LBEplot}}, \code{\link{LBEwrite}}, \code{\link{LBEa}} }
\examples{
## start
data(hedenfalk.pval)
res<-LBE(hedenfalk.pval)
print(res)
LBEsummary(res)
## end
}

\keyword{ htest }

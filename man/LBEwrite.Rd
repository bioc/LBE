\name{LBEwrite}
\alias{LBEwrite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Write the results of an LBE object to a file. }
\description{
  Write the results of an LBE object to a file.
}
\usage{
LBEwrite(LBEobj, filename = "LBE-results.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LBEobj}{ LBE object }
  \item{filename}{ Output filename (optional) }
}
\details{
  The output file lists the estimate of pi0, which is the proportion of true null hypotheses. It also lists each p-value and corresponding q-value, one per line. If an FDR significance level was specified in the call to qvalue, the significance level is printed below the estimate of pi0, and an indicator of significance is included as a third column for each p-value and q-value.  
}

\author{ Cyril Dalmasso }
\note{ 
  The function LBEwrite is analogous to the function qwrite from the package {qvalue}.
}
\seealso{ \code{\link{LBE}}, \code{\link{LBEplot}}, \code{\link{LBEsummary}}, \code{\link{LBEa}} }
\examples{
## start
data(hedenfalk.pval)
res<-LBE(hedenfalk.pval)
LBEwrite(res,filename="myresults.txt")
## end
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

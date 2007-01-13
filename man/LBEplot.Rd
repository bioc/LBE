\name{LBEplot}
\alias{LBEplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Graphical display of LBE objects. }
\description{
  Graphical display of LBE objects. 
}
\usage{
LBEplot(LBEobj, rng = c(0, 0.1), plot.type = c("multiple", "main"), legend = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LBEobj}{ LBE object. }
  \item{rng}{ Range of q-values to consider.  }
  \item{plot.type}{  
  If plot.type=="main", The estimated q-values versus the p-values are plotted together with the histogram of the p-values.  
  If plot.type=="multiple" (default value), several graphics are displayed.  
  }
  \item{legend}{ Logical value for displaying the legend on the plot (when plot.type=="main").}
}
\details{
  If plot.type=="multiple", the following graphics are displayed:
  1. The histogram of the p-values. 
  2. The estimated q-values versus the p-values. 
  3. The number of significant tests versus each qvalue cutoff. 
  4. The number of expected false positives versus the number of significant tests.
}
\author{ Cyril Dalmasso }
\note{ 
The function LBEplot is analogous to the function qplot from the package {qvalue}.
}
\seealso{ \code{\link{LBE}}, \code{\link{LBEsummary}}, \code{\link{LBEwrite}}, \code{\link{LBEa}} }
\examples{
## start
data(hedenfalk.pval)
res=LBE(hedenfalk.pval,plot.type="none")
LBEplot(res)
LBEplot(res,plot.type="main")
LBEplot(res,plot.type="main",legend=FALSE)
## end
}
\keyword{ htest }

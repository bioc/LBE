\name{LBE}
\alias{LBE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Estimation of the false discovery rate. }
\description{
LBE is an efficient procedure for estimating the proportion of true null hypotheses, the false discovery rate and the q-values.  
}
\usage{
LBE(pval, a = NA, l = 0.05, ci.level = 0.95, qvalues = TRUE, plot.type = "main", FDR.level = 0.05, n.significant = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pval}{ Numerical vector of p-values (only necessary input). }
  \item{a}{ Real value used in \eqn{[-ln (1-pi)]^a}  (see details).
  If a == NA (default), then the value of a is automatically calculated as the greatest value such that the upper bound of the asymptotic standard deviation of the estimator of pi0 is smaller than the threshold l. 
  If \eqn{a >= 1}, the value of a is used in \eqn{[-ln (1-pi)]^a} (see
  details). 
  If \eqn{a < 1}, the identity function is used for transforming the
  p-values. } 
  \item{l}{ Threshold for the upper bound of the asymptotic standard deviation
  (only used if a == NA). }
  \item{ci.level}{ Level for the confidence interval of pi0.  }
  \item{qvalues}{ Logical value for estimating the qvalues and the FDR. If
    qvalues = FALSE, only the proportion pi0 of true null hypotheses is
    estimated. } 
  \item{plot.type}{ 
  If plot.type = "none", no graphic is displayed.
  If plot.type = "main", the estimated q-values versus the p-values are
  plotted together with the histogram of the p-values.   
  If plot.type = "multiple", several graphics are displayed: 1. The histogram
  of the p-values 2. The estimated q-values versus the p-values 3. The number
  of significant tests versus each qvalue cutoff 4. The number of expected
  false positives versus the number of significant tests.  } 
  \item{FDR.level}{ Level at which to control the FDR (only used if
  n.significant == NA). }
  \item{n.significant}{ If specified, the FDR is estimated for the rejection
  region defined by the "n.significant" smallest p-values. } 
}
\details{
  The procedure LBE is based on the expectation of a particular transformation
  of the p-values leading to a straightforward estimation of the key quantity
  pi0 that is the proportion of true null hypotheses: 
  \eqn{pi0(a)=\{(1/m)*\sum_{i=1}^m[-\ln(1-pi)]^a\}/\Gamma(a+1),}
where a belongs to the interval \eqn{[1;inf)}.    
}
\value{
  A list containing:
  \item{ call }{Function call.}
  \item{ FDR }{Level at which to control the FDR (if n.significant == NA) or
    estimated FDR (if n.significant != NA).}
  \item{ pi0 }{Estimated value of pi0, the proportion of true null
    hypotheses.} 
  \item{ pi0.ci }{Confidence interval for pi0.}
  \item{ ci.level }{Level for the confidence interval of pi0.}
  \item{ a }{Value used in \eqn{[-\ln (1-pi)]^a} (see details).}
  \item{ l }{Upper bound of the asymptotic standard deviation for pi0.}
  \item{ qvalues }{Vector of the estimated q-values.}
  \item{ pvalues }{Vector of the original p-values.}
  \item{ significant }{Indicator of wether the null hypothesis is rejected.}
  \item{ n.significant }{Number of rejected null hypotheses.}
}
\references{ 
Dalmasso C, Broet P, Moreau T (2005). A simple procedure for estimating the
false discovery rate. Bioinformatics. Bioinformatics, 21: 660 - 668.  

Storey JD and Tibshirani R. (2003). Statistical significance for genome-wide
studies. Proc Natl Acad Sci, 100, 9440-9445. 
}
\author{ Cyril Dalmasso }
\note{ 
LBE is an alternative method to the one proposed by Storey and Tibshirani
(2003) for estimating the q-values, this latter method being implemented in
the package \code{qvalue}. 
}
\seealso{ \code{\link{LBEplot}}, \code{\link{LBEsummary}}, \code{\link{LBEwrite}}, \code{\link{LBEa}} }
\examples{
## start
data(hedenfalk.pval)
res=LBE(hedenfalk.pval)
data(golub.pval)
res=LBE(golub.pval)
## end
}
\keyword{ htest }




\name{LBEa}
\alias{LBEa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Optimal setting for the parameter a }
\description{
  The LBEa function is called by the main function LBE for choosing the greatest value of a such that the upper bound of the asymptotic standard deviation is less than a threshold l. A plot illustrating the relation between a and l for a fixed number of tested hypotheses can also be displayed.

}
\usage{
LBEa(m, l = 0.05, fig = TRUE, a.rng = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{ Total number of tested hypotheses. }
  \item{l}{ Threshold for the upper bound of the asymptotic standard deviation (default value is 0.05). }
  \item{fig}{ Logical value for plotting the standard deviation versus a. }
  \item{a.rng}{ Range of values of a to consider. If a.rng==NA, a.rng is set such that the standard deviation is less than 0.5. }
}
\details{
The procedure LBE is based on the expectation of a particular transformation of the p-values leading to a straightforward estimation of the key quantity pi0 that is the proportion of true null hypotheses:
  $ pi0(a)={(1/m)*sum_{i=1}^{m}[-ln(1-pi)]^a]}/Gamma(a+1) $
where a belongs to the interval [1;inf[.   
As there is a balance between bias (decreasing as a increase) and variance (increasing as a increase), for a specified number m of tested hypotheses, we have proposed to choose the greatest value of a such that the upper bound of the asymptotic standard deviation of the estimator of pi0 is smaller than the threshold l.
The function LBEa allows to plot the standard deviation versus a in order to help for the choice of the threshold l (for a specific number m of tested hypotheses). 
}
\value{
  \item{a }{ Greatest value of a such that the the upper bound of the asymptotic standard deviation of the estimator of pi0 is smaller than the threshold l. }
}
\references{ 
Dalmasso C, Broet P, Moreau T (2005). A simple procedure for estimating the false discovery rate. Bioinformatics. Bioinformatics, 21: 660 - 668. 
}
\author{ Cyril Dalmasso }

\seealso{ \code{\link{LBE}}, \code{\link{LBEplot}}, \code{\link{LBEsummary}}, \code{\link{LBEwrite}} }
\examples{
## start
data(hedenfalk.pval)
m <- length(hedenfalk.pval)
LBEa(m,l=0.05)
## end
}
\keyword{ misc }


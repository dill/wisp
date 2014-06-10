\name{sim.cir.2}
\alias{sim.cir.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate 2-occasion CIR multiple times}
\description{
 Perform 2-occasion change-in-ratio (CIR) according to users' specifications, and return a data frame
 with point estimate and measures of precision for each simulated experiment
}
\usage{
sim.cir.2(popn, frac.prehunt, frac.posthunt, frac.harv.male, frac.harv.fem, B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{popn}{ A WiSP population object, perhaps created by \code{\link{make.twosex.pop}} }
  \item{frac.prehunt}{ Proportion [0,1] of the population sampled prior to harvest season to estimate sex composition }
  \item{frac.posthunt}{ Proportion [0,1] of the population sampled following the harvest season to estimate sex composition }
  \item{frac.harv.male}{ Proportion [0,1] of the males harvested during the hunting season }
  \item{frac.harv.fem}{ Proportion [0,1] of the females harvested during the hunting season }
  \item{B}{ Number of simulated replications to perform }
}
\details{
  The population (popn) remains unchanged for all simulations; i.e., the same population is used for all replicates.  Hence
  none of the stochasticity in the results is attributable to changing populations.
}
\value{
  A list comprised of a recapitulation of the call to the function followed by a data frame
  \item{[[1]] }{First item in the list is the call to the function, explicitly keeping the way the function was used
  with the result of the function.  Used by \code{draw.hist2} to label the resulting histogram for future reference.}
  \item{$point.cir }{Point estimate of abundance for each replicate,}
  \item{$se.cir }{Standard error of abundance for each replicate,}
  \item{$coef.var }{Coefficient of variation (not percentage) of abundance for each replicate.}
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ The number
of replicates requested may not be equal to the number of replicates realized because of the presence of missing values.
Consider \code{\link{draw.hist2}} to portray results of a call to this function.}

\section{Warning }{The nature of the value returned by the function, specifically the call as the first element of the list,
subsequent use of the result can be simplified if after the function is called, a new data frame is manufactured from the result
as \code{result.frame <- data.frame(example.result[[2]])}.  Also note that because negative estimates are set equal to \code{NaN},
 any subsequent calculations should be preceeded by checks for \code{is.finite}.}

\seealso{ \code{\link{two.samp.cir}}, \code{\link{sim.cir.2}}, \code{\link{make.twosex.pop}}, \code{\link{draw.hist2}} }
\examples{
library(wisp)
example <- make.twosex.pop(abund=200, prop.male=0.4)
example.result <- sim.cir.2(popn=example, frac.prehunt=0.2, frac.posthunt=0.4,
                  frac.harv.male=0.6, frac.harv.fem=0.02, B=200)
example.result
draw.hist2(df=example.result, my.max=1000)

}
\keyword{ htest }% at least one, from doc/KEYWORDS

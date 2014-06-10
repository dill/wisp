\name{sim.cir.2.summary}
\alias{sim.cir.2.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate 2-occasion CIR multiple times and return a summary}
\description{
 Perform 2-occasion change-in-ratio (CIR) according to users' specifications, and return mean
 metrics from the simulation
}
\usage{
sim.cir.2.summary(popn, frac.prehunt, frac.posthunt, frac.harv.male, frac.harv.fem, B)
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
  A data frame is returned consisting of the following summary statistics:
  \item{$point.mean }{Mean of the B point estimates generated (after ignoring missing values)}
  \item{$se.mean }{Standard error of the B point estimates generated (after ignoring missing values)}
  \item{$cv.mean }{Coefficient of variation (not percentage) of the B point estimates generated (after ignoring missing values)}
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ Consult \code{\link{sim.cir.2}} for the manner in which missing values are generated. The number
of replicates requested may not be equal to the number of replicates realized because of the
presence of missing values. The utility of
this function is to concatenate results from multiple sets of simulations into a form for plotting.}

\seealso{ \code{\link{two.samp.cir}}, \code{\link{sim.cir.2}}, \code{\link{make.twosex.pop}} }
\examples{

library(wisp)
example <- make.twosex.pop(abund=200, prop.male=0.4)
example.result <- sim.cir.2.summary(popn=example, frac.prehunt=0.2, frac.posthunt=0.4,
                  frac.harv.male=0.6, frac.harv.fem=0.02, B=200)
example.result
}
\keyword{ htest }% at least one, from doc/KEYWORDS


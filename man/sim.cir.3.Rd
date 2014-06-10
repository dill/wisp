\name{sim.cir.3}
\alias{sim.cir.3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate 3-occasion CIR multiple times}
\description{
 Perform 3-occasion change-in-ratio (CIR) according to users' specifications, and return a data frame
 with point estimate and measures of precision for each simulated experiment
}
\usage{
sim.cir.3(popn, frac.prehunt, frac.midhunt, frac.posthunt,
          frac.harv.male.1, frac.harv.fem.1, frac.harv.male.2, frac.harv.fem.2, B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rm.pop}{ Population object being harvested and sampled (likely created by \code{\link{make.twosex.pop}}) }
  \item{frac.prehunt}{ Proportion of the population sampled prior to the  first harvest season}
  \item{frac.midhunt}{ Proportion of the population sampled between the harvest seasons}
  \item{frac.posthunt}{ Proportion of the population sampled following the second harvest season}
  \item{frac.harv.male.1}{ Proportion of males in the population removed during first harvest }
  \item{frac.harv.fem.1}{ Proportion of females in the population removed during first harvest }
  \item{frac.harv.male.2}{ Proportion of males in the population removed during second harvest }
  \item{frac.harv.fem.2}{ Proportion of females in the population removed during second harvest }
  \item{B}{ Number of simulated replicates of the experiment desired }
}
\details{
  The population (popn) remains unchanged for all simulations; i.e., the same population is used for all replicates.  Hence
  none of the stochasticity in the results is attributable to changing populations.  This function simply makes repeated
  calls to \code{\link{three.samp.cir}}.
}
\value{
A data frame consisting of
  \item{$point.cir }{Point estimate of abundance for each replicate,}
  \item{$se.cir }{Standard error of abundance for each replicate,}
  \item{$coef.var }{Coefficient of variation (not percentage) of abundance for each replicate.}
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ The number
of replicates requested may not be equal to the number of replicates realized because of the presence of missing values.}

\section{Warning }{Exercise caution using the results of the function because inadmissable values are returned as \code{NaN},
so checks on finiteness is called for before summarizing/plotting results.}

\seealso{  \code{\link{three.samp.cir}}, \code{\link{sim.cir.2}}, \code{\link{make.twosex.pop}} }
\examples{
library(wisp)
example <- make.twosex.pop(abund=200, prop.male=0.4)
example.result <- sim.cir.3(popn=example,frac.prehunt=0.4, frac.midhunt=0.4, frac.posthunt=0.4,
                            frac.harv.male.1=0.3, frac.harv.fem.1=0.05,
                            frac.harv.male.2=0.4, frac.harv.fem.2=0.05, B=200)

example.result
}
\keyword{ htest }% __ONLY ONE__ keyword per line

\name{three.samp.cir}
\alias{three.samp.cir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Analytical estimate of abundance from a 3-occasion, 2-type change-in-ratio sampling design }
\description{
  Performs sex-specific harvest of animals from a simulated population, and
  produces analytical estimates of abundance and standard error for three-occasion
  change-in-ratio (CIR) estimator.
}
\usage{
three.samp.cir(rm.pop, frac.prehunt, frac.midhunt, frac.posthunt,
               frac.harv.male.1, frac.harv.fem.1, frac.harv.male.2, frac.harv.fem.2)
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
}
\details{
  An extension of the two-occasion CIR technique, in which the sampling design possess two harvest
  periods.  Assessment of the sex ratio of the population is made three times: before, between, and following the
  harvest seasons.  Sex-specific harvest regulations are presumed to occur during each harvest season.
}
\value{
  A list is returned, consisting of
  \item{point.cir }{Estimated point estimate of abundance}
  \item{se.cir }{Standard error of abundance estimate}
  \item{coef.var }{Coefficient of variation of estimate (not percentage)}
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ Specifically, eqn. xxx of Williams et al. (2002) is used  for the point estimate, and xxx for the variance estimate.}

\section{Warning }{It is not impossible for the point estimate of abundance to be negative; particularly when the difference
in harvest between males and females is small.  This is a nonsensical result, and hence, the estimate is not considered
admissable.  In this situation, all values returned by this function are set equal to \code{NA}.}

\seealso{ \code{\link{two.samp.cir}}, \code{\link{sim.cir.2}}, \code{\link{make.twosex.pop}} }
\examples{
library(wisp)
example <- make.twosex.pop(abund=200, prop.male=0.4)
example.3.result <- three.samp.cir(rm.pop=example, frac.prehunt=0.2, frac.midhunt=0.3, frac.posthunt=0.4,
                                   frac.harv.male.1=0.4, frac.harv.fem.1=0.05,
                                   frac.harv.male.2=0.5, frac.harv.fem.2=0.05)
example.3.result
}
\keyword{ htest }% at least one, from doc/KEYWORDS

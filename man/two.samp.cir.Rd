\name{two.samp.cir}
\alias{two.samp.cir}
\title{ Computes analytical abundance estimates for two-sample Change-in-ratio }
\description{
  Performs sex-specific harvest of animals from a simulated population, and
  produces analytical estimates of abundance and standard error for two-occasion
  change-in-ratio estimator.
}
\usage{
two.samp.cir(rm.pop, frac.prehunt, frac.posthunt, frac.harv.male, frac.harv.fem)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rm.pop}{ Population object being harvested and sampled (likely created by \code{\link{make.twosex.pop}}) }
  \item{frac.prehunt}{ Proportion of the population sampled prior to the harvest }
  \item{frac.posthunt}{ Proportion of the population sampled after to the harvest }
  \item{frac.harv.male}{ Proportion of males in the population removed during harvest }
  \item{frac.harv.fem}{ Proportion of females in the population removed during harvest }
}
\details{
  This function augments the WiSP package by performing type-specific (think about
  type as equivalent to gender) harvest (or removal).  The sampling process of a
  change-in-ratio study is to
   a) sample the population prior to the harvest to estimate sex ratio, b) harvest population in a sex
   specific manner, and c) sample population after harvest to again estimate sex ratio.
}
\value{
  \item{point.cir }{Estimated point estimate of abundance}
  \item{se.cir }{Standard error of abundance estimate}
  \item{coef.var }{Coefficient of variation of estimate (not percentage)}
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ Specifically, eqn. 5.15 of Borchers et al. (2002) is used  for the point estimate, and 5.17 for the variance estimate.
  This function constitutes a building block
 of \code{\link{sim.cir.2}} and its relative \code{\link{sim.cir.2.summary}}, in that repeated calls to
 \code{two.samp.cir} are made by these other functions to generate replicate realizations of an experiment.}

\section{Warning }{It is not impossible for the point estimate of abundance to be negative; particularly when the difference
in harvest between males and females is small.  This is a nonsensical result, and hence, the estimate is not considered
admissable.  In this situation, all values returned by this function are set equal to \code{NA}.}

\seealso{ \code{\link{two.samp.cir}}, \code{\link{sim.cir.2}}, \code{\link{make.twosex.pop}} }
\examples{
library(wisp)
example.popn <- make.twosex.pop(abund=200, prop.male=0.4)
single.example.result <- two.samp.cir(rm.pop=example.popn, frac.prehunt=0.2, frac.posthunt=0.4,
                         frac.harv.male=0.6, frac.harv.fem=0.02)
single.example.result
}
\keyword{ htest }% at least one, from doc/KEYWORDS

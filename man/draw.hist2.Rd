\name{draw.hist2}
\alias{draw.hist2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Show distribution of point estimates of a set of change-in-ratio (CIR) experiments }
\description{
  Create histogram of point estimates of abundance from replicate CIR experiments.
  
}
\usage{
draw.hist2(df = df, my.max = my.max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ Data frame created by \code{\link{sim.cir.2}} of repeated simulations
  of a 2-occasion (or 3 occasion from \code{\link{sim.cir.3}}) CIR experiment}
  \item{my.max}{ Maximum value of point estimates to be portrayed in the histogram.
  This exists because the distribution of CIR point estimates of abundance tends to
  have a long right-tail, and unless this tail is ignored, much of the nature of the
  remaining distribution is lost. }
}
\details{
  The function could be generalized to draw not only the distribution of the point estimate,
  but also the distribution of the standard error of the simulated replicates, if desired.
}
\value{
Produces a histogram of point estimates, with a maximum specified by the user, and
labeled with the arguments used to call \code{\link{sim.cir.2}}
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ This function makes use of \code{is.finite} on the elements of the data frame
prior to allowing the distribution to be plotted. }

\seealso{ \code{\link{sim.cir.2}}, \code{\link{sim.cir.3}}, \code{\link{make.twosex.pop}} }
\examples{
example <- make.twosex.pop(abund=200, prop.male=0.4)
example.result <- sim.cir.2(popn=example, frac.prehunt=0.2, frac.posthunt=0.4,
                  frac.harv.male=0.6, frac.harv.fem=0.02, B=200)
draw.hist2(df=example.result, my.max=1000)
}
\keyword{ htest }% __ONLY ONE__ keyword per line

\name{point.est.compare}
\alias{point.est.compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Compare model fits to data sets }
\description{
  This function will produce a tabulated summary of point estimates from a list of
  fitted models.  They are sorted in order of decreasing AIC score, and AIC weights are
  computed, so that weighted averages of abundances are also computed.
}
\usage{
point.est.compare(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ Wisp objects containing point estimates from a data set.  All objects
  in the list must be of similar types, i.e., estimates from distance sampling estimators, or
  estimates from capture-recapture estimators. }
}
\details{
  If all of the included objects do not possess AIC values, then AIC weights and
  consequently weighted averages will not be produced.
}
\value{
  Produces a matrix with a row for each model entered as an argument, and final 
  row of weighted average.  Columns include model name, AIC, delta AIC, AIC weight, 
  estimated number of groups, estimated number of individuals, and estimated group size.
}
\references{ Burnham and Anderson. 2002. Model selection and multimodel inference, second edition. Springer.}
\author{ Mike Meredith, Wildlife Conservation Society \email{mmeredith@wcs.org} }
\seealso{ \code{\link{point.est.crM0}}, \code{\link{point.est.lt}} }
\examples{
data(taxi.samp.cr)
my.point.est.crM0 <- point.est.crM0(taxi.samp.cr)
my.point.est.crMt <- point.est.crMt(taxi.samp.cr)
my.point.est.crMb <- point.est.crMb(taxi.samp.cr)
my.point.est.crMh <- point.est.crMh(taxi.samp.cr)
point.est.compare(my.point.est.crM0, my.point.est.crMb, 
                  my.point.est.crMt, my.point.est.crMh)

data(tortoise.samp.lt)
lt.halfnorm <- point.est.lt(tortoise.samp.lt, plot=TRUE, title=TRUE, 
                 conditional=TRUE, model="half.normal")
lt.hazrate <- point.est.lt(tortoise.samp.lt, plot=TRUE, title=TRUE, 
                 conditional=TRUE, model="hazard.rate")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}


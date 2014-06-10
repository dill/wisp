\name{generate.design.pl}
\alias{generate.design.pl}
\title{Plot Sampling Survey Design Specification}
\description{
        Generates a plot sampling survey design, using the parameters in a `pars.design.pl' object.
}
\usage{
        generate.design.pl(pars.design.pl, seed = NULL)
}
\arguments{
\item{pars.design.pl}{A plot sampling design parameter object returned by the function \code{setpars.design.pl}}
\item{seed}{the number passed to set.seed() to initialise random number generator}
}
\details{
        Whereas `setpars.design.pl' sets up the rules that are used to generate a covered region for a plot sampling survey, this function introduces randomness and generates an actual covered region. The function \code{setpars.design.pl} must have been run before `generate.design.pl' is run.}
\value{
\code{generate.design.pl} returns an object of class `design.pl´ which has the following elements:{
  \item{region}{object of class `region´.}
  \item{number.areas}{number of survey units (plots).}
  \item{area.coordinates}{coordinates of the bottom left and top right corners of each survey unit.}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}}
}
\seealso{
        \code{\link{setpars.design.pl}}
}
\examples{
pl.reg <- generate.region(x.length = 100, y.width = 50)
pl.dens <- generate.density(pl.reg)
pl.poppars<-setpars.population(density.pop=pl.dens, number.groups = 100, size.method = "poisson",
                               size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                               exposure.min = 2, exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pl.pop <- generate.population(pl.poppars, seed=456)
pl.despars<-setpars.design.pl(pl.reg, n.interval.x = 10, n.interval.y = 20,
                              method = "random", area.covered = 0.2)
pl.des <- generate.design.pl(pl.despars, seed=789)
summary(pl.des)
}
\keyword{}


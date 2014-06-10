\name{summary.sample.pl}
\alias{summary.sample.pl}
\title{Default Summary Statement for Survey Sample of Plot Method}
\description{
        This function summarises the content of a `sample.pl´ object.
}
\usage{
        summary(samp, digits = 5)
}
\arguments{
\item{samp}{object of class `sample.pl´.}
\item{digits}{number of significant figures that numbers are rounded to for display}
}
\seealso{
        \code{\link{generate.sample.pl}} to create sample objects
     	\code{\link{plot.sample.pl}}
}
\examples{
pl.reg <- generate.region(x.length = 100, y.width = 50)
pl.dens <- generate.density(pl.reg)

pl.poppars<-setpars.population(density.pop=pl.dens, number.groups = 100, size.method = "poisson", 
						size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
						exposure.min = 2, exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pl.pop <- generate.population(pl.poppars, seed=456)
pl.despars<-setpars.design.pl(pl.reg, n.interval.x = 10, n.interval.y = 20,method = "random", area.covered = 0.2)
pl.des <- generate.design.pl(pl.despars, seed=789)
pl.samp<-generate.sample.pl(pl.pop, pl.des, seed=101112)

summary(pl.samp)
plot(pl.samp)
}
\keyword{}


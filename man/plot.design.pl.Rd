\name{plot.design.pl}
\alias{plot.design.pl}
\title{Default Plotting Statement for Survey Design of Plot Method }
\description{
        This function generates a graphical representation of the specified `design.pl´ object.
}
\usage{
        plot(des, newplot = T)
}
\arguments{
\item{des}{object of class `design.pl´.}
\item{newplot}{if true the new plot is added to the preceding output.}
}
\details{
        This function displays a survey design. It shows a picture of the region and the survey units.
}
\seealso{
        \code{\link{setpars.design.pl}},   \code{\link{generate.design.pl}} 
}
\examples{
pl.reg <- generate.region(x.length = 100, y.width = 50)
pl.dens <- generate.density(pl.reg)

pl.poppars<-setpars.population(density.pop=pl.dens, number.groups = 100,
                               size.method = "poisson", size.min = 1, size.max = 5,
                               size.mean = 1, exposure.method = "beta", exposure.min = 2,
                               exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pl.pop <- generate.population(pl.poppars, seed=456)
pl.despars<-setpars.design.pl(pl.reg, n.interval.x = 10, n.interval.y = 20,method = "random",area.covered = 0.2)
pl.des <- generate.design.pl(pl.despars, seed=789)
summary(pl.des)
plot(pl.des)
}
\keyword{}


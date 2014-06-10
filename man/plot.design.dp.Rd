\name{plot.design.dp}
\alias{plot.design.dp}
\title{Default Plotting Statement for Survey Design of Double Platform Method}
\description{
        This function generates a graphical representation of the specified `design.dp´ object.
}
\usage{
        plot(des, show.paths = FALSE)
}
\arguments{
\item{des}{object of class `design.pt´.}
\item{show.paths}{if true the transect paths will be added as arrows pointing into the direction of the current transect.}
}
\details{
        This function displays a survey design. It shows a picture of the region and the survey units.
}
\seealso{
        \code{\link{setpars.design.dp}},   \code{\link{generate.design.dp}} 
}
\examples{
dp.reg <- generate.region(x.length = 100, y.width = 50)
dp.dens <- generate.density(dp.reg)

#heterogeneous population
dp.poppars<-setpars.population(density.pop = dp.dens, number.groups = 1000, size.method = "poisson", 
						size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta", 
						exposure.min = 0, exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
dp.pop<-generate.population(dp.poppars)

dp.despars<-setpars.design.dp(dp.reg, n.transects=10, n.units=10, visual.range=2, percent.on.effort=1)
dp.des<-generate.design.dp(dp.despars, seed=3)
summary(dp.des)
plot(dp.des, show.paths=TRUE)
}
\keyword{}


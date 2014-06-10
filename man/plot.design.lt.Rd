\name{plot.design.lt}
\alias{plot.design.lt}
\title{Default Plotting Statement for Survey Design of Line Transect Method}
\description{
        This function generates a graphical representation of the specified `design.lt´ object.
}
\usage{
        plot(des, show.paths = FALSE)
}
\arguments{
\item{des}{object of class `design.lt´.}
\item{show.paths}{if true the transect paths will be added as arrows pointing into the direction of the current  
transect.}
}
\details{
        This function displays a survey design. It shows a picture of the region and the survey units.
}
\seealso{
        \code{\link{setpars.design.lt}},   \code{\link{generate.design.lt}}
} 
\examples{
lt.reg <- generate.region(x.length = 100, y.width = 50)
lt.dens <- generate.density(lt.reg)

#heterogeneous population
lt.poppars<-setpars.population(density.pop = lt.dens, number.groups = 1000, size.method = "poisson", size.min = 1,  
size.max = 30, size.mean = 10, exposure.method = "beta", exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,  
exposure.shape = 0.5, type.values=c("Male","Female"), type.prob=c(0.48,0.52))
lt.pop<-generate.population(lt.poppars)

lt.despars<-setpars.design.lt(lt.reg, n.transects=10, n.units=10, visual.range=4, percent.on.effort=1)
lt.des<-generate.design.lt(lt.despars, seed=3)
summary(lt.des)
plot(lt.des, show.paths=TRUE)
}
\keyword{}


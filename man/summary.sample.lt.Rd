\name{summary.sample.lt}
\alias{summary.sample.lt}
\title{Default Summary for Line Transect Survey Parameters}
\description{
        This function summarises the content of a `sample.lt´ object.
}
\usage{
        summary(samp, digits = 5)
     	summary.sample.lt(samp)
}
\arguments{
\item{par}{object of class `sample.lt´.}
\item{digits}{number of significant figures that numbers are rounded to for display}
}
\seealso{
        \code{\link{setpars.survey.lt}},   \code{\link{generate.sample.lt}}
     	\code{\link{plot.sample.lt}}
}
\examples{
lt.reg <- generate.region(x.length = 100, y.width = 50)
lt.dens <- generate.density(lt.reg)

#heterogeneous population
lt.poppars<-setpars.population(density.pop = lt.dens, number.groups = 1000, size.method = "poisson", 
						size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta", 
						exposure.min = 0, exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
lt.pop<-generate.population(lt.poppars)

lt.despars<-setpars.design.lt(lt.reg, n.transects=10, n.units=10, visual.range=4, percent.on.effort=1)
lt.des<-generate.design.lt(lt.despars, seed=3)
lt.survpars<-setpars.survey.lt(lt.pop, lt.des, disthalf.min=2, disthalf.max=4)
lt.samp<-generate.sample.lt(lt.survpars)

summary(lt.samp)
plot(lt.samp)
}
\keyword{}

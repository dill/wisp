\name{summary.pars.survey.lt}
\alias{summary.pars.survey.lt}
\title{Default Summary for Line Transect Survey Parameters}
\description{
        This function summarises the content of a `pars.survey.lt´ object and produces plots of the survey region and detection function.
}
\usage{
        summary(pars, digits=5, plot=FALSE)
}
\arguments{
\item{pars}{object of class `pars.survey.lt´.}
\item{digits}{The number of significant figures numbers are rounded to}
\item{plot}{if TRUE, a plot is produced of the detection functions for minimum and maximum exposures}
}
\details{
        This function is useful for viewing the detection function specified in a `pars.survey.lt' object; the detection functions for the least detectable animals (those with minimum exposure) and for the most detectable animals (those with maximum exposure) are plotted. 
The detection function form and its parameter values are printed.
}
\seealso{
        \code{\link{setpars.survey.lt}},   \code{\link{generate.sample.lt}}
		\code{\link{summary.sample.lt}}
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
summary(lt.survpars, plot=TRUE)
}
\keyword{}


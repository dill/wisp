\name{summary.pars.survey.pt}
\alias{summary.pars.survey.pt}
\title{Default Summary for Point Transect Survey Parameters}
\description{
     This function summarises the content of a `pars.survey.pt´ object and produces plots of the survey region and detection function.}
\usage{
      summary(pars, digits=5, plot=FALSE)
}
\arguments{
\item{pars}{object of class `pars.survey.pt´.}
\item{digits}{The number of significant figures numbers are rounded to}
\item{plot}{if TRUE, a plot is produced of the detection functions for minimum and maximum exposures}
}
\details{
This function is useful for viewing the detection function specified in a `pars.survey.lt' object; the detection functions for the least detectable  
animals (those with minimum exposure) and for the most detectable animals (those with maximum exposure) are plotted. 

The detection function form and its parameter values are printed.
}
\seealso{
     \code{\link{setpars.survey.pt}}, \code{\link{generate.sample.pt}}, \code{\link{summary.sample.pt}}
}
\examples{
pt.reg <- generate.region(x.length = 100, y.width = 50)
pt.dens <- generate.density(pt.reg)

#heterogeneous population
pt.poppars<-setpars.population(density.pop = pt.dens, number.groups = 1000, size.method = "poisson", 
						size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta", 
						exposure.min = 0, exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
pt.pop<-generate.population(pt.poppars)

pt.despars<-setpars.design.pt(pt.reg, n.transects=8, n.units=32, visual.range=5)
pt.des<-generate.design.pt(pt.despars)
pt.survpars<-setpars.survey.pt(pt.pop, pt.des, disthalf.min=1, disthalf.max=2)
summary(pt.survpars)
plot(pt.survpars)
}
\keyword{}


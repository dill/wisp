\name{summary.sample.pt}
\alias{summary.sample.pt}
\title{Default Summary Statement for Survey Sample of Point Transect Method}
\description{
     This function summarises the content of a `sample.pt´ object and plots a histogram of the observed distances.}

\usage{
     summary(samp)
     summary.sample.pt(samp)     
     }
\arguments{
\item{samp}{object of class `sample.pt´.}
}
\seealso{
     \code{\link{setpars.survey.pt}}, \code{\link{generate.sample.pt}} 
     \code{\link{plot.sample.pt}}
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
pt.samp<-generate.sample.pt(pt.survpars)
summary(pt.samp)
plot(pt.samp)
}
\keyword{}


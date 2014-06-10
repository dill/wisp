\name{summary.sample.rm}
\alias{summary.sample.rm}
\title{Default Summary Statement for Survey Sample of Removal Methods}
\description{
        This function summarises the content of a `sample.rm´ object.
}
\usage{
        summary(samp)
}
\arguments{
\item{samp}{object of class `sample.rm´.}
}
\seealso{
        \code{\link{setpars.survey.rm}},   \code{\link{generate.sample.rm}} 
     	\code{\link{plot.sample.rm}}
}
\examples{
rm.reg<-generate.region(x.length=100, y.width=50)
rm.dens <- generate.density(rm.reg)

rm.poppars<-setpars.population(density.pop = rm.dens, number.groups = 100, size.method = "poisson", 
						size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
						exposure.min = 2, exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
rm.pop<-generate.population(rm.poppars)

rm.des<-generate.design.rm(rm.reg, n.occ = 5, effort=c(1,2,3,2,1))
rm.survpars<-setpars.survey.rm(pop=rm.pop, des=rm.des, pmin=0.03, pmax=0.95, improvement=0.05)
rm.samp<-generate.sample.rm(rm.survpars)

summary(rm.samp) 
plot(rm.samp)
}
\keyword{}


\name{summary.sample.cr}
\alias{summary.sample.cr}
\title{Default Summary Statement for Mark-Recapture Sample}
\description{
        This function summarises the content of a `sample.cr´ object.
}
\usage{
        summary(samp, digits = 5)
}
\arguments{
\item{samp}{object of class `sample.cr´.}
\item{digits}{the number of significant figures numbers are rounded to}
}
\seealso{
        \code{\link{setpars.survey.cr}},   \code{\link{generate.sample.cr}}
	 	\code{\link{plot.sample.cr}}
}
\examples{
cr.reg<-generate.region(x.length=100, y.width=50)
cr.dens <- generate.density(cr.reg)

cr.poppars<-setpars.population(density.pop = cr.dens, number.groups = 100, 
						size.method = "poisson", size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
						exposure.min = 2, exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5, 
						type.values = c("Male","Female"), type.prob = c(0.48,0.52))
cr.pop<-generate.population(cr.poppars)

cr.des<-generate.design.cr(cr.reg, n.occ = 4)
cr.survpars<-setpars.survey.cr(cr.pop, cr.des, pmin.unmarked=0.00001, pmax.unmarked=0.5, improvement=0.01)
cr.samp<-generate.sample.cr(cr.survpars)

summary(cr.samp) 
}
\keyword{}


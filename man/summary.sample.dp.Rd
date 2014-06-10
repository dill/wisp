\name{summary.sample.dp}
\alias{summary.sample.dp}
\title{Default Summary Statement for Survey Sample of Double Platform Method}
\description{
        This function summarises the content of a `sample.dp´ object.
}
\usage{
        summary.sample.dp(samp)
}
\arguments{
\item{samp}{object of class `sample.dp´.}
}
\seealso{
        \code{\link{setpars.survey.dp}},   \code{\link{generate.sample.dp}} 
     	\code{\link{plot.sample.dp}}
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
dp.survpars<-setpars.survey.dp(dp.pop, dp.des, adjust.interactive=FALSE, theta.obs1=0.35, theta.obs2=0, theta.exp=3, theta.dist=-2)
dp.samp<-generate.sample.dp(dp.survpars)

summary(dp.samp)
plot(dp.samp)
}
\keyword{}

\name{int.est.rm}
\alias{int.est.rm}
\title{Removal Method Abundance Estimation: Interval Estimate}
\description{
        This function estimates the animal population size for the current survey sample of the simple removal method.
}
\usage{
        int.est.rm(samp, ci.type = "boot.nonpar", nboot = 999, vlevels = c(0.025,0.975), numerical = TRUE, plot = FALSE, seed = NULL, ...)
}
\arguments{function (samp, ci.type="boot.nonpar", nboot=999, vlevels=c(0.025,0.975), numerical=TRUE, plot=FALSE, seed=NULL, ...) 
\item{samp}{object of class `sample.rm´.}
\item{ci.type}{method for constructing the confidence interval. Possible methods are
             *  `boot.par´ for parametric bootstrap CI, 
             *  `boot.nonpar´ for nonparametric bootstrap CI, or
             *  `profile´ to get a profile likelihood CI.}
\item{nboot}{number of bootstrap replications.}
\item{vlevels}{vector of percentage levels for confidence intervals.}
\item{numerical}{if TRUE, estimation is by numerical maximisation of the log likelihood function.  Otherwise, estimation is by analytical methods}
\item{plot}{if true the distribution of the estimator of N is plotted (bootstrap methods only).}
\item{seed}{the number passed to set.seed() to initialise random number generator}
\item{...}{other plot parameters}
}
\value{
An object of class `int.est.rm´ containing the following items:
  \item{levels}{percentage levels for confidence interval}
  \item{ci}{the confidence interval}
  \item{boot.mean}{mean of bootstrap estimates}
  \item{boot.dbn}{full set of nboot bootstrap estimates.}
  \item{se}{standard error}
  \item{cv}{coefficient of variation}
  \item{ci.type}{Equal to the object 'ci.type' passed to the function}
  \item{proflik}{(not currently used)}
  \item{numerical}{Equal to the object 'numerical' passed to the function}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function} 
}
\note{
        The warning `NA/Inf replaced by maximum positive value' may occur; this seems usually to be because the numerical optimization routine tried searching in an infeasible region of the paramter space. 
}
\seealso{
        \code{\link{setpars.survey.rm}},   \code{\link{generate.sample.rm}}
     	\code{\link{point.est.rm}}, 	   \code{\link{set.seed}}
}
\examples{
rm.reg<-generate.region(x.length=100, y.width=50)

rm.dens <- generate.density(rm.reg)
rm.poppars<-setpars.population(density.pop = rm.dens, number.groups = 100, size.method = "poisson", size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", exposure.min = 2, exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5, type.values=c("Male","Female"), type.prob=c(0.48,0.52))
rm.pop<-generate.population(rm.poppars)

rm.des<-generate.design.rm(rm.reg, n.occ = 5, effort=c(1,2,3,2,1))
rm.survpars<-setpars.survey.rm(pop=rm.pop, des=rm.des, pmin=0.03, pmax=0.95, improvement=0.05)
rm.samp<-generate.sample.rm(rm.survpars)

rm.ci<-int.est.rm(rm.samp)
summary(rm.ci)
plot(rm.ci, est="Nhat.grp")
plot(rm.ci, est="Es", type="hist")
plot(rm.ci, est="Nhat.ind", type="box")
}
\keyword{}


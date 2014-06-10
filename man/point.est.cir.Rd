\name{point.est.cir}
\alias{point.est.cir}
\title{Change-In-Ration Method Abundance Estimation: Point Estimate}
\description{
        This function estimates abundance and related parameters from a change-in-ratio method sample object (of class `sample.cir').
}
\usage{
        point.est.cir(samp, numerical=TRUE)
}
\arguments{
\item{samp}{object of class `sample.rm´.}
\item{numerical}{if TRUE, estimation is by numerical maximisation of the likelihood}
}
\details{
        The analytical estimator (numerical=FALSE) can only be used for 2 survey occasions and is given by:
 
Nhat = [ R_2(1) - R_2 * p_2(1) ] / [ p_1(1) - p_2(1) ] where
 
        R_2(1):= the number of removals at the beginning of the second 
                 survey occasion of type 1\cr
		R_2:=    the total number of removals at the beginning of the second 
                 survey occasion\cr
		p_1(1):= the estimated initial proportion of type 1 in the population\cr
		p_2(1):= the estimated proportion of type 1 in the population at the
                 beginning of the second survey occasion
 
Please note:\cr
The analytical estimator has got very bad statistical properties. It mostly results in estimators which are actually even smaller than the number of detected animals!
If this happens the function will print out a warning message.
 
Possible errors of the function:\cr
Numerical estimation uses the R-function 'nlm' to maximize the full likelihood.  This maximization routine sometimes fails to deliver a sensible estimate for N or even stops with an error if it could not calculate any estimator at all.  This is dependent on the sample.
}
\value{ 
An object of class `point.est.cir´ containing the following items:
  \item{sample}{details of the object of class 'sample.cr', used to create the sample}
  \item{Nhat}{ML - estimator of animal abundance N}
  \item{theta}{Estimator of detection function parameter (only available when using numerical estimation)}
  \item{phat}{Estimators of detection probability on each survey occasion (only available when using numerical estimation)}
  \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}
  \item{Nshats}{Estimator of all separate N of the different animal types (only available when using numerical estimation)}
  \item{log.Likelihood}{the value of the log likelihood function at the maximum}
  \item{AIC}{Akaike´s Information Criterion}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time} 
  \item{numerical}{if TRUE, estimation is by numerical maximisation of the log likelihood function.  If FALSE, estimation is by analytical methods}
}
\seealso{
        \code{\link{setpars.survey.rm}},   \code{\link{generate.sample.rm}}
		\code{\link{int.est.cir}}
}
\examples{
rm.reg<-generate.region(x.length=100, y.width=50)

rm.dens <- generate.density(rm.reg)
rm.poppars<-setpars.population(density.pop = rm.dens, number.groups = 100,
                               size.method = "poisson", size.min = 1, size.max = 5,
                               size.mean = 1, exposure.method = "beta", exposure.min = 2,
                               exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5,
                               type.values=c("Male","Female"), type.prob=c(0.48,0.52))
rm.pop<-generate.population(rm.poppars)

rm.des<-generate.design.rm(rm.reg, n.occ = 5, effort=c(1,2,3,2,1))
rm.survpars<-setpars.survey.rm(pop=rm.pop, des=rm.des, pmin=0.03, pmax=0.95, improvement=0.05)
rm.samp<-generate.sample.rm(rm.survpars)

# Change-in-ratio method
cir.est<-point.est.cir(rm.samp)
summary(cir.est)
}
\keyword{}


\name{int.est.cir}
\alias{int.est.cir}
\title{Change-In-Ratio Method Abundance Estimation: Interval Estimate}
\description{
        This function estimates a confidence interval for group abundance for the change in ratio method.
}
\usage{
        int.est.cir(samp, ci.type="boot.par", nboot = 99,  vlevels = c(0.025, 0.975), plot = FALSE, 
				numerical = TRUE, seed = NULL, ...)
}
\arguments{
\item{samp}{The survey sample (object of class `sample.rm´).}
\item{ci.type}{The method used to compute the confidence interval. Choose			
		     * 'boot.par' for parametric boostrap	
		     * 'normal' for calculating the confidence interval assuming
		        that the estimator of N is normally distributed. This method
		        is only implemented for 2 types of animals and exactly
		        2 survey occasions (because it uses analytical estimation
		        of N, see \code{point.est.cir})}
\item{nboot}{Number of bootstrap resamples. This parameter is only relevant if 'ci.type' is 'boot.par'.}
\item{vlevels}{The percentage levels of the confidence interval}
\item{plot}{If TRUE, (only for bootstrap method) the distribution of the bootstrap estimators will be plotted including the confidence interval. If FALSE no plot will be generated.}
\item{numerical}{if TRUE estimation is by numerical maximisation of the log likelihood function.  if FALSE, estimation is by analytical methods}
\item{seed}{the number passed to set.seed() to initialise random number generator}
\item{...}{Additional plot parameters}
}
\details{
        The parametric bootstrap method uses the estimate of N and of the detection probabilities delivered by \code{point.est.cir}. It generates bootstrap samples using a binomial distribution model using these point estimates as parameters. For each bootstrap sample the point estimate is calculated.
Note: The bootstrap samples do not feature seperate removals (see \code{setpars.survey.cir}).
Note: The running time of the parametric bootstrap algorithm increases with  the number of bootstrap replicates ('nboot') and with the number of animal types in the population.
 
Possible errors of the function:

The point estimation function uses the numerical optimization function  \code{nlm} provided by R to maximize full likelihood. The more bootstrap resamples are generated (the higher 'nboot' is) the higher is the probability of 'nlm' failing to calculating a sensible estimator. This is also dependent on the original sample. If \code{nlm} fails to calculate an estimator \code{int.est.cir} stops with an error message.
 
This problem will be addressed as soon as we find time to address it.

 There are difficulties with this incarnation of the change-in-ratio estimator.  At present, consider the use of \code{\link{two.samp.cir}} and  \code{\link{three.samp.cir}} as alternatives in the two- and three- sample situations.
}
\value{
An object of class `int.est.cir´ containing the following items:
  \item{levels}{percentage levels for confidence interval}
  \item{ci}{the confidence interval}
  \item{boot.mean}{mean of bootstrap estimates}
  \item{boot.dbn}{full set of nboot bootstrap estimates.}
  \item{seNhat}{Standard error of Nhat (only available if ci.type = normal)}
}
\note{
The warning `NA/Inf replaced by maximum positive value' may occur; this seems usually to be because the numerical optimization routine tried searching in an infeasible region of the paramter space. 
}
\seealso{
     \code{\link{setpars.survey.rm}}, \code{\link{generate.sample.rm}} to create sample objects
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
cir.ci<-int.est.cir(rm.samp, nboot=25)
summary(cir.ci)
plot(cir.ci)
}
\keyword{}


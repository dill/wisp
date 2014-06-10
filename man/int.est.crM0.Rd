\name{int.est.crM0}
\alias{int.est.crM0}
\title{Mark Recapture Model M0 Abundance Estimation: Interval Estimate}
\description{
        This function estimates a confidence interval for group abundance from mark recapture method M0.
}
\usage{
        int.est.M0(samp, init.N = -1, ci.type = "boot.nonpar", nboot = 999, vlevels = c(0.025, 0.975),
                   plot = FALSE, Chapmod = FALSE, numerical = FALSE, seed=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{samp}{object of class `sample.crM0´.}
\item{init.N}{starting value of N used by the maximum likelihood optimisation routine}
\item{ci.type}{method for constructing the confidence interval. Possible methods are

                *  `boot.par´ gives a parametric bootstrap CI,
             
                *  `boot.nonpar´ gives a nonparametric bootstrap CI,
             
                *  `normal´ gives a CI based on the assumption that the
                 estimator is normally distributed (2 capture occasions only),
             
                *  `lognormal´ gives a CI based on the assumption that the 
			     estimator is normally distributed (2 capture occasions only), 
        
                *  `profile´ gives a profile likelihood CI (2 capture occasions only). 
}
\item{nboot}{number of bootstrap replications.}
\item{vlevels}{vector of percentage levels for confidence intervals.}
\item{plot}{if true the distribution of the group abundance estimates are plotted.}
\item{Chapmod}{if TRUE, Chapman's modification of the Petersen estimator is used (2 capture occasions only)}
\item{numerical}{if FALSE, an analytic estimator is used (2 capture occasions only), else the likelihood is maximized numerically}
\item{seed}{Number passed to set.seed() to initialise random number generator}
}
\details{
        Details of the bootstrap methods are given in Borchers et al. (2002), pp112-113.
}
\value{
An object of class `int.est.crM0´ containing the following items:
  \item{levels}{percentage levels for confidence interval}
  \item{ci}{the confidence interval}
  \item{boot.mean}{mean of bootstrap estimates}
  \item{boot.dbn}{full set of nboot bootstrap estimates.}
  \item{init.N}{Equal to the object 'init.N' passed to the function}
  \item{Chapmod}{Equal to the object 'Chapmod' passed to the function}
  \item{numerical}{Equal to the object 'numerical' passed to the function}
  \item{se}{standard error}
  \item{cv}{coefficient of variation}
  \item{ci.type}{Equal to the object 'ci.type' passed to the function}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function} 
}
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations. Springer. London. 314pp.
}
\seealso{
        \code{\link{setpars.survey.cr}},   \code{\link{generate.sample.cr}} 
        \code{\link{point.est.crM0}}
}
\examples{
cr.reg<-generate.region(x.length=100, y.width=50)

cr.dens <- generate.density(cr.reg)
cr.poppars<-setpars.population(density.pop = cr.dens, number.groups = 100, 
                              size.method = "poisson", size.min = 1, size.max = 5,
                              size.mean = 1, exposure.method = "beta",exposure.min = 2,
                              exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5,
                              type.values = c("Male","Female"), type.prob = c(0.48,0.52))
cr.pop<-generate.population(cr.poppars)

cr.des<-generate.design.cr(cr.reg, n.occ = 4)
cr.survpars<-setpars.survey.cr(cr.pop, cr.des, pmin.unmarked=0.00001, pmax.unmarked=0.5, improvement=0.01)
cr.samp<-generate.sample.cr(cr.survpars)

# M0
cr.ci.M0<-int.est.crM0(cr.samp)
summary(cr.ci.M0)
plot(cr.ci.M0)
}
\keyword{}


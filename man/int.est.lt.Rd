\name{int.est.lt}
\alias{int.est.lt}
\title{Line Transect Method Abundance Estimation: Interval Estimate}
\description{
        This function estimates a confidence interval for group abundance for the line transect method.
}
\usage{
        int.est.lt(sampl, ci.type = "boot.nonpar", nboot = 999, vlevels = c(0.025, 0.975), conditional = TRUE, model = "half.normal", plot = TRUE,  
show.all = FALSE, seed = NULL, ...)
}
\arguments{ 
\item{sampl}{object of class `sample.lt´.}
\item{ci.type}{="boot.nonpar" (no other methods implemented yet)}
\item{nboot}{number bootstrap replicates}
\item{vlevels}{percentage levels for confidence iterval}
\item{conditional}{if TRUE the conditional likelihood (Equation (7.33) of Borchers et al. 2002) is maximized and abundance is estimated using Equation  
(7.34) of Borchers et al. 2002; if FALSE, the full likelihood (Equation (7.32) of Borchers et al. 2002) is maximized.}
\item{model}{="half.normal" or "hazard.rate"}
\item{plot}{=T if you want a plot of boostrap distribution of Nhat.grp}
\item{plot.all.fits}{=F if you want to see the fit for every bootstrap sample as it is fitted.}
\item{seed}{the number passed to set.seed() to initialise random number generator}
\item{...}{other plot arguments}
}
\value{
An object of class `int.est.lt´ containing the following items:
  \item{levels}{percentage levels for confidence interval}
  \item{ci}{the confidence interval}
  \item{boot.mean}{mean of bootstrap estimates}
  \item{boot.dbn}{a list with components "Nhat.grp", "Nhat.ind","theta", "mu", "nL" and "Es" (see \code{point.est.lt}) each containing the full set of  
nboot estimates	of that parameter. (This is useful if you want to create a plot of the bootstrap distribution of the estimates and control bin width  
etc.)}
  \item{se}{standard error}
  \item{cv}{coefficient of variation}
  \item{ci.type}{Equal to the object 'ci.type' passed to the function}
  \item{conditional}{Equal to the object 'conditional' passed to the function}
  \item{parents}{Details of WiSP objects passed to the function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function} 
}
\details{
        The nonparametric bootstrap method resamples transects with replacement (each resample has the same number of transects as were in the  
original sampele). Details of the method are given in Borchers et al. (2002), p146.
}
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations. Springer. London. 314pp.
}
\seealso{
        \code{\link{setpars.survey.lt}},   \code{\link{generate.sample.lt}} 
		\code{\link{point.est.lt}} for point estimation, \code{\link{set.seed}}
}
\examples{
lt.reg <- generate.region(x.length = 100, y.width = 50)
lt.dens <- generate.density(lt.reg)

#heterogeneous population
lt.poppars<-setpars.population(density.pop = lt.dens, number.groups = 1000, size.method = "poisson", size.min = 1, size.max = 30, size.mean = 10,  
exposure.method = "beta", exposure.min = 0, exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5,  type.values=c("Male","Female"),  
type.prob=c(0.48,0.52))
lt.pop<-generate.population(lt.poppars)

lt.despars<-setpars.design.lt(lt.reg, n.transects=10, n.units=10, visual.range=4, percent.on.effort=1)
lt.des<-generate.design.lt(lt.despars, seed=3)
lt.survpars<-setpars.survey.lt(lt.pop, lt.des, disthalf.min=2, disthalf.max=4)
lt.samp<-generate.sample.lt(lt.survpars)

lt.ci<-int.est.lt(lt.samp, vlevels=c(0.025, 0.975), ci.type="boot.nonpar", nboot=99, plot=T, seed=NULL, model="hazard.rate") 
summary(lt.ci)
plot(lt.ci)
}
\keyword{}


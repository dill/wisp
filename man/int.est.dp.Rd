\name{int.est.dp}
\alias{int.est.dp}
\title{Double Platform Line Transect Method Abundance Estimation: Interval Estimate}
\description{
        This function estimates a confidence interval for group abundance for the double platform line transect method.
}
\usage{
        int.est.dp(sampl, ci.type = "boot.nonpar", nboot = 999, vlevels = c(0.025, 0.975), model = "~distance", plot = FALSE, show.all = FALSE, seed = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{sampl}{object of class `sample.dp´.}
\item{ci.type}{="boot.nonpar" (no other methods implemented yet)}
\item{nboot}{number bootstrap replicates}
\item{vlevels}{percentage levels for confidence iterval}
\item{model}{model specification allowing the selection of covariates}
\item{plot}{=T if you want a plot of boostrap distribution of Nhat.grp}
\item{show.all}{if TRUE, shows details for each bootstrap resample, as it is run.}
\item{seed}{the number passed to set.seed() to initialise random number generator}
\item{...}{other plot arguments}
}
\details{
'model' - The default setting is model='~distance' which takes the perpendicular distance of the recorded observation as the sole explanatory variable of the response variable 'seen'.  That is, whether an animal was observed or not is solely a function of its perpendicular distance from the observer.  The WiSP sample object 'sample.dp' contains the variables distance, exposure, groupsize and one factor level variable named types.  A platform variable is also created to denote which of the two 'platforms' observed the animal.  A model including all these explanatory variables would then be defined as: 
        model="~platform + distance + exposure + size + types"
Note that the explanatory variable size refers to groupsize in the sample object.
}
\value{
An object of class `int.est.dp´ containing the following items:
  \item{levels}{percentage levels for confidence interval}
  \item{ci}{the confidence interval}
  \item{boot.mean}{mean of bootstrap estimates}
  \item{boot.dbn}{a list with components "Nhat.grp", "Nhat.ind", "Es", "prob.det", "mu", "nL", "average.g0", "log.Likelihood", and "AIC" (see \code{point.est.dp}) each containing the full set of nboot estimates of that parameter. (This is useful if you want to create a plot of the bootstrap distribution of the estimates and control bin width etc.)}
}
\note{
        The nonparametric bootstrap method resamples transects with replacement (each resample has the same number of transects as were in the original sample). Details of the method are given in Borchers et al. (2002).
}
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations. Springer. London. 314pp.
}
\seealso{
        \code{\link{setpars.survey.dp}},   \code{\link{generate.sample.dp}} 
     	\code{\link{point.est.dp}} for point estimation
}
\examples{
dp.reg <- generate.region(x.length = 100, y.width = 50)
dp.dens <- generate.density(dp.reg)

#heterogeneous population
dp.poppars<-setpars.population(density.pop = dp.dens, number.groups = 1000, size.method = "poisson", size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta", exposure.min = 0, exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5, type.values=c("Male","Female"), type.prob=c(0.48,0.52))
dp.pop<-generate.population(dp.poppars)

dp.despars<-setpars.design.dp(dp.reg, n.transects=10, n.units=10, visual.range=2, percent.on.effort=1)
dp.des<-generate.design.dp(dp.despars, seed=3)
dp.survpars<-setpars.survey.dp(dp.pop, dp.des, disthalf.min=2, disthalf.max=4)
dp.samp<-generate.sample.dp(dp.survpars)

#This may take a minute or two to run.  
#For a quick viewing, press escape, and try again with a low nboot number
#At least 99 is recommended for estimates with real data.  999 is better.
dp.ci<-int.est.dp(dp.samp, nboot=99, model="~distance")
summary(dp.ci)
plot(dp.ci)
}
\keyword{}

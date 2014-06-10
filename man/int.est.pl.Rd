\name{int.est.pl}
\alias{int.est.pl}
\title{Plot Sampling Method Abundance Estimation: Interval Estimate}
\description{
        This function calculates confidence intervals for group abundance for the plot sampling method.
}
\usage{
        int.est.pl(samp, HT=FALSE, vlevels = c(0.025, 0.975), ci.type = "boot.nonpar", 
				nboot = 999, plot = TRUE, seed = NULL, ...)
}
\arguments{
\item{samp}{object of class `sample.pl´.}
\item{HT}{if FALSE, the abundance estimate produced is the MLE, if TRUE it is the Horvitz-Thompson estimate.}
\item{vlevels}{vector of percentage levels for confidence interval.}
\item{ci.type}{method for constructing the confidence interval. Possible methods are
                *  `normal´ for a CI based on assumed normality of the estimator,
                *  `boot.par´ for a parametric bootstrap CI, 
                *  `boot.nonpar´ for a nonparametric bootstrap CI.}
\item{nboot}{number of bootstrap replications.}
\item{plot}{if true the distribution of the estimator of N is to be plotted.}
\item{seed}{the number passed to set.seed() to initialise random number generator}
\item{...}{other plot parameters}
}
\value{
An object of class `int.est.pl´ containing the following items:
  \item{levels}{percentage levels for confidence interval}
  \item{ci}{the confidence interval}
  \item{boot.mean}{mean of bootstrap estimates}
  \item{boot.dbn}{full set of nboot bootstrap estimates.}
}
\seealso{
        \code{\link{generate.sample.pl}},   \code{\link{point.est.pl}}
        \code{\link{summary.sample.pl}}, 	\code{\link{plot.sample.pl}} 
	 	\code{\link{set.seed}}     
}
\examples{
pl.reg <- generate.region(x.length = 100, y.width = 50)
pl.dens <- generate.density(pl.reg)
pl.poppars<-setpars.population(density.pop=pl.dens, number.groups = 100, size.method = "poisson", 
						size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", exposure.min = 2, 
						exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pl.pop <- generate.population(pl.poppars, seed=456)

pl.despars<-setpars.design.pl(pl.reg, n.interval.x = 10, n.interval.y = 20,method = "random", area.covered = 0.2)
pl.des <- generate.design.pl(pl.despars, seed=789)
pl.samp<-generate.sample.pl(pl.pop, pl.des, seed=101112)

# normal-based CI
pl.int.est.norm<-int.est.pl(pl.samp, vlevels = c(0.025, 0.975), ci.type = "normal", nboot = 99, plot = T, seed=1) 
summary(pl.int.est.norm)
plot(pl.int.est.norm)

# parametric bootstrap
pl.int.est.pbs<-int.est.pl(pl.samp, vlevels = c(0.025, 0.975), ci.type = "boot.par", nboot = 99, plot = F, seed=NULL) 
summary(pl.int.est.pbs)
plot(pl.int.est.pbs, nclass=20)

# nonparametric bootstrap
pl.int.est.npbs<-int.est.pl(pl.samp, vlevels = c(0.025, 0.975), ci.type = "boot.nonpar", nboot = 99, plot = F, seed=3)
summary(pl.int.est.npbs)
plot(pl.int.est.npbs)
}
\keyword{}


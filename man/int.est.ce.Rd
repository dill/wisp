\name{int.est.ce}
\alias{int.est.ce}
\title{Catch-Effort Method Abundance Estimation: Interval Estimate}
\description{
        This function estimates confidence intervals for group abundance for the catch-effort method.
}
\usage{
        int.est.ce(samp, ci.type = "boot.nonpar", nboot=999, vlevels=c(0.025,0.975), plot = FALSE, seed = NULL, ...)
}
\arguments{
\item{samp}{object of class `sample.rm´.}
\item{ci.type}{The type of method used to estimate the confidence interval.  The only valid option is:
                * 'boot.nonpar'  Non-parametric bootstrap}
\item{nboot}{number of bootstrap replicates to be used in estimation}
\item{vlevels}{confidence interval percentage levels}
\item{plot}{if TRUE a histogram of the distribution of the boostrap estimates will be plotted. The plot will also show the confidence interval.}
\item{seed}{the number passed to set.seed() to initialise random number generator}
\item{...}{further optional parameters for the plot}
}
\details{
         The parametric bootstrap method uses the estimate of N and of the detection probabilities delivered by \code{point.est.ce}. It generates bootstrap samples using a binomial distribution model using these point estimates as parameters. For each bootstrap sample the point estimate is calculated.
}
\value{
An object of class `int.est.ce´ containing the following items:
  \item{levels}{percentage levels for confidence interval}
  \item{ci}{the confidence interval}
  \item{boot.mean}{mean of bootstrap estimates}
  \item{boot.dbn}{full set of nboot bootstrap estimates.}
  \item{se}{standard error}
  \item{cv}{coefficient of variation}
  \item{ci.type}{Equal to thee object 'ci.type' passed to the function}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}
}
\note{
The warning `NA/Inf replaced by maximum positive value' may occur; this seems usually to be because the numerical optimization routine tried searching in an infeasible region of the paramter space. 
}
\seealso{
        \code{\link{setpars.survey.rm}},   \code{\link{generate.sample.rm}}
		\code{\link{point.est.ce}},		   \code{\link{set.seed}}
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

# Catch-effort method
ce.ci<-int.est.ce(rm.samp)
summary(ce.ci)
plot(ce.ci)
}
\keyword{}


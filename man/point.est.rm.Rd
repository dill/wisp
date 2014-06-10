\name{point.est.rm}
\alias{point.est.rm}
\title{Removal Method Method Abundance Estimation: Point Estimate}
\description{
        This function estimates abundance and related parameters from a simple removal method sample object (of class `sample.rm').
}
\usage{
        point.est.rm(samp, numerical = TRUE, plot = FALSE)
}
\arguments{
\item{samp}{object of class `sample.rm´.}
\item{numerical}{If TRUE the estimator will be calculated by maximising the likelihood derived in "Borchers, Buckland and Zucchini", equation 5.4. If FALSE the estimator will be calculated analytically. The analytic estimator is only available for exactly two survey occasions.}
\item{plot}{If TRUE a plot of the cumulative removals and the resulting estimator will be generated.}
}
\value{
An object of class `point.est.rm´ containing the following items:
  \item{sample}{The data contained in the sample object}
  \item{numerical}{Equal to the object 'numerical' passed to the function}
  \item{Nhat.grp}{MLE of group abundance}
  \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
  \item{phat}{Estimate(s) of capture probability for the relevant model (try it and see)}
  \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}
  \item{log.Likelihood}{Value of log-likelihood at MLE}
  \item{AIC}{Akaike's information criterion}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{setpars.survey.rm}},   \code{\link{generate.sample.rm}} 
     	\code{\link{int.est.rm}} 
}
\examples{
rm.reg<-generate.region(x.length=100, y.width=50)

rm.dens <- generate.density(rm.reg)
rm.poppars<-setpars.population(density.pop = rm.dens, number.groups = 100, size.method = "poisson", 
						size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
						exposure.min = 2, exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
rm.pop<-generate.population(rm.poppars)

rm.des<-generate.design.rm(rm.reg, n.occ = 5, effort=c(1,2,3,2,1))
rm.survpars<-setpars.survey.rm(pop=rm.pop, des=rm.des, pmin=0.03, pmax=0.95, improvement=0.05)
rm.samp<-generate.sample.rm(rm.survpars)

rm.est<-point.est.rm(rm.samp)
summary(rm.est)
}
\keyword{}
 

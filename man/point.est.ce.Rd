\name{point.est.ce}
\alias{point.est.ce}
\title{Catch-Effort Method Method Abundance Estimation: Point Estimate}
\description{
        This function estimates abundance and related parameters from a catch-effort method sample object (of class `sample.ce').
}
\usage{
        point.est.ce(sample, plot = FALSE)
}
\arguments{
\item{sample}{object of class `sample.ce´.}
\item{plot}{if TRUE a plot of the cumulative removals and the fitted estimate.}
}
\value{
An object of class `point.est.ce´ containing the following items:
  \item{sample}{details of the object of class 'sample.cr', used to create the sample}
  \item{Nhat.grp}{MLE of group abundance}
  \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
  \item{theta}{MLE of the parameter of the detection probability function which is
                p(ls) = e^(-theta * ls) where `ls´ is the effort}
  \item{pshat}{Estimators of the detection probability on each survey occasion 
                p(ls) = e^(-theta * ls), where ls is the effort}
  \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}
  \item{log.Likelihood}{Value of log-likelihood at the MLE}
  \item{AIC}{Akaike's information criterion}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{setpars.survey.rm}},   \code{\link{generate.sample.rm}}
     	\code{\link{int.est.ce}}}
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
ce.est<-point.est.ce(rm.samp)
summary(ce.est)
plot(ce.est)
}
\keyword{}


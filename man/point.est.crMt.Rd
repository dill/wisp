\name{point.est.crMt}
\alias{point.est.crMt}
\title{Mark Recapture Method Mt Abundance Estimation: Point Estimate}
\description{
        This function estimates abundance and related parameters from a mark recapture method sample object (of class `sample.cr'), using the mark recapture model Mt. 
}
\usage{
        point.est.crMt(samp.cr, init.N = -1)
}
\arguments{
\item{sample}{object of class `sample.cr.´}
\item{init.N}{starting value of N used in maximum likelihood optimisation routine}
}
\value{
An object of class `point.est.crMt´ containing the following items:
  \item{sample}{details of the object of class 'sample.cr', used to create the sample}
  \item{Nhat.grp}{MLE of group abundance}
  \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
  \item{phat}{Estimate(s) of capture probability for the relevant model (try it and see)}
  \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}
  \item{log.Likelihood}{Value of log-likelihood at MLE}
  \item{res.Deviance}{Residual deviance at MLE}
  \item{AIC}{Akaike's information criterion}
  \item{init.N}{starting value of N used in maximum likelihood optimisation routine}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{setpars.survey.cr}},   \code{\link{generate.sample.cr}} 
		\code{\link{int.est.crMt}} 
}
\examples{
cr.reg<-generate.region(x.length=100, y.width=50)

cr.dens <- generate.density(cr.reg)
cr.poppars<-setpars.population(density.pop = cr.dens, number.groups = 100, 
                               size.method = "poisson", size.min = 1, size.max = 5,
                               size.mean = 1, exposure.method = "beta", exposure.min = 2,
                               exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5,
                               type.values = c("Male","Female"), type.prob = c(0.48,0.52))
cr.pop<-generate.population(cr.poppars)

cr.des<-generate.design.cr(cr.reg, n.occ = 4)
cr.survpars<-setpars.survey.cr(cr.pop, cr.des, pmin.unmarked=0.00001, pmax.unmarked=0.5, improvement=0.01)
cr.samp<-generate.sample.cr(cr.survpars)

# Mt
cr.est.Mt<-point.est.crMt(cr.samp)
summary(cr.est.Mt)
}
\keyword{}

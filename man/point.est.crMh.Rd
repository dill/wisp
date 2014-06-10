\name{point.est.crMh}
\alias{point.est.crMh}
\title{Mark Recapture Method Mh Abundance Estimation: Point Estimate}
\description{
        This function estimates abundance and related parameters from a mark recapture method sample object (of class  
`sample.cr'), using the mark recapture model Mh. 
}
\usage{
        point.est.crMh(sample, num.mix = 2, init.N = -1)
}
\arguments{
\item{sample}{ object of class `sample.cr´}
\item{num.mix}{ number of mixtures of animal groups}
\item{init.N}{ starting value of N used in the maximum likelihood optimisation routine}
}
\details{
        When fitting an MhA model we assume there is a fixed number, A, of animal groups in the population, within each  
of which there is homogeneity of capture.  Specifying the value of num.mix determines the number of animal groups  
assumed to exist in the closed population.  Thus, setting num.mix = 2 would fit the Mh2 model to the data contained in  
'sample'. 
The starting value of N used in the maximum likelihood optimisation routine can be altered to enable a lower AIC value  
to be produced in the output of this function.  Occasionally the choice of starting value can result in the  
optimisation routine failing to converge.
}
\value{
 An object of class `point.est.crMh´ containing the following items:
  \item{sample}{details of the object of class 'sample.cr', used to create the sample}
  \item{Nhat.grp}{MLE of group abundance}
  \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
  \item{phat}{Estimate(s) of capture probability for the animal groups in the relevant model}  
  \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}
  \item{log.Likelihood}{Value of log-likelihood at MLE}
  \item{res.Deviance}{Value of the residual deviance at MLE}
  \item{AIC}{Akaike's information criterion}
  \item{init.N}{starting value of N used in maximum likelihood optimisation routine}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\seealso{ 
        \code{\link{setpars.survey.cr}},   \code{\link{generate.sample.cr}} 
     	\code{\link{int.est.crMh}} 
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

# Mh
cr.est.Mh<-point.est.crMh(cr.samp)
summary(cr.est.Mh)
}
\keyword{}


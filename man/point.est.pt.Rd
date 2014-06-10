\name{point.est.pt}
\alias{point.est.pt}
\title{Point Transect Method Abundance Estimation: Point Estimate}
\description{
  This function estimates abundance and related parameters from a point transect method sample object (of class `sample.pt'). It also plots a density  
histogram of observed radial distances with the estimated probability density function superimposed.
}
\usage{
point.est.lt(sampl, plot = TRUE, title = TRUE, conditional = FALSE, model = "half.normal")
}
\arguments{
\item{sampl}{object of class `sample.pt´}
\item{plot}{if TRUE, a density histogram of observed radial distances with the estimated probability density function superimposed, is plotted}
\item{title}{Flag telling if you want the title "Radial distance distribution and fitted detection function"}
\item{conditional}{if FALSE, the full likelihood (Equation (7.32) of Borchers et al. 2002) is maximized; if TRUE the conditional likelihood (Equation  
(7.33) of Borchers et al. 2002) is maximized and abundance is estimated using Equation (7.34) of Borchers et al. 2002.}
\item{model}{="half.normal" (no other detection function models have implemented yet)}
}
\value{
An object of class `point.est.pt´ containing the following items:

\item{Nhat.ML.grp}{MLE of group abundance}
\item{Nhat.ML.ind}{MLE of individual abundance (= Nhat.grp * Es)}
\item{theta}{MLE of detection function parameter (sigma^2)}
\item{esa}{MLE effective survey area: \eqn{2*pi* \int_0^w x p(x)dx}{2\pi\int_0^w x p(x)\,dx}, where w is the trunction distance, and integration is  
from 0 to w.}
\item{nbar}{"encounter rate" - the mean number of groups detected per point}
\item{Es}{mean group size}
\item{log.likelihood}{the value of the log-likelihood function at the maximum}
\item{AIC}{Akaike´s Information Criterion}
\item{fit.summary}{output from numerical minimization routine}
}
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations. Springer. London. 314pp.
}
\seealso{
     \code{\link{setpars.survey.pt}}, \code{\link{summary.sample.pt}}, \code{\link{summary.pars.survey.pt}}, \code{\link{int.est.pt}}
}
\examples{
pt.reg <- generate.region(x.length = 100, y.width = 50)
pt.dens <- generate.density(pt.reg)

#heterogeneous population
pt.poppars<-setpars.population(density.pop = pt.dens, number.groups = 1000, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
pt.pop<-generate.population(pt.poppars)

pt.despars<-setpars.design.pt(pt.reg, n.transects=8, n.units=32, visual.range=5)
pt.des<-generate.design.pt(pt.despars)

pt.survpars<-setpars.survey.pt(pt.pop, pt.des, disthalf.min=2, disthalf.max=4)
pt.samp<-generate.sample.pt(pt.survpars)

pt.est<-point.est.pt(pt.samp) 
summary(pt.est)
plot(pt.est)
}
\keyword{}


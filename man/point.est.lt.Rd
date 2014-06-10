\name{point.est.lt}
\alias{point.est.lt}
\title{Line Transect Method Abundance Estimation: Point Estimate}
\description{
        This function estimates abundance and related parameters from a line transect method sample object (of class `sample.lt'). 
}
\usage{
        point.est.lt(sampl, plot = FALSE, title = TRUE, conditional = TRUE, model="half.normal")
}
\arguments{
\item{sampl}{object of class `sample.lt´}
\item{plot}{if TRUE, a density histogram of observed radial distances with the estimated probability density function superimposed, is plotted}
\item{title}{Flag telling if you want the title "Perpendicular distance distribution and fitted detection function"}
\item{conditional}{if TRUE the conditional likelihood (Equation (7.8) of Borchers et al. 2002) is maximized and abundance is estimated using Equation (7.12) of Borchers et al. 2002; if FALSE, the full likelihood (Equation (7.10) of Borchers et al. 2002) is maximized.}
\item{model}{="half.normal" or ="hazard.rate" for detection function form}
}
\value{
An object of class `point.est.lt´ containing the following items:
  \item{sample}{details of the object of class 'sample.cr', used to create the sample}
  \item{model}{the model used to fit the detection function}
  \item{conditional}{Equal to the argument 'conditional' passed to the function}
  \item{Nhat.grp}{MLE of group abundance}
  \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
  \item{theta}{MLE of detection function parameter}
  \item{mu}{MLE of effective strip half-width}
  \item{nL}{"encounter rate" - the number of groups detected per unit distance surveyed along lines}
  \item{Es}{mean group size}
  \item{log.likelihood}{the value of the log-likelihood function at the maximum}
  \item{AIC}{Akaike´s Information Criterion}
  \item{fit.summary}{output from numerical minimization routine}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time} 
}
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations. Springer. London. 314pp.
}
\seealso{
        \code{\link{setpars.survey.lt}},  	  \code{\link{generate.sample.lt}}
		\code{\link{summary.pars.survey.lt}}, \code{\link{int.est.lt}}
}
\examples{
lt.reg <- generate.region(x.length = 100, y.width = 50)
lt.dens <- generate.density(lt.reg)

#heterogeneous population
lt.poppars<-setpars.population(density.pop = lt.dens, number.groups = 1000, size.method = "poisson", 
						size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta", 
						exposure.min = 0, exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
lt.pop<-generate.population(lt.poppars)

lt.despars<-setpars.design.lt(lt.reg, n.transects=10, n.units=10, visual.range=4, percent.on.effort=1)
lt.des<-generate.design.lt(lt.despars, seed=3)
lt.survpars<-setpars.survey.lt(lt.pop, lt.des, disthalf.min=2, disthalf.max=4)
lt.samp<-generate.sample.lt(lt.survpars)

# half-normal model
lt.est<-point.est.lt(lt.samp) 
summary(lt.est)

# hazard rate model
lt.est.hr<-point.est.lt(lt.samp, model="hazard.rate") 
summary(lt.est.hr)
}
\keyword{}


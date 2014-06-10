\name{point.est.dp}
\alias{point.est.dp}
\title{Double Platform Line Transect Method Abundance Estimation: Point Estimate}
\description{
        This function estimates abundance and related parameters from a double platform line transect method sample object (of class `sample.dp'). 
}
\usage{
        point.est.dp(samp, model = "~distance")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{samp}{sample of class 'sample.dp' }
\item{model}{model specification allowing the selection of covariates}
}
\details{
        'model' - The default setting is model='~distance' which takes the perpendicular distance of the recorded observation as the sole explanatory variable of the response variable 'seen'.  That is, whether an animal was observed or not is solely a function of its perpendicular distance from the observer.  The WiSP sample object 'sample.dp' contains the variables distance, exposure, groupsize and one factor level variable named types.  A platform variable is also created to denote which of the two 'platforms' observed the animal.  A model including all these explanatory variables would then be defined as:
                model="~platform + distance + exposure + size + types"
Note that the explanatory variable size refers to groupsize in the sample object.
}
\value{
An object of class `point.est.dp´ containing the following items:
  \item{sample}{Sample from which point.est.dp object was created (i.e. the data).}
  \item{Nhat.grp}{MLE of group abundance}
  \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
  \item{Es}{mean group size}
  \item{phat}{MLE of probability of detecting a group - number of distinct groups detected / MLE of group abundance}
  \item{mu}{MLE of effective strip half-width}
  \item{nL}{"encounter rate" - the number of groups detected per unit distance surveyed along lines}
  \item{average.g0}{mean trackline detection probability for each platform and pooled across both platforms}
  \item{log.likelihood}{the value of the log-likelihood function at the maximum}
  \item{AIC}{Akaike´s Information Criterion}
  \item{model.summary}{coefficients output from numerical fitting routine}
  \item{model}{The model formula passed to the function in argument 'model'}
  \item{plotx}{A range of perpendicular distance values to use in plotting detection functions.}
  \item{plotp1}{Observer 1's mean detection probability evaluated at each value in plotx.}
  \item{plotp2}{Observer 2's mean detection probability evaluated at each value in plotx.}
  \item{plotp}{The combined Observer's mean detection probability evaluated at each value in plotx.}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\references{Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations. Springer. London. 314pp. 
}
\seealso{  
        \code{\link{setpars.survey.dp}},   \code{\link{generate.sample.dp}}
}
\examples{
dp.reg <- generate.region(x.length = 100, y.width = 50)
dp.dens <- generate.density(dp.reg)

#heterogeneous population
dp.poppars<-setpars.population(density.pop = dp.dens, number.groups = 1000, size.method = "poisson", 
						size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta", 
						exposure.min = 0, exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
dp.pop<-generate.population(dp.poppars)

dp.despars<-setpars.design.dp(dp.reg, n.transects=10, n.units=10, visual.range=2, percent.on.effort=1)
dp.des<-generate.design.dp(dp.despars, seed=3)
dp.survpars<-setpars.survey.dp(dp.pop, dp.des, adjust.interactive=FALSE, 
							theta.obs1=0.35, theta.obs2=0, theta.exp=3, theta.dist=-2)
dp.samp<-generate.sample.dp(dp.survpars)

dp.est<-point.est.dp(dp.samp,model="~platform + distance")
summary(dp.est)
}
\keyword{}


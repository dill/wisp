\name{point.sim.dp}
\alias{point.sim.dp}
\title{Double Platform Line Transect Method : Simulation}
\description{
        This function simulates sample objects of class 'sample.dp' and estimates abundance and  related parameters for each simulated sample object.  A histogram of the group abundance  point estimates obtained from each sample object is produced with true group abundance and  the mean of the abundance estimates from the simulated sample objects displayed on the plot.
}
\usage{
        point.sim.dp(pop.spec, survey.spec, design.spec, B = 99, model = "~distance", ...)
}
\arguments{
\item{pop.spec}{population specification; either an object of class 'population' or 'pars.population'}
\item{survey.spec}{survey specification; an object of class 'pars.survey.dp'}
\item{design.spec}{design specification; either an object of class 'design.dp' or 'pars.design.dp'}
\item{B}{number of simulations required}
\item{model}{argument for point.est.dp - model specification allowing the selection of covariates}
\item{...}{extra plot arguments}
}
\details{
        This function simulates sample objects of class 'sample.dp' by simulating from the observation model (using survey.spec) and if pop.spec is of class 'pars.population' from the state model (using pop.spec to generate new populations on each simulation ) and if design.spec is of class 'pars.design.dp' from the design (using design.spec to generate new design realizations on each simulation.)  
}
\value{
A results list, each row of which contains the following values:
  \item{Nhat.grp}{MLE of group abundance}
  \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
  \item{Es}{mean group size}
  \item{prob.det}{MLE of probability of detecting a group - number of distinct groups detected / MLE of group abundance}
  \item{mu}{MLE of effective strip half-width}
  \item{nL}{"encounter rate" - the number of groups detected per unit distance surveyed along lines}
  \item{avrg.g0.prim}{mean trackline detection rates for the primary platform}
  \item{avrg.g0.sec}{mean trackline detection rates for the secondary platform}
  \item{avrg.g0.pool}{mean trackline detection rates pooled across both platforms}
  \item{log.likelihood}{the value of the log-likelihood function at the maximum}
  \item{AIC}{Akaike´s Information Criterion}
}
\seealso{
        \code{\link{setpars.population}},   \code{\link{setpars.design.dp}}
     	\code{\link{setpars.survey.dp}},  	\code{\link{point.est.dp}}
}
\examples{
dp.reg <- generate.region(x.length = 100, y.width = 50)
dp.dens <- generate.density(dp.reg)

#heterogeneous population
dp.poppars<-setpars.population(density.pop = dp.dens, number.groups = 1000, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0,  exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
dp.pop<-generate.population(dp.poppars)

dp.despars<-setpars.design.dp(dp.reg, n.transects=10, n.units=10, visual.range=2, percent.on.effort=1)
dp.des<-generate.design.dp(dp.despars, seed=3)
dp.survpars<-setpars.survey.dp(dp.pop, dp.des, theta.obs1=0.7, theta.obs2=0.4, theta.exp=0.2, theta.dist=-0.7)
dp.samp<-generate.sample.dp(dp.survpars)

# simulate (design and population randomization):
# This may take a long time to do all the simulations
# To exit press escape and try with e.g. B=9.  
# At least B=99 is recommended for real data.  999 is better.
dp.sim<-point.sim.dp(pop.spec=dp.poppars, design.spec=dp.despars, survey.spec=dp.survpars, B=99)
summary(dp.sim)
plot(dp.sim)
}
\keyword{}

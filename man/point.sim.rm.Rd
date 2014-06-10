\name{point.sim.rm}
\alias{point.sim.rm}
\title{ Removal Method : Simulation }
\description{
        This function simulates sample objects of class 'sample.rm' and estimates abundance and related parameters for each simulated sample object. 
}
\usage{
        point.sim.rm(pop.spec, survey.spec, design.spec, B = 99, numerical = TRUE, 
											plot = FALSE, show = FALSE, seed = NULL, ...)
}
\arguments{
\item{pop.spec}{population specification; either an object of class 'population' or 'pars.population'}
\item{survey.spec}{survey specification; an object of class 'pars.survey.rm'}
\item{design.spec}{design specification; an object of class design.rm}
\item{B}{number of simulations required}
\item{numerical}{ Argument in point.est.rm - Boolean flag: If TRUE the estimator will be calculated by maximising the  likelihood derived in derived in "Borchers, Buckland and Zucchini", equation 5.4. If FALSE the estimator will be calculated analytically. The analytic estimator is only available for exactly two survey occasions. }
\item{plot}{if TRUE, histograms and/or boxplots are produced, using estimates from the simulations, of individual abundance, group abundance and mean group size}
\item{show}{if TRUE displays the cumulative removal curve for each simulated survey as it is run}
\item{seed}{if TRUE displays the histograms of observations, and the fitted distribution function for each simulated survey as it is run}
\item{...}{extra plot arguments}
}
\details{
        This function simulates surveys by simulating from the observation model (using survey.spec) and if pop.spec is of class 'pars.population' from the state model (using pop.spec to generate new populations on each simulation).  
}
\value{
An object of class point.sim.ce with the following elements: 
  \item{A results matrix, each row of which contains the following values:}{}
    \item{Nhat.grp}{MLE of group abundance}
    \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
    \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}

  \item{true}{The true (simulated) values of group abundance, animal abundance and mean group size}
  \item{numerical}{Equal to the argument 'model.sel' passed to the function}
  \item{random.pop}{TRUE if population is randomised}
  \item{random.design}{TRUE if design is randomised}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}
}
\seealso{ 
        \code{\link{setpars.population}},   \code{\link{setpars.survey.rm}}
     	\code{\link{point.est.rm}}, 		\code{\link{set.seed}}
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

#simulate (population randomisation only): 
rm.sim<-point.sim.rm(pop.spec=rm.poppars, survey.spec=rm.survpars, design.spec=rm.des, B=99,
                     seed=NULL, plot=FALSE, show=TRUE)
summary(rm.sim)
plot(rm.sim)
plot(rm.sim, type="hist")
plot(rm.sim, type="box")
}
\keyword{}


\name{point.sim.crMh}
\alias{point.sim.crMh}
\title{Mark-Recapture Method Mh : Simulation}
\description{
        This function simulates samples and estimates abundance and  related parameters for each simulated sample using  
the mark-recpature model Mh of Pledger (2002). 
}
\usage{
        point.sim.crMh(pop.spec, survey.spec, design.spec, B = 99, init.N = -1, seed = NULL, num.mix = 2, plot=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{pop.spec}{population specification; either an object of class 'population' or 'pars.population'}
\item{survey.spec}{survey specification; an object of class 'pars.survey.cr'}
\item{design.spec}{design specification; an object of class 'design.cr'}
\item{B}{number of simulations required}
\item{init.N}{starting value of N used by the maximum likelihood opitimisation routine (see point.est.crMt)}
\item{seed}{Number passed to set.seed() to initialise random number generator}
\item{num.mix}{ number of mixtures of animal groups. }
\item{plot}{if TRUE, a histogram of the group abundance point estimates obtained from each sample is produced with true  
group abundance and the mean of the abundance estimates from the simulations displayed on the plot}
}
\details{
        This function simulates samples by simulating from the observation model (using survey.spec) and if pop.spec is  
of class 'pars.population' from the state model (using pop.spec to generate new populations on each simulation).  
}
\value{
An object of class point.sim.crMh, with the following elements:
  \item{est}{A results matrix, each row of which contains the following values:}
	\item{Nhat.grp}{MLE of group abundance}
	\item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
	\item{Es}{Estimate of mean group size (simple mean of observed group sizes)}
	\item{phat}{Estimate(s) of capture probability for the relevant model (try it and see)}
	\item{log.Likelihood}{Value of log-likelihood at MLE}
	\item{res.Deviance}{Residual deviance at MLE}
	\item{AIC}{Akaike's information criterion}

  \item{true}{The true (simulated) values of group abundance, animal abundance and mean group size}
  \item{init.N}{Equal to the argument 'init.N' passed to the function}
  \item{random.pop}{TRUE if population is randomised}
  \item{random.design}{TRUE if design is randomised}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}
 
}
\seealso{ 
        \code{\link{setpars.population}},   \code{\link{setpars.survey.cr}}
     	\code{\link{point.est.crMh}}, 		\code{\link{set.seed}}
}
\examples{
cr.reg<-generate.region(x.length=100, y.width=50)
cr.dens <- generate.density(cr.reg)

cr.poppars<-setpars.population(density.pop = cr.dens, number.groups = 100, size.method = "poisson",
                               size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                               exposure.min = 2, exposure.max = 10, exposure.mean = 3,
                               exposure.shape = 0.5,type.values = c("Male","Female"),
                               type.prob = c(0.48,0.52))
cr.pop<-generate.population(cr.poppars)

cr.des<-generate.design.cr(cr.reg, n.occ = 4)
cr.survpars<-setpars.survey.cr(cr.pop, cr.des, pmin.unmarked=0.2, pmax.unmarked=0.8, improvement=0.01)
cr.samp<-generate.sample.cr(cr.survpars)

#Randomise population and survey
cr.sim.Mh<-point.sim.crMh(pop.spec=cr.poppars, survey.spec=cr.survpars, design.spec=cr.des, B = 99)
summary(cr.sim.Mh)
plot(cr.sim.Mh)
plot(cr.sim.Mh, type="hist")
plot(cr.sim.Mh, type="box")

#Randomise survey only
cr.sim.Mh<- point.sim.crMh(pop.spec=cr.pop, survey.spec=cr.survpars, design.spec=cr.des, B = 99) 
summary(cr.sim.Mh)
plot(cr.sim.Mh)
}
\keyword{}


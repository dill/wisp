\name{point.sim.pt}
\alias{point.sim.pt}
\title{Point Transect Method : Simulation}
\description{
  This function simulates sample objects of class 'sample.pt' and estimates abundance and  related parameters for each simulated sample object.
}
\usage{
point.sim.lt(pop.spec, survey.spec, design.spec, B = 999, plot = FALSE, title = FALSE, conditional = TRUE, model = "half.normal", seed = NULL, show =  
FALSE, ...) 
}
\arguments{
  \item{pop.spec}{population specification; either an object of class 'population' or 'pars.population'}
  \item{survey.spec}{survey specification; an object of class 'pars.survey.pt'}
  \item{design.spec}{design specification; either an object of class 'design.pt' or 'pars.design.pt'}
  \item{B}{number of simulations required}
  \item{plot}{argument for point.est.pt - if TRUE, a density histogram of observed radial distances with the estimated probability density function  
superimposed, is plotted for each simulation}
\item{title}{  Flag telling if you want the title "Radial distance distribution and fitted detection function" }
  \item{conditional}{argument for point.est.pt - if FALSE, the full likelihood (Equation (7.32) of Borchers et al. 2002) is maximized; if TRUE the  
conditional likelihood (Equation (7.33) of Borchers et al. 2002) is maximized and abundance is estimated using Equation (7.34) of Borchers et al.  
2002.}
  \item{model}{argument for point.est.pt - ="half.normal" (no other detection function models have implemented yet)}
  \item{seed}{Number passed to set.seed() to initialise random number generator}
  \item{show}{if TRUE displays the histograms of observations, and the fitted distribution function for each simulated survey as it is run}  
  \item{...}{extra plot arguments}
}
\details{
  This function simulates sample objects of class 'sample.pt' by simulating from the observation model (using survey.spec) and if pop.spec is of class  
'pars.population' from the state model (using pop.spec to generate new populations on each simulation) and if design.spec is of class 'pars.design.pt'  
from the design (using design.spec to generate new design realizations on each simulation.)  
}
\value{
An object of class point.sim.lt with the following elements:
  A results matrix, each row of which contains the following values:
    \item{Nhat.grp}{MLE of group abundance}
    \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
    \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}

    \item{esa}{MLE effective survey area: 2piint_0^w x p(x),dx, where w is
          the trunction distance, and integration is  from 0 to w.}
    \item{nbar}{"encounter rate" - the number of groups detected per point}

  \item{true}{The true (simulated) values of group abundance, animal abundance and mean group size}
  \item{conditional}{Equal to the argument 'conditional' passed to the function}
  \item{random.pop}{TRUE if population is randomised}
  \item{random.design}{TRUE if design is randomised}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}  
}
\seealso{ 
     \code{\link{setpars.population}}, \code{\link{setpars.design.pt}}
     \code{\link{setpars.survey.pt}},  \code{\link{point.est.pt}}
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

pt.despars<-setpars.design.pt(pt.reg, n.transects=8, n.units=32, visual.range=3.5)
pt.des<-generate.design.pt(pt.despars)

pt.survpars<-setpars.survey.pt(pt.pop, pt.des, disthalf.min=2, disthalf.max=4)
pt.samp<-generate.sample.pt(pt.survpars)

# simulate (design and population randomisation):
pt.sim<-point.sim.pt(pop.spec=pt.poppars, design.spec=pt.despars, survey.spec=pt.survpars, B=9, seed=NULL, plot=TRUE)
summary(pt.sim)
plot(pt.sim)
plot(pt.sim, type="hist")
plot(pt.sim, type="box")

# simulate (design randomisation only):
pt.sim<-point.sim.pt(pop.spec=pt.pop, design.spec=pt.despars, survey.spec=pt.survpars, B=99, seed=NULL, plot=FALSE)
summary(pt.sim)
plot(pt.sim)
}
\keyword{}

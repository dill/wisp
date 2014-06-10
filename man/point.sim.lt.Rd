\name{point.sim.lt}
\alias{point.sim.lt}
\title{Line Transect Method : Simulation}
\description{
        This function simulates sample objects of class 'sample.lt' and estimates abundance and  related parameters for each simulated sample object.  
}
\usage{
        point.sim.lt(pop.spec, survey.spec, design.spec, B = 999, model.sel = FALSE, plot = FALSE, 
				title = FALSE, conditional = TRUE, model = "half.normal", seed = NULL, show = FALSE, ...) 
}
\arguments{
\item{pop.spec}{population specification; either an object of class 'population' or 'pars.population'}
\item{survey.spec}{survey specification; an object of class 'pars.survey.lt'}
\item{design.spec}{design specification; either an object of class 'design.lt' or 'pars.design.lt'}
\item{B}{number of simulations required}
\item{model.sel}{if TRUE, the half-normal and hazard rate models will be fitted to each simulation and the model chosen based on the lowest AIC value  
will be selected.}
\item{plot}{if TRUE, a density histogram of observed radial distances with the estimated probability density function superimposed, is plotted}
\item{title}{  Flag telling if you want the title "Perpendicular  distance distribution and fitted detection function" }
\item{conditional}{argument in point.est.lt - if FALSE, the full likelihood (Equation (7.10) of Borchers et al. 2002) is  maximized; if TRUE the  
conditional likelihood (Equation (7.8) of Borchers et al. 2002) is maximized  and abundance is estimated using Equation (7.12) of Borchers et al.  
2002.}
\item{model}{argument in point.est.lt - model: ="half.normal" or ="hazard.rate" for detection function form}
\item{plot}{ argument in point.est.lt - if TRUE, a density histogram of observed radial distances with the estimated probability density function  
superimposed, is plotted}
\item{seed}{Number passed to set.seed() to initialise random number generator}
\item{show}{if TRUE displays the histograms of observations, and the fitted distribution function for each simulated survey as it is run}  
\item{...}{extra plot arguments}
}
\details{
        This function simulates sample objects of class 'sample.lt' by simulating from the observation model (using survey.spec) and if pop.spec is of  
class 'pars.population' from the state model (using pop.spec to generate new populations on each simulation ) and if design.spec is of class  
'pars.design.lt' from the design (using design.spec to generate new design realisations on each simulation.)  
}
\value{
An object of class point.sim.lt with the following elements:{
  A results matrix, each row of which contains the following values:
    \item{Nhat.grp}{MLE of group abundance}
    \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
    \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}
	\item{theta}{MLE of detection function parameter}
    \item{mu}{MLE of effective strip half-width}
    \item{nL}{"encounter rate" - the number of groups detected per unit distance surveyed along lines}
    \item{log.likelihood}{the value of the log-likelihood function at the maximum}
    \item{AIC}{Akaike´s Information Criterion}
    \item{model}{the model chosen to fit the distribution of the data}

  \item{true}{The true (simulated) values of group abundance, animal abundance and mean group size}
  \item{model.sel}{Equal to the argument 'model.sel' passed to the function}
  \item{conditional}{Equal to the argument 'conditional' passed to the function}
  \item{model}{The model chosen to fit the distribution of the data}
  \item{random.pop}{TRUE if population is randomised}
  \item{random.design}{TRUE if design is randomised}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}}  
}
\seealso{ 
        \code{\link{setpars.population}},   \code{\link{setpars.design.lt}}
     	\code{\link{setpars.survey.lt}},	\code{\link{point.est.lt}}
	 	\code{\link{set.seed}}
}
\examples{
lt.reg <- generate.region(x.length = 100, y.width = 50)
lt.dens <- generate.density(lt.reg)

#heterogeneous population
lt.poppars<-setpars.population(density.pop = lt.dens, number.groups = 1000, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
lt.pop<-generate.population(lt.poppars)

lt.despars<-setpars.design.lt(lt.reg, n.transects=10, n.units=10, visual.range=4, percent.on.effort=1)
lt.des<-generate.design.lt(lt.despars, seed=3)

lt.survpars<-setpars.survey.lt(lt.pop, lt.des, disthalf.min=2, disthalf.max=4)
lt.samp<-generate.sample.lt(lt.survpars)

# simulate (design and population randomisation):
lt.sim<-point.sim.lt(pop.spec=lt.poppars, design.spec=lt.despars, survey.spec=lt.survpars, B=9,
                     seed=NULL, plot=FALSE, model.sel=TRUE)
summary(lt.sim)
plot(lt.sim)
plot(lt.sim, type="hist")
plot(lt.sim, type="box")

# simulate (design randomisation only) half normal:
lt.sim<-point.sim.lt(pop.spec=lt.pop, design.spec=lt.despars, survey.spec=lt.survpars, B=99,
                     seed=NULL, plot=FALSE)
summary(lt.sim)
plot(lt.sim)

# simulate (design randomization only) hazard rate:
lt.sim<-point.sim.lt(pop.spec=lt.pop, design.spec=lt.despars, survey.spec=lt.survpars, B=99,
                     seed=NULL, plot=FALSE, model="hazard.rate")
summary(lt.sim)
plot(lt.sim)

# simulate (design randomization only, automated model selection):
lt.sim<-point.sim.lt(pop.spec=lt.pop, design.spec=lt.despars, survey.spec=lt.survpars, B=99,
                     seed=NULL, plot=FALSE, model.sel=TRUE)
summary(lt.sim)
plot(lt.sim)
}
\keyword{}


\name{point.sim.pl}
\alias{point.sim.pl}
\title{Plot Sampling Method : Simulation}
\description{
        This function simulates surveys and estimates abundance and related parameters for each simulated survey. 
}
\usage{
        point.sim.pl(pop.spec, design.spec, B = 99, HT = FALSE, seed=NULL, show=FALSE, plot=FALSE)
}
\arguments{
\item{pop.spec}{population specification; either an object of class 'population' or 'pars.population'}
\item{design.spec}{design specification; either an object of class 'design.pl' or 'pars.design.no'}
\item{B}{number of simulations required}  
\item{HT}{argument in point.est.pl - HT: if FALSE, the abundance estimate produced is the MLE, if TRUE it is the Horvitz-Thompson  estimate.}
\item{seed}{Equal to the argument 'seed' passed to the function}
\item{show}{if TRUE displays the position of plots and the observations 'seen', for each simulated survey as it is run}
\item{plot}{if TRUE a histogram of the group abundance point estimates obtained from each sample object is produced with true group abundance and the mean of the simulated abundance estimates displayed on the plot}
}
\details{
        This function simulates surveys by simulating from the state model if pop.spec is of class 'pars.population' (using pop.spec to generate new populations on each simulation ) and if design.spec is of class 'pars.design.dp' from the design (using design.spec to generate new design realisations on each simulation.)  
}
\value{
An object of class point.sim.ce with the following elements:
  \item{A results matrix, each row of which contains the following values:}{}
  \item{Nhat.grp}{MLE of group abundance}
   \item{Nhat.ind}{MLE of individual abundance (= Nhat.grp * Es)}
   \item{Es}{Estimate of mean group size (simple mean of observed group sizes)}

  \item{true}{The true (simulated) values of group abundance, animal abundance and mean group size}
  \item{HT}{Equal to the argument 'model.sel' passed to the function}
  \item{random.pop}{TRUE if population is randomised}
  \item{random.design}{TRUE is design is randomised}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}  
}
\seealso{
        \code{\link{setpars.population}},   \code{\link{setpars.design.pl}}
     	\code{\link{point.est.pl}}, 		\code{\link{set.seed}}
}
\examples{
pl.reg <- generate.region(x.length = 100, y.width = 50)
pl.dens <- generate.density(pl.reg)
pl.poppars<-setpars.population(density.pop=pl.dens, number.groups = 100, size.method = "poisson",
                               size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                               exposure.min = 2, exposure.max = 10, exposure.mean = 6,
                               exposure.shape = 1)
pl.pop <- generate.population(pl.poppars, seed=456)
pl.despars<-setpars.design.pl(pl.reg, n.interval.x = 10, n.interval.y = 20,
                              method = "random", area.covered = 0.2)
pl.des <- generate.design.pl(pl.despars, seed=789)

pl.samp<-generate.sample.pl(pl.pop, pl.des, seed=101112)
plot(pl.samp, whole.population=TRUE)

# To simulate with fixed population and random design realizations:
pl.sim<-point.sim.pl(pop.spec=pl.pop, design.spec=pl.despars, B=99, show=TRUE)
summary(pl.sim)
plot(pl.sim)
plot(pl.sim, type="hist")
plot(pl.sim, type="box")

# To simulate with fixed design realization and random animal locations:
pl.sim<-point.sim.pl(pop.spec=pl.poppars, design.spec=pl.des, B=99,)
summary(pl.sim)
plot(pl.sim)

# To simulate with random population and random design realizations:
pl.sim<-point.sim.pl(pop.spec=pl.poppars, design.spec=pl.despars, B=99)
summary(pl.sim)
plot(pl.sim)
}
\keyword{}

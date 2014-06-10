\name{setpars.survey.cr}
\alias{setpars.survey.cr}
\title{Specifies Survey Design Parameters for a Mark-Recapture Survey.}
\description{
        This function stores the information needed to generate a survey design for a mark-recapture survey in an object of class `pars.design.cr´.
}
\usage{
        setpars.survey.cr(pop, des, pmin.unmarked, pmax.unmarked = pmin.unmarked, pmin.marked = pmin.unmarked, pmax.marked = pmax.unmarked, improvement = 0)
}
\arguments{
\item{pop}{object of class `population´.}
\item{des}{object of class `design.cr´.}
\item{pmin.unmarked}{expected proportion of the least detectable unmarked animals (those with lowest "exposure") that will be captured, using the lowest of the effort values in `design.cr'.}
\item{pmax.unmarked}{expected proportion of the most detectable unmarked animals (those with highest "exposure") that will be captured, using the lowest of the effort values in `design.cr'.}
\item{pmin.marked}{expected proportion of the least detectable marked animals (those with lowest "exposure") that will be captured, using the lowest of the effort values in `design.cr'.}
\item{pmax.marked}{expected proportion of the most detectable unmarked animals (those with highest "exposure") that will be captured, using the lowest of the effort values in `design.cr'.}
\item{improvement}{percentage improvement in detection probability between occasions for animals with mean "exposure".}
}
\details{
        This function calculates the parameters of the observation model represented by a capture function.

The most general form of capture function is:

p(exposure,mark,occasion)=1 - exp(-theta_0(mark) - theta_1(mark) * exposure - effort * (1 + theta2 * (occasion - 1)))

where \eqn{theta_0(mark)} and \eqn{theta_1(mark)} are parameters which are different for marked and unmarked animals.

The capture function parameters (the thetas) are calculated to be consistent with the arguments `pmin.unmarked´, `pmin.marked´, `pmax.unmarked´, `pmax.marked´ and `improvement´.  By modifying these arguments the user can control the complexity of the observation model.  This means concretely:

1.)\cr
If `pmax.unmarked´ = `pmin.unmarked´ then theta_1_unmarked = 0.
In this case the capture probabilities for unmarked animals will not depend on the exposure. That means that the observation model does not involve any heterogeneity resulting from different exposure values.  The detection probability will rather be the same for all unmarked animals.  If you do want to consider heterogeneity resulting from different exposures in the observation model, `pmax.unmarked´ has to be bigger than `pmin.unmarked´.

2.)\cr
If `pmax.marked´ = `pmin.marked´ then theta_1_marked = 0.
See description for unmarked animals under 1.). The only difference is that these arguments control if heterogeneity is considered for marked animals rather than for unmarked animals.

3.)\cr
If `pmin.unmarked´ = `pmin.marked´ and `pmax.unmarked´ = `pmax.marked´ then theta_0_unmarked = theta_0_marked and theta_1_unmarked = theta_1_marked.
In this case the observation model will not involve trap-shyness.  The capture probabilities will not depend on whether an animal is marked or unmarked.  If you do want to consider trap-shyness, the arguments for marked animals have to be different from those for unmarked animals.

4.)\cr
If `improvement´ = 0 then theta2 = 0.\cr
In this case the observation model will not involve any efficiency improvement over the survey occasions. The only variation of capture probabilities on each occasion will result from different catch-effort levels specified in \code{generate.design.cr}.
}
\value{ 
Returns an object of class `pars.survey.cr´ defining the survey parameters. This object can be passed to the function \code{generate.sample.cr} to generate actual survey data.  The object is a list, containing all or some of the following information (population, design, theta0.unmarked, theta0.marked, theta1.unmarked, theta1.marked, theta2, parents, created)
}
\seealso{
        \code{\link{generate.design.cr}},   \code{\link{generate.sample.cr}}
}
\examples{
cr.reg<-generate.region(x.length=100, y.width=50)
cr.dens <- generate.density(cr.reg)
cr.poppars<-setpars.population(density.pop = cr.dens, number.groups = 100, size.method = "poisson", 
						size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
						exposure.min = 2, exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5,  
						type.values = c("Male","Female"), type.prob = c(0.48,0.52))
cr.pop<-generate.population(cr.poppars)
cr.des<-generate.design.cr(cr.reg, n.occ = 4)

cr.survpars<-setpars.survey.cr(cr.pop, cr.des, pmin.unmarked=0.00001, pmax.unmarked=0.5, improvement=0.01)
summary(cr.survpars)
}
\keyword{}


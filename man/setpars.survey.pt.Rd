\name{setpars.survey.pt}
\alias{setpars.survey.pt}
\title{Survey Sample Parameters For A Point Transect Survey}
\description{
     This function stores the information needed to generate a survey design for a point transect survey in an object of class `pars.design.pt´.}
\usage{
     setpars.survey.pt(pop, des, disthalf.min, disthalf.max)
}
\arguments{
\item{pop}{object of class `population´.}
\item{des}{object of class `design.pt´.}
\item{disthalf.min}{radial distance at which detection probability is 0.5 for animals with minimum exposure.}
\item{disthalf.max}{radial distance at which detection probability is 0.5 for animals with maximum exposure.}
}
\details{

This function calculates the parameters of the half-normal point transect detection function that satisfies the requirement that detection probability  
is 0.5 at radial distance `disthalf.min' in the case of animals with minimal exposure, and detection probability is 0.5 at radial distance  
`disthalf.max' in the case of animals with maximal exposure. Detection probability is assumed to be 1 at radial distance zero and exposure affects  
only the scale parameter of the detection function. 

The implementation of the half-normal detection function has the following form:

p(distance,exposure)=exp(-0.5 * distance^2/(exp(theta_0 + theta_1 * exposure))^2)

By modifying the arguments `disthalf.min´ and `disthalf.max´ the user is able to
control the complexity of the observation model.

This means concretely:

1.)\cr
If `disthalf.max´ = `disthalf.min´ then theta_1 = 0.

In this case the detection probabilities will
not depend on the exposure. That means that the observation model does not involve
any heterogeneity resulting from different exposures of the animals. 
The detection probability will only depend on the distance.
If you do want to consider heterogeneity in the observation model, `disthalf.max´ has to be
bigger than `disthalf.min´.
}
\value{
Returns an object of class `pars.survey.pt´ which has to be passed on to
the function \code{generate.sample.pt} as a parameter.
}
\seealso{
     \code{\link{setpars.design.pt}}, \code{\link{generate.design.pt}}, \code{\link{generate.sample.pt}}, \code{\link{summary.sample.pt}},  
\code{\link{plot.sample.pt}}
}
\examples{
pt.reg <- generate.region(x.length = 100, y.width = 50)
pt.dens <- generate.density(pt.reg)

#heterogeneous population
pt.poppars<-setpars.population(density.pop = pt.dens, number.groups = 1000, size.method = "poisson", 
						size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta", 
						exposure.min = 0, exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
pt.pop<-generate.population(pt.poppars)

pt.despars<-setpars.design.pt(pt.reg, n.transects=8, n.units=32, visual.range=5)
pt.des<-generate.design.pt(pt.despars)
pt.survpars<-setpars.survey.pt(pt.pop, pt.des, disthalf.min=1, disthalf.max=2)
summary(pt.survpars)
plot(pt.survpars)
}
\keyword{}


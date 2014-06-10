\name{setpars.survey.lt}
\alias{setpars.survey.lt}
\title{Survey Sample Parameters For A Line Transect Survey}
\description{
        This function stores the information needed to generate a survey design for a line transect survey in an object of class `pars.design.lt´.
}
\usage{
        setpars.survey.lt(pop, des, disthalf.min, disthalf.max, model = "half.normal")
}
\arguments{
\item{pop}{object of class `population´.}
\item{des}{object of class `design.lt´.}
\item{disthalf.min}{distance perpendicular to the transect, at which detection probability is 0.5 for animals with minimum exposure.}
\item{disthalf.max}{distance perpendicular to the transect, at which detection probability is 0.5 for animals with maximum exposure.}
\item{model}{The model used to fit the detection function}
}
\details{
        This function calculates the parameters of the half-normal line transect detection function that satisfies the requirement that detection  
probability is 0.5 at perpendicular distance `disthalf.min' in the case of animals with minimal exposure, and detection probability is 0.5 at  
perpendicular distance `disthalf.max' in the case of animals with maximal exposure. Detection probability is assumed to be 1 at perpendicular distance  
zero and exposure affects only the scale parameter of the detection function. 
The implementation of the half-normal detection function has the following form:

p(distance,exposure)=exp(-0.5 * distance^2/(exp(theta_0 + theta_1 * exposure))^2)

By modifying the arguments `disthalf.min´ and `disthalf.max´ the user is able to control the complexity of the observation model.
This means concretely:

1.)\cr
If `disthalf.max´ = `disthalf.min´ then theta_1 = 0.
In this case the detection probabilities will not depend on the exposure. That means that the observation model does not involve any heterogeneity  
resulting from different exposures of the animals.  The detection probability will only depend on the distance.  If you do want to consider  
heterogeneity in the observation model, `disthalf.max´ has to be bigger than `disthalf.min´.
}
\value{
Returns an object of class `pars.survey.lt´ which is required by the function \code{generate.sample.lt} as a parameter.  The object is a list containing the following objects:
  \item{population}{The population the survey is carried out on}
  \item{design}{The design the survey follows}
  \item{theta}{A vector with the parameters theta 0 and theta 1}
  \item{model}{equal to the parameter 'model' passed to the function}
  \item{parents}{details of WiSP objects passed to the function}
  \item{created}{creation date and time}  
}
\seealso{
        \code{\link{setpars.design.lt}},   \code{\link{generate.design.lt}}
		\code{\link{generate.sample.lt}},  \code{\link{summary.sample.lt}}
		\code{\link{plot.sample.lt}}
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
summary(lt.survpars)
plot(lt.survpars)
}
\keyword{}


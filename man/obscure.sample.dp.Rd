\name{obscure.sample.dp}
\alias{obscure.sample.dp}
\title{Reducing a double platform line transect sample object to the observed data}
\description{
        The function filters out all the unobserved information from a `sample.dp' object, leaving only the observed data. It is useful when creating sample.dp objects for exercises -- when you don't want those doing the exercises to be able to see the whole population.
}
\usage{
        obscure(samp)
}
\arguments{
\item{sample}{Double platform line transect sample object (of class `sample.dp').}
}
\details{
        This function removes from the `sample.dp' object all data relating to animals and groups that were not detected -- those for which `(sample$detected==T & !is.na(sample$detected))'. 
}
\value{
\code{obscure.sample.dp} returns an object of class `sample.dp´ which has the following elements:
\item{population}{object of class `population´, but containing only data for observed members of the population.}
\item{design}{object of class `design.dp´.}
\item{detected.observer1}{vector indicating which animal groups have been detected by the first observer. \code{NA} means that an animal group was outside all survey units and could not be detected. \code{0} indicates that an animal group was inside a survey unit however has not been detected. \code{1} means that an animal group was inside a survey unit and has been detected. This object is redundant; all elements are \code{1}; it is retained purely to conform with the structure of `sample.dp' objects.}
\item{detected.observer2}{vector indicating which animal groups have been detected by the second observer. \code{NA} means that an animal group was outside all survey units and could not be detected. \code{0} indicates that an animal group was inside a survey unit however has not been detected. \code{1} means that an animal group was inside a survey unit and has been detected. This object is redundant; all elements are \code{1}; it is retained purely to conform with the structure of `sample.dp' objects.}
\item{distance}{vector of perpendicular distances of detected animal groups inside the survey units from the respective transect paths.}
\item{transect}{vector of transect path numbers of detected animal groups.}
}
\seealso{ 
        \code{\link{generate.sample.dp}},   \code{\link{setpars.survey.dp}}
        \code{\link{point.sim.dp}}
}
\examples{
reg<-generate.region(x.length = 50, y.width = 80)

dens <- generate.density(reg)
pop.pars<-setpars.population(reg, density.pop = dens, number.groups = 100, size.method = "poisson", 
					size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", exposure.min = 2, 
					exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pop<-generate.population(pop.pars)
dp.des.pars<-setpars.design.dp(reg, n.transects = 3, n.units = 6, visual.range = 2, percent.on.effort = 0.6)
dp.des<-generate.design.dp(dp.des.pars)
dp.surv.pars<-setpars.survey.dp(pop = pop, des = dp.des, adjust.interactive = FALSE, 
							theta.obs1 = 0.7, theta.obs2 = 0.4, theta.exp = 0.2, theta.dist = -0.7)
dp.samp<-generate.sample.dp(dp.surv.pars)

#  To view the complete sample object including all unobserved data:
dp.samp
#  To view the stripped down sample object containing only the observed data:
summary(dp.samp)
dp.samp.obs<-obscure(dp.samp)
summary(dp.samp.obs)
}
\keyword{}


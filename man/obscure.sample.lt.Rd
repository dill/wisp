\name{obscure.sample.lt}
\alias{obscure.sample.lt}
\title{Reducing a line transect sample object to the observed data}
\description{
        The function filters out all the unobserved information from a `sample.lt' object, leaving only the observed  
data. It is useful when creating sample.lt objects for exercises -- when you don't want those doing the exercises to be  
able to see the whole population.
}
\usage{
        obscure(sample)
}
\arguments{
\item{sample}{Line transect sample object (of class `sample.lt').}
}
\details{
        This function removes from the `sample.lt' object all data relating to animals and groups that were not  
detected -- those for which `(sample$detected==T & !is.na(sample$detected))'. 
}
\value{
 \code{obscure.sample.lt} returns an object of class `sample.lt´ which has the following elements:
  \item{population}{object of class `population´, but containing only data for observed members of the population.}
  \item{design}{object of class `design.lt´.}
  \item{detected}{vector indicating which animal groups have been detected. \code{NA} means that an animal group was  
outside all survey units and could not be detected. \code{0} indicates that an animal group was inside a survey unit  
however has not been detected. \code{1} means that an animal group was inside a survey unit and has been detected. This  
object is redundant; all elements are \code{1}; it is retained purely to conform with the structure of `sample.lt'  
objects.}
  \item{distance}{vector of perpendicular distances of detected animal groups inside the survey units from the  
respective transect paths.}
  \item{transect}{vector of transect path numbers of detected animal groups.}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}  
}
\seealso{
        \code{\link{generate.sample.lt}},   \code{\link{setpars.survey.lt}} 
		\code{\link{summary.sample.lt}}, 	\code{\link{plot.sample.lt}}
}
\examples{
reg<-generate.region(x.length = 50, y.width = 80)

dens <- generate.density(reg)
pop.pars<-setpars.population(reg, density.pop = dens, number.groups = 100, size.method = "poisson", size.min = 1,  
size.max = 5, size.mean = 1, exposure.method = "beta", exposure.min = 2, exposure.max = 10, exposure.mean = 6,  
exposure.shape = 1)
pop<-generate.population(pop.pars)

lt.des.pars<-setpars.design.lt(reg, n.transects = 4, n.units = 20, visual.range = 2, percent.on.effort = 0.7)
lt.des<-generate.design.lt(lt.des.pars)
lt.surv.pars<-setpars.survey.lt(pop, lt.des, disthalf.min = 1, disthalf.max = 2)
lt.samp<-generate.sample.lt(lt.surv.pars)

# now strip the unobserved data out of the sample.lt object:
lt.obs.samp<-obscure(lt.samp)
# (Note: `lt.obs.samp<-obscure.sample.lt(lt.samp)' has the same effect.)
plot(lt.obs.samp,whole.population=TRUE)
# note that `whole.population=T' has no effect - because all unobserved data is gone
summary(lt.obs.samp)
# ... but the summary is the same - because summary involves only the observed data
}
\keyword{}


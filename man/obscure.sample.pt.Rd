\name{obscure.sample.pt}
\alias{obscure.sample.pt}
\title{Reducing a point transect sample object to the observed data}
\description{
     The function filters out all the unobserved information from a `sample.pt' object, leaving only  
the observed data. It is useful when creating sample.pt objects for exercises -- when you don't want  
those doing the exercises to be able to see the whole population.
}
\usage{
obscure(sample)
obscure.sample.pt(sample)
}
\arguments{
\item{sample}{Point transect sample object (of class `sample.pt').}
}
\details{
     This function removes from the `sample.pt' object all data relating to animals and groups that  
were not detected -- those for which `(sample$detected!=T | !is.na(sample$detected))'. 
}
\value{
     \code{obscure.sample.pt} returns an object of class `sample.pt´ which has the following  
elements:

\item{population}{object of class `population´, but containing only data for observed members of the  
population.}

\item{design}{object of class `design.pt´.}

\item{detected}{vector indicating which animal groups have been detected. \code{NA} means that an  
animal group was outside all survey units and could not be detected. \code{0} indicates that an  
animal group was inside a survey unit however has not been detected. \code{1} means that an animal  
group was inside a survey unit and has been detected. This object is redundant; all elements are  
\code{1}; it is retained purely to conform with the structure of `sample.pt' objects.}

\item{distance}{vector of perpendicular distances of detected animal groups inside the survey units  
from the respective points.}

\item{transect}{vector of point numbers from which animal groups were detected.}
}
\seealso{
     \code{\link{generate.sample.pt}}, \code{\link{setpars.survey.pt}}, \code{\link{summary.sample.pt}},  
\code{\link{plot.sample.pt}}
}
\examples{
     # First generate a population and sample it:
     myreg<-generate.region(x.length = 50, y.width = 80)
     mydens <- generate.density()
     mypoppars<-setpars.population(myreg, density.pop = mydens, 
                        number.groups = 500, size.method = "poisson", 
                        size.min = 1, size.max = 5, size.mean = 1, 
                        exposure.method = "beta", exposure.min = 2,
                        exposure.max = 10, exposure.mean = 6, 
                        exposure.shape = 1)
     mypop<-generate.population(mypoppars)
     mydespars<-setpars.design.pt(myreg, n.transects = 4, n.units = 20, visual.range = 2.5)
     set.seed(1066)
     mydes<-generate.design.pt(mydespars)
     mysurvpars<-setpars.survey.pt(mypop, mydes, disthalf.min = 1.5, disthalf.max = 2)
     mysamp<-generate.sample.pt(mysurvpars)
     plot(mysamp,whole.population=T)
     summary(mysamp)
     # now strip the unobserved data out of the sample.pt object:
     obs.samp<-obscure(mysamp)
     plot(obs.samp,whole.population=T)
     # note that `whole.population=T' has no effect - because all unobserved data is gone
     summary(obs.samp)
     # ... but the summary is the same - because summary involves only the observed data
}
\keyword{}

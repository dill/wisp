\name{obscure.sample.no}
\alias{obscure.sample.no}
\title{Brief Survey Sample of Plot Method}
\description{
     The function filters out all the unobserved information from a `sample.rm' object,  
leaving only the observed data. It is useful when creating `sample.rm' objects for  
exercises -- when you don't want those doing the exercises to be able to see the whole  
population.
}
\usage{
obscure(sample)
obscure.sample.no(sample)
}
\arguments{
\item{sample}{Plot sampling method object, of class `sample.no´.}
}
\details{
     This function removes from the `sample.no' object all data relating to animals and  
groups that were not detected -- those for which `(sample$detected!=T |  
!is.na(sample$detected))'. Note that if sample contains information on nearest  
neighbours, data for these animals is also retained. 
}
\value{
     \code{obscure.sample.no} returns an object of class `sample.no´ which has 
     the following elements:

\item{population}{object of class `population´.}

\item{design}{object of class `design.no´.}

\item{detected}{vector indicating which animal groups have been detected. \code{0} 
          indicates that an animal group has not been detected, \code{1} means that 
          an animal group has been detected. }

\item{unit}{vector indicating survey units in which animal groups were detected.}
}
\seealso{
     \code{\link{generate.sample.no}},
     \code{\link{summary.sample.no}}, \code{\link{plot.sample.no}}
}
\examples{
     myreg<-generate.region(x.length = 80, y.width = 50)
     
     mydens <- generate.density()
     mypoppars<-setpars.population(myreg, density.pop = mydens, 
                        number.groups = 100, size.method = "poisson", 
                        size.min = 1, size.max = 5, size.mean = 1,     
                        exposure.method = "beta", exposure.min = 2, 
                        exposure.max = 10, exposure.mean = 6,
                        exposure.shape = 1)
     mypop<-generate.population(mypoppars)
     mydes <- generate.design.no(myreg, n.points = 20)
     set.seed(1843)
     mysamp<-generate.sample.no(mypop, mydes, with.neighbours=FALSE)
     myobscsamp<-obscure.sample.no(mysamp)
     
     # Note that whole.population=T has no effect on obscured sample:
     plot(mysamp, whole.population=T)
     plot(myobscsamp, whole.population=T)
     # ... but summary is the same:
     summary(mysamp)
     summary(myobscsamp)
}
\keyword{}

\name{generate.sample.no}
\alias{generate.sample.no}
\title{Survey Sample of Nearest Object Method}
\description{
This function uses the data stored in the `pars.survey.no´ object to create a  
point-to-nearest-object sample object.
}
\usage{
     generate.sample.no(pop, des, with.neighbours=FALSE)
}
\arguments{
\item{pop}{The population, an object returned by the function 
           \code{generate.population}}
\item{des}{The survey design, an object of class `design.no' that was returned by the 
	   function \code{generate.design.no}}
\item{with.neighbours}{If TRUE, the function also returns information on the nearest  
neighbour to the object closest to the sample point. }
}
\details{
 The calculation of the nearest neighbours (with.neighbours == TRUE) has the constraint
 that the line from design point to object and thence to neighbour is >90 degrees. 
 If a point has no neighbours satisfying the constraint, the neighbour identity
 is NA and the distance is 0.
}
\value{
Returns an object of class `sample.no´ which has the following elements:
\item{population}{The population being sampled (object of class `population')}
\item{design}{The design used (object of class `design.no')}
\item{detected}{Vector of length equal to the number of animals in the population. 0  
indicates the animal is not one nearest a sample point, 1 indicates it is. }
\item{point.animal}{The groupID of the animals closest to each of the points. For  
example, if the first element of this vector is 24 it means that the animal/group in the  
population with groupID 24 was the one closest to the first point; if the sectond element  
is 11 it means that the animal/group with groupID 11 was the one closest to the second  
point, and so on.}
\item{point.distance}{The distances from the survey points to the nearest animal; this  
vector is in the same order as `point.animal' above, so the first distance is that from  
the first point, the second is that from the second point, and so on.}
\item{neighbour.animal}{If `with.neighbours' was FALSE, this contains NAs, else it  
contains the groupID of the animal that was nearest the corresponding animal in  
`point.animal' (nearest neighbours).}
\item{neighbour.distance}{The distances from `neighbour.animal' to `point.animal'.}
}
\seealso{
    \code{\link{summary.sample.no}},
    \code{\link{plot.sample.no}}
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
     plot(mysamp)
#    and with neighbours:
     set.seed(1843)
     mysamp<-generate.sample.no(mypop, mydes, with.neighbours=TRUE)
     plot(mysamp)
#    show whole population:
     plot(mysamp, whole.population=TRUE)


}
\keyword{}


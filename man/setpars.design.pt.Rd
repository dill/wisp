\name{setpars.design.pt}
\alias{setpars.design.pt}
\title{Survey Design Parameters for a Point Transect Survey.}
\description{
     This function stores the information needed to generate a survey design for a point transect survey in an object of class `pars.design.pt´.}

\usage{
     setpars.design.pt(reg, n.transects = 1, n.units = 1, visual.range)
}
\arguments{
\item{reg}{object of class `region'.}
\item{n.transects}{number of transects. (The only point transect design currently implemented involves locating points at regular distances along  
"north-south" transects.)}
\item{n.units}{total number of survey points. So the number of survey points per
	       transect is equal to `n.units / n.transects´ . Therefor `n.units´ has to be a multiple
	       of `n.transects´}
\item{visual.range}{maximum distance from the point that is searched: the circular plot radius.}
}
\details{
     Survey design parameters for the point transect method are defined. The only point transect design currently implemented involves locating points  
at regular distances along "north-south" transect paths. The design is composed of a set of survey units (circular plots), centred on the points from  
which observations are made. These points lie at regular intervals along transect paths. The transects run parallel to the y-axis and the distances  
between them are the same in any one design. The location of the leftmost transect path is a (uniform) random variable, as is the distance along the  
path at which the first point is located. All circular plots are the same size.

This function specifies the "rules" to be used in generating a design, and returns them in an object of class `pars.design.pt'. It does not generate  
the design (which involves some randomization); this is done by passing the `pars.design.pt' object to `generate.design.pt', which introduces the  
random components. The same `pars.design.pt' object passed to \code{generate.design.pt} twice will result in different designs because of this  
randomness. 
}
\value{
Returns an object of class `pars.design.pt´ which can be passed to the function \code{generate.design.pt} as a parameter.
}
\seealso{
     \code{\link{generate.design.pt}}, \code{\link{plot.design.pt}}
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
summary(pt.despars)
}
\keyword{}



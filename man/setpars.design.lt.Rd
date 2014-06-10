\name{setpars.design.lt}
\alias{setpars.design.lt}
\title{Survey Design Parameters for a Line Transect Survey.}
\description{
        This function stores the information needed to generate a survey design for a line transect survey in an object of class `pars.design.lt´.
}
\usage{
        setpars.design.lt(reg, n.transects = 1, n.units = 1, visual.range, percent.on.effort = 1)
}
\arguments{
\item{reg}{object of class `region´.}
\item{n.transects}{number of transects. Each transect can contain more than one "unit"; this happens if some segments of the transect are not searched.}
\item{n.units}{number of survey units per transect that are searched; if the whole transect is searched this will be 1.}
\item{visual.range}{maximum distance from the path that is searched: the strip half-width.}
\item{percent.on.effort}{percentage of the transect length that is searched.}
}
\details{
        Survey design parameters for the line transect method are defined. The design is composed of a set of survey units (lines) that lie along transect paths. The path can consist of survey units and unsurveyed sections. The transects run parallel to the y-axis and the distances between them are the same in any one design. The location of the bottom left of the first path is a (bivariate uniform) random variable. All survey units are the same size and are the same distance apart along the paths.

        This function specifies the ``rules'' to be used in generating a design, and returns them in an object of class `pars.design.lt'. It does not generate the design (which involves some randomization); this is done by passing the `pars.design.lt' object to `generate.design.lt', which introduces the random components. The same `pars.design.lt' object passed to \code{generate.design.lt} twice will result in different designs because of this randomness. 
}
\value{
Returns an object of class `pars.design.lt´ which can be passed to the function \code{generate.design.lt} as a parameter, and contains the following objects:
  \item{region}{the region over which the survey takes place}
  \item{n/transects}{the number of transects}
  \item{n.units}{the number of survey units}
  \item{visual.range}{visibility of the observer}
  \item{percentage.on.effort}{percentage of the transect paths that is actually used for observation}
  \item{parents}{details of the WiSP objects passed to the function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{generate.design.lt}},   \code{\link{plot.design.lt}}
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
summary(lt.despars)
}
\keyword{}



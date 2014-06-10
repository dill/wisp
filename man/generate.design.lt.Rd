\name{generate.design.lt}
\alias{generate.design.lt}
\title{Line Transect Survey Design Specification}
\description{
        Generates a line transect survey design, using the parameters in a `pars.design.lt' object.
}
\usage{
        generate.design.lt(pars.design.lt, seed = NULL)}
\arguments{
\item{pars.design.lt}{a line transect design parameter object returned by the function \code{setpars.design.lt}}
\item{seed}{the number passed to set.seed() to initialise random number generator}
}
\details{
        Whereas `setpars.design.lt' sets up the rules that are used to generate a covered region for a line transect survey, this function introduces randomness and generates an actual covered region. The function \code{setpars.design.lt} must have been run before `generate.design.lt' is run.
}
\value{ 
\code{generate.design.lt} returns an object of class `design.lt´ which  has the following elements:
  \item{region}{object of class `region´.}
  \item{n.transects}{number of transects. Each transect can contain more than one "unit"; this happens if some segments of the transect are not searched.}
  \item{effective.n.transects}{the effective number of transects}
  \item{n.units}{number of survey units per transect that are searched; if the whole transect is searched this will be 1.}
  \item{pos.x}{vector of x-positions of  each survey unit.}
  \item{start.y}{vector of y-positions of the start of each survey unit. The direction in which the transects are traversed determines whether the start is the bottom or the top of the unit. The first transect is traversed starting from the bottom, the second from the top, the third from the bottom, and so on. You can see the directions using `plot(*,show.paths=T)', where * is an object of class `design.lt'.}
  \item{end.y}{vector of y-positions of the end of each survey unit. The direction in which the transects are traversed determines whether the start is the bottom or the top of the unit. The first transect is traversed starting from the bottom, the second from the top, the third from the bottom, and so on. You can see the directions using `plot(*,show.paths=T)', where * is an object of class `design.lt'.}
  \item{visual.range}{maximum distance from the path that is searched: the strip half-width.}
  \item{percentage.on.effort}{percentage of the transect lines that is searched}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}  
}
\seealso{
     \code{\link{setpars.design.lt}}, \code{\link{plot.design.lt}}
}
\examples{
lt.reg <- generate.region(x.length = 100, y.width = 50)
lt.dens <- generate.density(lt.reg)

#heterogeneous population
lt.poppars<-setpars.population(density.pop = lt.dens, number.groups = 1000, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
lt.pop<-generate.population(lt.poppars)

lt.despars<-setpars.design.lt(lt.reg, n.transects=10, n.units=10, visual.range=4,
                              percent.on.effort=1)
lt.des<-generate.design.lt(lt.despars, seed=3)
summary(lt.des)
}
\keyword{}


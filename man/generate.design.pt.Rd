\name{generate.design.pt}
\alias{generate.design.pt}
\title{Point Transect Survey Design Specification}
\description{
     Generates a point transect survey design, using the parameters in a `pars.design.pt' object.
}
\usage{
        generate.design.pt(pars.design.pt, seed = NULL)}
\arguments{
\item{pars.design.pt}{a line transect design parameter object returned by the function \code{setpars.design.pt}}
\item{seed}{the number passed to set.seed() to initialise random number generator}
}
\details{
     Whereas \code{setpars.design.pt} sets up the rules (in a `pars.design.pt' object) that are used to generate a covered region for a point transect survey, this function introduces randomness and generates an actual covered region. The function \code{setpars.design.pt} must have been run before \code{generate.design.pt} is run -- to generate the `pars.design.pt' object.
}
\value{
     \code{generate.design.pt} returns an object of class `design.pt´ which 
     has the following elements:

\item{region}{object of class `region´.}

\item{n.transects}{number of transects. (The only point transect design currently implemented involves locating points at regular distances along "north-south" transects.)}

\item{n.units}{number of survey points per transect.}

\item{units.x}{vector of x-coordinates of the points.}

\item{units.y}{vector of y-coordinates of the points.}

\item{visual.range}{maximum distance from the point that is searched, i.e. the circular plot radius.}
}
\seealso{
     \code{\link{setpars.design.pt}}, \code{\link{plot.design.pt}}
}
\examples{
pt.reg <- generate.region(x.length = 100, y.width = 50)
pt.dens <- generate.density(pt.reg)

#heterogeneous population
pt.poppars<-setpars.population(density.pop = pt.dens, number.groups = 1000, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
pt.pop<-generate.population(pt.poppars)

pt.despars<-setpars.design.pt(pt.reg, n.transects=8, n.units=32, visual.range=3.5)
pt.des<-generate.design.pt(pt.despars)
summary(pt.des)
plot(pt.des)
}
\keyword{}


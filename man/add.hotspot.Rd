\name{add.hotspot}
\alias{add.hotspot}
\title{Spots of Different Animal Population Density}
\description{
        Adds a spot of higher or lower density to a population density object.
}
\usage{
        add.hotspot(dens, x, y, altitude, sigma)
}
\arguments{
\item{dens}{object of class `density.population´}
\item{x}{x-coordinate of hotspot centre}
\item{y}{y-coordinate of hotspot centre}
\item{altitude}{height of hotspot}
\item{sigma}{width of hotspot}
}
\details{
        The function adds a specified two-dimensional Gaussian distribution to the given density object. The centre of the hotspot must be given in region coordinates.
The height can be negative. The width is defined by sigma, the standard deviation of the Gaussian distribution.
}
\value{
\code{add.hotspot} returns an object of class `density.population´ which has the following elements:
  \item{n.interval.x}{number of x-intervals.}
  \item{n.interval.y}{number of y-intervals.}
  \item{matrix}{the (modified) density matrix.}
}
\seealso{
        \code{\link{generate.region}} to create region objects
        \code{\link{generate.density}} to create density objects
        \code{\link{set.stripe}} to include stripes of different densities
}
\examples{
myreg <- generate.region(x.length =100, y.width = 50)
mydens <- generate.density(myreg,nint.x = 100, nint.y = 50, southwest = 1, southeast = 10, northwest = 20)

mydens<-add.hotspot(mydens, 20,10, 100,10)
plot(mydens)
plot(mydens, method="image")
}
\keyword{}

\name{set.stripe}
\alias{set.stripe}
\title{Stripes of Different Animal Population Density}
\description{
        Defines a stripe of a given density inside the population density object. 
}
\usage{
        set.stripe(dens, x1, y1, x2, y2, value = 0, width = 1)
}
\arguments{
\item{dens}{object of class `density.population´.}
\item{x1}{starting x-coordinate of stripe.}
\item{y1}{starting y-coordinate of stripe.}
\item{x2}{ending x-coordinate of stripe.}
\item{y2}{ending y-coordinate of stripe.}
\item{value}{density value inside stripe area.}
\item{width}{width of stripe. }
}
\details{
        The x- and y-coordinates can lie outside the specified region. However, the stripe must cross this region. 
Since a region is divided into rectangular cells by the density matrix it can occur that the stripe covers some cells only partially (e.g. a stripe that runs diagonally through the region). In this case, a cell will be included if its centre lies inside the stripe area, otherwise not.
}
\value{
\code{set.stripe} returns an object of class `density.population´ which has the following elements:
  \item{n.interval.x}{number of x-intervals.}
  \item{n.interval.y}{number of y-intervals.}
  \item{matrix}{the (modified) density matrix.}
}
\seealso{
        \code{\link{generate.region}} to create region objects
     	\code{\link{generate.density}} to create density objects
     	\code{\link{add.hotspot}} to include spots of different densities     }
\examples{
# set zero stripe
reg<-generate.region(x.length = 50, y.width = 80)
dens <- generate.density(reg, nint.x = 50, nint.y = 40, southwest = 1, southeast = 10, northwest = 20)
dens <- set.stripe(dens, x1 = 30, y1 = -10, x2 = 10, y2 = 90, width = 4)
plot(dens, method="image")

# set rectangle with constant density
reg<-generate.region(x.length = 50, y.width = 80)
dens <- generate.density(reg, nint.x = 50, nint.y = 40, southwest = 1, southeast = 10, northwest = 20)
dens <- set.stripe(dens, x1 = 33, y1 = 40, x2 = 38, y2 = 10, value = 20, width = 12)
plot(dens)
}
\keyword{}


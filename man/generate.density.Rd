\name{generate.density}
\alias{generate.density}
\title{Defining an Animal Population Density}
\description{
        The function creates a two dimensional density matrix which defines the distribution of an animal population inside a region.
}
\usage{
        generate.density(reg, nint.x = 100, nint.y = 100, southwest = 1, southeast = 1, northwest = 1)
}
\arguments{
\item{reg}{a region onto which the density is mapped}
\item{nint.x}{number of x-intervals in the density matrix.}
\item{nint.y}{number of y-intervals in the density matrix.}
\item{southwest}{density value at the south-western corner of the region.}
\item{southeast}{density value at the south-eastern corner of the region.}
\item{northwest}{density value at the north-western corner of the region.}
}
\details{
        This function places a `nint.x' by `nint.y' grid over the survey region; using linear interpolation and the three density values at `southwest', `southeast' and `northwest', it calculates relative densities at other grid points in the region. The resulting density matrix defines a relative density plane. Note that the density surface is relative; it is proportional to the density of animal groups but absolute density depends on the number of groups in the region (which is set using \code{setpars.population}). Higher resolution grids are created by increasing `nint.x' and/or `nint.y'. Stips or blocks of constant density can be added using the function \code{set.stripe}; bell-shaped density "bumps" or "dips" can be added using \code{add.hotspot}.}
\value{
An object of class `density.population´ which has the following elements:
\item{n.interval.x}{number of x-intervals}
\item{n.interval.y}{number of y-intervals}
\item{matrix}{the density matrix.}
}
\seealso{
        \code{\link{generate.region}}
		\code{\link{summary.density.population}}
		\code{\link{plot.density.population}}
		\code{\link{add.hotspot}}
		\code{\link{set.stripe}}
}
\examples{
reg <- generate.region(x.length = 50, y.width = 80)
dens <- generate.density(reg)
     
# image plot:
plot(dens, method = "image")

# 3-D wireframe plot:
plot(dens, eye.horiz=330, eye.vert=35)

# generate another density matrix with ascending density
reg <- generate.region(x.length = 50, y.width = 80)
dens <- generate.density(reg, nint.x = 50, nint.y = 40, southwest = 1, southeast = 10, northwest = 20)

# image plot:
plot(dens, method = "image")

# 3-D wireframe plot:
plot(dens, eye.horiz=330, eye.vert=35)
}
\keyword{}

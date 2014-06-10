\name{genden.data}
\alias{genden.data}
\title{ Create density surface from animal location data }
\description{
  If user possesses location data (from aerial photography, telemetry, etc.) of
  animal distribution within the study area, this information can be used to generate
  realizations of this population for subsequent simulation.
}
\usage{
genden.data(reg, xylocations, nint.x = 100, nint.y = 100, gam.param = 1)
}
\arguments{
	\item{reg}{a region onto which the density is mapped}
  \item{xylocations}{ Data frame consisting of only x and y columns.  These
	are the animal locations.  Note that range of x- and y-values must fall within
	the boundaries specified by the argument \code{reg} }
	\item{nint.x}{number of x-intervals in the density matrix.}
	\item{nint.y}{number of y-intervals in the density matrix.}
  \item{gam.param}{ Gamma parameter in gam smooth; lower this value <1 to cause a more 'patchy'
	       fit to your data; a value >1.5 will cause a higher degree of smoothness to the surface }
}
\details{
	For users wishing to be able to more realistically simulate the population of interest to them,
	this function has been created to allow density surfaces that more closely approximate the
	dispersion of animals within a study area.  If location data are available, a data frame consisting
	of the x- and y-coordinates can be used as data for a simple fitting exercise.
	
	This function is intended as a substitute for \code{add.hotspot} and \code{set.stripe} when data
	about animal locations are available.
}
\value{
An object of class `density.population´ which has the following elements:
\item{n.interval.x}{number of x-intervals}
\item{n.interval.y}{number of y-intervals}
\item{matrix}{the density matrix.}
}
\references{ }
\author{ Eric Rexstad, RUWPA, Univ. of St. Andrews. }
\note{  
  The method by which the density surface is a simple smooth of the location data, using only
  the x- and y- coordinates as predictors.  This is done using gams (general additive models)
  from the \code{mgcv} package.  Consult that package for more details regarding fitting smooths.
}
\seealso{ \code{\link{add.hotspot}}, \code{\link{set.stripe}}, \code{\link{run.FShake3D}} }
\examples{
	library(wisp)
	data(beach); data(seal.pop)
#			Borrow animal locations from the 'seal.pop' dataset, and fit a gam to those locations	
	gam.seal <- genden.data(beach, xylocations=data.frame(x=seal.pop$posx, y=seal.pop$posy), 
						  gam.param=0.7)
	plot.density.population(gam.seal, method='image')
}	
\keyword{ }

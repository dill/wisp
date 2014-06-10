\name{run.FShake3D}
\alias{run.FShake3D}
\alias{sample.FShake3D}
\alias{launch.FShake3D}
\alias{dnorm2d.FShake3D}
\alias{sampleDialog.FShake3D}
\alias{densityDialog.FShake3D}

\title{ Create density surface with mouse manipulations }
\description{
  By pulling on nodes throughout the study region, the user can create a density
  surface from which the simulated animal population is generated with the use
  of a mouse.  This can make creation of a complex density surface much more rapid
  than repeated use of \code{add.hotspot} and \code{set.stripe}.
}
\usage{
run.FShake3D(xlim = c(-1, 1), ylim = c(-1, 1), nx = 10, ny = 10)
}
\arguments{
  \item{xlim}{ Limits of study region in x-dimension }
  \item{ylim}{ Limits of study region in y-dimension }
  \item{nx}{ Number of manipulable nodes along the x-dimension }
  \item{ny}{ Number of manipulable nodes along the y-dimension }
}
\details{
  Makes call to C++ code to perform the actual surface manipulation.
}
\value{
An object of class `density.population´ which has the following elements:
\item{n.interval.x}{number of x-intervals}
\item{n.interval.y}{number of y-intervals}
\item{matrix}{the density matrix.}
}
\author{ Daniel Adler }
\note{ ~~further notes~~ 

}
\seealso{ \code{\link{add.hotspot}}, \code{\link{set.stripe}} }
\examples{
#		Because function uses the mouse, can only be done when interactive
if (interactive()) {
	run.FShake3D(xlim = c(-1, 1), ylim = c(-1, 1), nx = 10, ny = 10)
	}
}
\keyword{ }

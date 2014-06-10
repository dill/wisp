\name{plot.density.population}
\alias{plot.density.population}
\title{Default Plotting Statement for Animal Population Density}
\description{
        This function generates a graphical representation of the specified `density.population´ object.
}
\usage{
        plot.density.population(dens, method = "persp", eye.horiz = 330, eye.vert = 35, ...)
}
\arguments{
\item{dens}{object of class `density.population´.}
\item{method}{used plot method. Possible methods are
                *  \code{persp} for a three dimensional figure,
             	*  \code{image} for a two dimensional plot where different colours represent different densities.}
\item{eye.horiz}{horizontal plot rotation (in degrees).  Only used with "persp"}
\item{eye.vert}{vertical plot rotation (in degrees).  Only used with "persp"}
\item{...}{extra plot arguments}
}
\seealso{
        \code{\link{generate.region}},   \code{\link{generate.density}}
        \code{\link{summary.density.population}}
}
\value{
a list containing the following elements
  \item{reg}{study region object}
  \item{n.interval.x}{the number of units of along the x axis.  Equal to the number 'nint.x', passed to the \code{\link{generate.density}} function}
  \item{n.interval.y}{the number of units of along the y axis.  Equal to the number 'nint.y', passed to the \code{\link{generate.density}} function}
  \item{matrix}{A matrix of the densities in the area.  N.B. It is very large}
  \item{parents}{details of the WiSP objects passed to the function}
  \item{created}{The date and time the object was created}
}
\examples{
reg<-generate.region(x.length = 50, y.width = 80)
     
dens <- generate.density(reg, nint.x = 50, nint.y = 40, southwest = 1, southeast = 10, northwest = 20)
plot(dens, method = "persp")       			# show perspective plot
plot(dens, method = "image")       			# show image plot

# plot with  different vertical and horizontal rotation
plot(dens, method = "persp", eye.horiz= 30, eye.vert = 10)	   
}
\keyword{}


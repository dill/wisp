\name{plot.region}
\alias{plot.region}
\title{Default Plotting Statement For A Survey Region}
\description{
        This function generates a graphical representation of the specified `region´ object.
}
\usage{
        plot(reg, reset.pars = TRUE)
}
\arguments{
\item{reg}{object of class `region´ generated with \code{generate.region} function.}
\item{reset.pars}{The region is always plotted with the correct aspect ratio.  If reset.pars=TRUE, graphical parameters are restored to what they were before this function, for the next plot.  If you're going to plot something in the region (e.g. group locations) set reset.pars=FALSE and the coordinate systems of the region and the thing subsequently plotted will then match}
}
\seealso{
        \code{\link{generate.region}} to create region objects
}
\examples{
reg<-generate.region(x.length=100, y.width=60)
plot(reg)
}
\keyword{}


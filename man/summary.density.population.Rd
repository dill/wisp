\name{summary.density.population}
\alias{summary.density.population}
\title{Default Summary Statement for Animal Population Density}
\description{
        This function summarises the content of a `density.population´ object. 
}
\usage{
        summary(dens)
}
\arguments{
\item{dens}{an object of class `density.population´.}
}
\seealso{
        \code{\link{generate.density}}
  	\code{\link{plot.density.population}}
}
\examples{
reg <- generate.region(x.length = 100, y.width = 50)
dens <- generate.density(reg, nint.x = 5, nint.y = 4, southwest = 1, southeast = 10, northwest = 20)
summary(dens)
}
\keyword{}


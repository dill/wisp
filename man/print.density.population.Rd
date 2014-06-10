\name{print.density.population}
\alias{print.density.population}
\title{Default Print Statement for Animal Population Density}
\description{
        This function prints the content of a `density.population´ object. 
}
\usage{
        print.density.population(dens)
}
\arguments{
\item{dens}{object of class `density.population´.}
}
\seealso{
        \code{\link{generate.density}}
     	\code{\link{summary.density.population}}, \code{\link{plot.density.population}}
}
\examples{
reg <- generate.region(x.length = 100, y.width = 50)
dens <- generate.density(reg, nint.x = 7, nint.y = 4, southwest = 1, southeast = 10, northwest = 20)
print(dens)
summary(dens)
}
\keyword{}


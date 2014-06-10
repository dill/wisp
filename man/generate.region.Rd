\name{generate.region}
\alias{generate.region}
\title{Defining a Survey Region}
\description{
        This function creates a new survey region
}
\usage{
        generate.region(x.length = 100, y.width = 100, habitat = matrix(0, 1, 1))
}
\arguments{
\item{x.length}{length of region}
\item{y.width}{width of region}
\item{habitat}{matrix with habitat entries for the region (NOT FULLY IMPLEMENTED)}
}
\details{
}     
\value{
     \code{generate.region} returns an object of class `region´ which has the following elements:
  \item{x.length}{length of region.}
  \item{y.width}{width of region.}
  \item{habitat}{matrix with habitat entries for the region. (Use of this matrix has not yet been implemented in other WiSP functions.)}
}
\seealso{     
}
\examples{
reg <- generate.region(x.length = 50, y.width = 80)
summary(reg)
plot(reg)
}
\keyword{}


\name{generate.design.no}
\alias{generate.design.no}
\title{Survey Design of Nearest Object Method}
\description{
Generates a point-to-nearest-object survey design for the point-to-nearest-object method.
}
\usage{
generate.design.no(reg, n.points=2)
}
\arguments{
\item{reg}{object of class `region' generated with \code{generate.region} function}
\item{n.points}{number of points in the sample}
}
\details{
Chooses n.points in such a way that all points in the survey region are equally likelly to be chosen  
and each chosen point is independent of the location of other chosen points. 
}
\value{
Returns an object of class `design.no´ which has the following elements:
\item{region}{object of class `region´}
\item{n.points}{number of sampled points within the region}
\item{x}{the x-coordinates of the points}
\item{y}{the y-coordinates of the points}
}
\seealso{
     \code{\link{generate.region}}
     \code{\link{generate.sample.no}}
}
\examples{
     myreg<- generate.region(x.length = 80, y.width = 50)
     mydes <- generate.design.no(myreg, n.points = 20)
     plot(mydes)
}
\keyword{}


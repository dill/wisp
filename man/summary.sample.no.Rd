\name{summary.sample.no}
\alias{summary.sample.no}
\title{Default Summary Statement for Survey Sample of Nearest-Object Method}
\description{
     This function summarises the content of a `sample.no´ object, generated
     with \code{generate.sample.no} function.
}
\usage{
     summary(samp)
}
\arguments{
\item{samp}{object of class `sample.no´, generated with \code{generate.sample.no}
            function.}
}
\seealso{
     \code{\link{generate.sample.no}} 
     \code{\link{plot.sample.no}}
}
\examples{
     myreg<-generate.region(x.length = 80, y.width = 50)
          
     mydens <- generate.density()
     mypoppars<-setpars.population(myreg, density.pop = mydens, 
                        number.groups = 100, size.method = "poisson", 
                        size.min = 1, size.max = 5, size.mean = 1,     
                        exposure.method = "beta", exposure.min = 2, 
                        exposure.max = 10, exposure.mean = 6,
                        exposure.shape = 1)
     mypop<-generate.population(mypoppars)
     mydes <- generate.design.no(myreg, n.points = 20)
     mysamp<-generate.sample.no(mypop, mydes, with.neighbours=T)
     
     summary(mysamp)
}
\keyword{}


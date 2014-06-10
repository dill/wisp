\name{summary.population}
\alias{summary.population}
\title{Default Summary Statement for an Animal Population }
\description{
        This function summarises the content of a `population´ object.
}
\usage{
        summary.population(pop, plot=FALSE, digits=5, ...)
}
\arguments{
\item{pop}{object of class `population´.}
\item{plot}{if TRUE the population will be plotted}
\item{digits}{the number of significant figures that numbers will be given to}
\item{...}{extra plot arguments}
}
\seealso{
        \code{\link{setpars.population}},   \code{\link{generate.population}} 
     	\code{\link{plot.population}}
}
\examples{
reg<-generate.region(x.length = 50, y.width = 80)
     
dens <- generate.density(reg)
pop.pars<-setpars.population(reg, density.pop = dens, number.groups = 100, size.method = "poisson", 
					size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
					exposure.min = 2, exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pop<-generate.population(pop.pars)

summary(pop)
}
\keyword{}


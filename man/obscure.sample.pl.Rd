\name{obscure.sample.pl}
\alias{obscure.sample.pl}
\title{Brief Survey Sample of Plot Method}
\description{
        The function filters out all the unobserved information from a `sample.pl' object, leaving only the observed data. It is useful when creating `sample.pl' objects for exercises -- when you don't want those doing the exercises to be able to see the whole population.
}
\usage{
        obscure(sample)
}
\arguments{
\item{samp}{Plot sampling method object, of class `sample.pl´.}
}
\details{
        This function removes from the `sample.pl' object all data relating to animals and groups that were not detected -- those for which `(sample$detected!=T | !is.na(sample$detected))'. 
}
\value{
\code{obscure.sample.pl} returns an object of class `sample.pl´ which has the following elements:
  \item{population}{object of class `population´.}
  \item{design}{object of class `design.pl´.}
  \item{detected}{vector indicating which animal groups have been detected. \code{0} indicates that an animal group has not been detected, \code{1} means that an animal group has been detected. This object is redundant; all elements are \code{1}; it is retained purely to conform with the structure of `sample.pl' objects.}
  \item{unit}{vector indicating survey units in which animal groups were detected.}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{generate.sample.pl}},   \code{\link{summary.sample.pl}}
		\code{\link{plot.sample.pl}}
}
\examples{
reg <- generate.region(x.length = 50, y.width = 80)

dens <- generate.density(reg)
pop.pars<-setpars.population(reg, density.pop = dens, number.groups = 100, size.method = "poisson", 
					size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", exposure.min = 2, 
					exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pop <- generate.population(pop.pars)

pl.des.pars<-setpars.design.pl(reg, n.interval.x = 5, n.interval.y = 6, method = "regular", area.covered = 0.18, jitter = "unsync")
pl.des <- generate.design.pl(pl.des.pars)
pl.samp <- generate.sample.pl(pop, pl.des)

obs.pl.samp<-obscure.sample.pl(pl.samp)
# Note that whole.population=T has no effect on obscured sample:
plot(pl.samp, whole.population=T)
plot(obs.pl.samp, whole.population=T)
# ... but summary is the same:
summary(pl.samp)
summary(obs.pl.samp)
}
\keyword{}


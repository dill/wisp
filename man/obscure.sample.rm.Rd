\name{obscure.sample.rm}
\alias{obscure.sample.rm}
\title{Reducing a catch-effort sample object to the observed data}
\description{
        The function filters out all the unobserved information from a `sample.rm' object, leaving only the observed data. It is useful when creating `sample.rm' objects for exercises -- when you don't want those doing the exercises to be able to see the whole population.
}
\usage{
        obscure(sample)
}
\arguments{
\item{sample}{sample object of class `sample.rm'}
}
\details{
        This function removes from the `sample.ce' object all data relating to animals and groups that were not detected -- those for which `(sample$detected!=T | !is.na(sample$detected))'. 
}
\value{
\code{obscure.sample.rm} returns an object of class `sample.rm´ which has the following elements:
  \item{population}{object of class `population´.}
  \item{design}{object of class `design.rm´.}
  \item{removal}{two dimensional detection matrix that indicates which animal groups have been captured on which occasion. A \code{1} in row i, column j indicates that the i-th animal/group was removed the occasion before j; a \code{1} means that it was not. This object is redundant; all elements are \code{1}; it is retained purely to conform with the structure of `sample.rm' objects.}
  \item{detected}{two dimensional detection matrix that indicates which animal groups have been captured on which occasion.  A \code{1} in row i, column k indicated that the i-th animal/group was detected and removed on occasion k.}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{generate.sample.rm}},   \code{\link{setpars.survey.rm}}
		\code{\link{summary.sample.rm}}, 	\code{\link{plot.sample.rm}}
}
\examples{
reg<-generate.region(x.length = 50, y.width = 80)

den <- generate.density(reg)
pop.pars<-setpars.population(reg, density.pop = den, number.groups = 100, size.method = "poisson", 
					size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", exposure.min = 2, 
					exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pop<-generate.population(pop.pars)
rm.des <- generate.design.rm(reg, n.occ = 4, effort = c(4,5,6,7))
rm.survpars<-setpars.survey.rm(pop = pop, des = rm.des,  pmin=0.03, pmax=0.95, improvement = 0.1)
rm.samp<-generate.sample.rm(rm.survpars)
     
# now strip the unobserved data out of the sample.ce object:
obs.rm.samp<-obscure.sample.rm(rm.samp)
# (Note: `obs.samp<-obscure(rm.samp)' has the same effect.)
plot(obs.rm.samp)
# (Note: the plot will only contain observations, so 'whole.population=T' has no effect)
summary(obs.rm.samp)
# (Note:... although the summary will be the same, as it only contains observations)
}
\keyword{}


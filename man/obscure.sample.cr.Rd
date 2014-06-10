\name{obscure.sample.cr}
\alias{obscure.sample.cr}
\title{Reducing a mark-recapture method sample object to the observed data}
\description{
        The function filters out all the unobserved information from a `sample.cr' object, leaving only the observed data. It is useful when creating `sample.cr' objects for exercises -- when you don't want those doing the exercises to be able to see the whole population.
}
\usage{
        obscure(sample)
}
\arguments{
\item{sample}{Mark-recapture sample object (of class `sample.cr').}
}
\details{
        This function removes from the `sample.cr' object all data relating to animals and groups that were not detected -- those for which `(sample$detected!=T | !is.na(sample$detected))'. 
}
\value{
\code{obscure.sample.ce} returns an object of class `sample.ce´ which has the following elements:
  \item{population}{object of class `population´.}
  \item{design}{object of class `design.ce´.}
  \item{capture}{two dimensional detection matrix that indicates which animal groups have been captured on which occasion. A \code{1} in row i, column j indicates that the i-th animal/group was captured on occasion j; a \code{1} means that it was not. This object is redundant; all elements are \code{1}; it is retained purely to conform with the structure of `sample.cr' objects.}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{generate.sample.cr}},   \code{\link{setpars.survey.cr}}, 
		\code{\link{summary.sample.cr}}, 	\code{\link{plot.sample.cr}}
}
\examples{
reg<-generate.region(x.length = 50, y.width = 80)

den <- generate.density(reg)
pop.pars<-setpars.population(reg, density.pop = den, number.groups = 100, size.method = "poisson",
                             size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                             exposure.min = 2, exposure.max = 10, exposure.mean = 6,
                             exposure.shape = 1)
pop<-generate.population(pop.pars)
     
cr.des<-generate.design.cr(reg, n.occ = 5)
cr.survpars<-setpars.survey.cr(pop, cr.des, pmin.unmarked = 0.01, pmax.unmarked = 0.03,
                               pmin.marked = 0.005, pmax.marked = 0.008, improvement = 0.06)
cr.samp<-generate.sample.cr(cr.survpars)
     
# now strip the unobserved data out of the sample.cr object:
obs.cr.samp<-obscure(cr.samp)
# (Note: `obs.cr.samp<-obscure.sample.cr(cr.samp)' has the same effect.)
plot(obs.cr.samp,whole.population=T)
# note that `whole.population=T' has no effect - because all unobserved data is gone
summary(obs.cr.samp)
# ... but the summary is the same - because summary involves only the observed data
}
\keyword{}


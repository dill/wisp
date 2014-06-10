\name{make.twosex.pop}
\alias{make.twosex.pop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create population comprised of animals of 2 types (sexes) }
\description{
  A generic population is created that consists of 2 types of individuals.  There is no
  explicit spatial organization to the population.
}
\usage{
make.twosex.pop(abund = 100, prop.male = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{abund}{ Specify the number of individuals (in groups of size=1) in the population }
  \item{prop.male}{ Proportion of the generated population that is male }
}
\details{
  This function has no provision for setting dispersion, group size, nor exposure heterogeneity for the
  population begin generated.  This function is intended to be used in conjunction with
  \code{\link{two.samp.cir}} and its derivatives to examine the behavior of the change-in-ratio
  (CIR) estimator.
}
\value{
  Population object as described in \code{\link{setpars.population}}
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }

\section{Warning }{More versatility in the attributes of the population created
can be obtained by explicit calls to the functions that are called
by \code{make.twosex.pop}, i.e., \code{\link{setpars.population}}.
}

\seealso{ \code{\link{two.samp.cir}}, \code{\link{sim.cir.2}}, \code{\link{setpars.population}} }
\examples{
library(wisp)
example <- make.twosex.pop(abund=200, prop.male=0.4)
plot(example, title="Population constructed using make.twosex.pop")
}
\keyword{ htest }% at least one, from doc/KEYWORDS

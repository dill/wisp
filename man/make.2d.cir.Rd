\name{make.2d.cir}
\alias{make.2d.cir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Examines consequence of differing pre- and post-harvest change-in-ratio (CIR) sampling schemes }
\description{
  When designing a CIR experiment, effort needs to be allocated not only to measuring the
  magnitude of the harvest, but also to estimating the population sex ratio prior to, and following
  the harvest.  This function examines combinations of different sampling intensities pre- and post-harvest.
}
\usage{
make.2d.cir(pop, top.samp = 0.5, samp.incr = 0.1, male.harv = 0.5, fem.harv = 0.05, reps = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pop}{ Population object being harvested and sampled (likely created by \code{\link{make.twosex.pop}}) }
  \item{top.samp}{ Greatest proportion of the population sampled pre- and post-harvest for estimation of sex ratio }
  \item{samp.incr}{ Step size to be used to iterate through the sampling proportions (both pre- and post-haravest) }
  \item{male.harv}{ Proportion of males in the population removed during the harvest }
  \item{fem.harv}{ Proportion of females in the population removed during the harvest }
  \item{reps}{ Number of simulations to perform at each combination of pre- and post-harvest sampling values. }
}
\details{
  This function is a wrapper, that repeatedly calls \code{\link{sim.cir.2.summary}}.  Exercise some caution
  in the number of replicates requested; this can considerably increase the computational burden.  Similarly, having
  a \code{samp.incr} vanishingly small, will also lead to long waits for results.  Reasonable results can be derived
  from \code{reps=500} and \code{samp.incr=0.02}.
}
\value{
The resulting data frame consists of
  \item{pre-harvest sampling fraction}{Sampling fraction for pre-harvest}
  \item{post-harvest sampling fraction}{Sampling fraction for post-harvest}
  \item{mean point estimate for sampling combination}{Mean of the \code{reps} point estimates of abundance}
  \item{mean standard error for sampling combination}{Mean of the \code{reps} standard errors of estimates of abundance}
  \item{mean coefficient of variation for sampling combination}{Mean of the \code{reps} coefficients of variation of estimates of abundance}
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ To produce contour or heat plots of the resulting data frame, the column of interest must be converted back
to a matrix, e.g., \code{result.mat <- matrix(result$point, nrow=5, ncol=5)}.  }

\section{Caution }{The resulting data frame is comprised of means, calculated after missing values (inadmissable
values) have been eliminated.  Because distributions of estimates from CIR estimators can be quite skewed, it
is worth considering the possibility of examining medians.}

\seealso{ \code{\link{sim.cir.2.summary}}, \code{\link{two.samp.cir}}, and \code{\link{make.twosex.pop}} }
\examples{

example.pop <- make.twosex.pop(abund=300, prop.male=0.4)
example.result <- make.2d.cir(pop=example.pop, top.samp=0.5, samp.incr=0.1, male.harv=0.5,
                  fem.harv=0.05, reps=100)
#  Note, interest here is actually in precision of estimate, not in bias, hence
#        matrix is created of CV rather than of point
example.result.mat <- matrix(example.result$cv, nrow=5, ncol=5) # note 5 unique values for pre- and post-
contour(x=seq(0.1, 0.5, by=0.1), y=seq(0.1, 0.5, by=0.1), z=example.result.mat,
        main="Example output from function make.2d.cir")
#
#  Dont run
#       unless you have installed the rgl library
#
 library(rgl)
 mat2 <- pmin(example.result.mat, 2)    # Truncate maximum value to a CV of 2
 collut <- terrain.colors(200)          # make the plot more colorful
 col2 <- collut[mat2*100]
 rgl.surface(x=10*(seq(.1,.5, by=.1)),z=10*(seq(.1,.5,by=.1)),mat2,color=col2)
 rgl.bbox()
 rgl.texts(x=2.5,z=2.5,y=1,"CV for values of p-pre and p-post", justify="center")  # label the surface

}
\keyword{ htest }% __ONLY ONE__ keyword per line

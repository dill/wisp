\name{run.sexratio.sims}
\alias{run.sexratio.sims}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Examines the performance of change-in-ratio (CIR) estimator for populations
of differing sex ratios. }
\description{
 Populations with skewed sex ratios may arise from past selective harvest pressure.
 This function allows users to specify a sex ratio extant in a population, selectively
 harvest that population with changing differential harvest pressure among the sexes, and
 produce a graph of the result.
}
\usage{
run.sexratio.sims(abund=100, ratio=0.40, nreps=500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{abund}{ Size of the population being simulated; i.e., 'true' value of population size }
  \item{ratio}{ Sex-ratio in the population being simulated }
  \item{nreps}{ Number of simulations for each level of delta-P (difference between male and female
                harvest rates) }
}
\details{
  This function is a wrapper function for \code{\link{make.twosex.pop}} and \code{\link{sim.cir.2}}.
}
\value{
The graph produced depicts the median estimate of abundance along with the 5th and 95th
quantile of the replicated CIR (removing missing values) for each level of underlying sex
ratio in the population and difference in exploitation pressure between males and females (between 0.0
and 0.55). The proportion of simulations at each level of delta-P is shown above the 95th quantile on
the resulting graph.  In addition, the true population size is show as a horizontal blue line.
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5
\url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ If you create large populations (>500), this function can take a considerable time to run.  This
is because 25,000 estimates of abundance are generated when this function is called.  Each estimate
necessitates the simulation of animals being sampled, harvested, and resampled.  Feel free to examine
the performance of the estimator when populations are large, simply recognize the result may take some
time to appear. }

\section{Warning }{Two facets of the simulation experiment are 'hard-wired' and cannot
be altered by the user in calls to the function:
a) Range of difference in exploitation pressure (fixed at 0.6 for males, and varying between
0.0 and 0.55 for females), and
b) upper and lower bounds for the confidence interval (set at 0.05 and 0.95).
}

\seealso{ \code{\link{make.twosex.pop}} and \code{\link{sim.cir.2}} }
\examples{

run.sexratio.sims(abund=500, ratio=0.3, nreps=500)

}
\keyword{ htest }% __ONLY ONE__ keyword per line

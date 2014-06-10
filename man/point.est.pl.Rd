\name{point.est.pl}
\alias{point.est.pl}
\title{Plot Sampling Method Abundance Estimation: Point Estimate}
\description{
        This function estimates abundance and related parameters from a plot sampling method sample object (of class `sample.pl').
}
\usage{
        point.est.pl(samp, HT=FALSE)
}
\arguments{
\item{samp}{object of class `sample.pl´.}
\item{HT}{if FALSE, the abundance estimate produced is the MLE, if TRUE it is the Horvitz-Thompson estimate.}
}
\details{
        The MLE (given immediately after Equation (4.3) in Borchers et al. 2002) is 
          
		Nhat = n / pc, where
     
        n  = number of detected animals\cr
        pc = percentage of area covered
     
The Horvitz-Thompson estimator is given in Equation (4.1) of Borchers et al. 2002:
     
        Nhat = sum (n_j / p_j), where
     
        n_j = number of animals detected in plot j
        p_j = pc_j * J, with pc_j = percentage of area covered by plot j and J = number of plots
     
NOTE: In the current implementation the percentage of the area covered by any plot j is the same for all j (all plots have equal size) so that the Horvitz-Thompson estimator is equal to the ML estimator.     
}
\value{ 
An object of class `point.est.pl´ containing the following items:
  \item{sample}{The sample used in estimation (an object of class 'sample.pl')}
  \item{Nhat.grp}{Either MLE or HT estimator of group abundance}
  \item{Nhat.ind}{Either MLE or HT estimator of individual abundance (= Nhat.grp * Es)}
  \item{Es}{mean group size}
  \item{HT}{Equal to the argument 'HT' passed to the function}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
  \item{seed}{Equal to the argument 'seed' passed to the function}  
}
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations. Springer. London. 314pp.
}
\seealso{
        \code{\link{generate.sample.pl}},   \code{\link{int.est.pl}},
     	\code{\link{summary.sample.pl}}, 	\code{\link{plot.sample.pl}}      
}
\examples{
pl.reg <- generate.region(x.length = 100, y.width = 50)
pl.dens <- generate.density(pl.reg)
pl.poppars<-setpars.population(density.pop=pl.dens, number.groups = 100, size.method = "poisson", 
						size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
						exposure.min = 2, exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pl.pop <- generate.population(pl.poppars, seed=456)
pl.despars<-setpars.design.pl(pl.reg, n.interval.x = 10, n.interval.y = 20,method = "random", area.covered = 0.2)

pl.des <- generate.design.pl(pl.despars, seed=789)
pl.samp<-generate.sample.pl(pl.pop, pl.des, seed=101112)

pl.est<-point.est.pl(pl.samp)
summary(pl.est)

#With Horvitz-Thompson estimator
HT.pl.est<-point.est.pl(pl.samp, HT=TRUE)
summary(HT.pl.est)
}
\keyword{}


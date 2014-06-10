\name{int.est.no}
\alias{int.est.no}
\title{Point-to-Nearest-Object Method Abundance Estimation: Interval Estimate}
\description{
     Calculates parametric or non-parametric bootstrap confidence intervals of abundance for the  
point-to-nearest-object method.
}
\usage{
    int.est.no(samp, vlevels = c(0.025, 0.975), ci.type = "boot.nonpar", 
    nboot = 999, plot = T, ...)
}
\arguments{
\item{samp}{object of class `sample.no´, generated with function 
	    \code{generate.sample.no}}
\item{vlevels}{vector of percentage levels for confidence intervals.}
\item{ci.type}{method for constructing the confidence interval. Possible methods are

             *  `boot.par´ for using parametric bootstrap, 

             *  `boot.nonpar´ for using nonparametric bootstrap.
}
\item{nboot}{number of bootstrap replications.}
\item{plot}{if TRUE the bootstrap distribution of the estimator of N is plotted 
            using kernel smoothing. The confidence interval will also be
            plotted.}
\item{...}{additional plot parameters}
}
\details{
The parametric bootstrap generates point-to-nearest-object parametrically from the distribution of  
these distances that applies under the assumption that animal groups are uniformly distributed in  
the survey region. The nonparametric bootstrap resamples with replacement from the observed  
distances. The confidence interval is calculated using the percentile method.
}
\value{
An object of class `int.est.no´ containing the following items:

\item{levels}{percentage levels for confidence interval}
\item{ci}{the confidence interval}
\item{boot.mean}{mean of bootstrap estimates}
\item{boot.dbn}{full set of nboot bootstrap estimates.}
}

\seealso{
     \code{\link{point.est.no}},
     \code{\link{generate.sample.no}} to create sample objects,
     \code{\link{summary.sample.no}},
     \code{\link{plot.sample.no}}      
}
\examples{
     
     # generating all necessary objects
     myreg<-generate.region(x.length = 50, y.width = 80)
          
     mydens <- generate.density()
     mypoppars<-setpars.population(myreg, density.pop = mydens, 
                          number.groups = 100, size.method = "poisson", 
                          size.min = 1, size.max = 5, size.mean = 1,     
                          exposure.method = "beta", exposure.min = 2, 
                          exposure.max = 10, exposure.mean = 6,
                          exposure.shape = 1)
     mypop<-generate.population(mypoppars)
     mydes <- generate.design.no(myreg, n.points = 4)
     mysamp<-generate.sample.no(mypop, mydes, with.neighbours=T)
          
     # nonparametric bootstrap
     my.int.est<-int.est.no(mysamp, ci.type="boot.nonpar")
     
     # parametric bootstrap
     my.int.est<-int.est.no(mysamp, ci.type="boot.par")
     
}
\keyword{}


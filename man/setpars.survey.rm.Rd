\name{setpars.survey.rm}
\alias{setpars.survey.rm}
\title{Survey Sample Parameters For A General Removal Method Survey}
\description{
        This function stores the information needed to define a survey sample of the change in ratio method in a `pars.survey.ce´ object.
}
\usage{
        setpars.survey.rm(pop, des, pmin, pmax = pmin, 	improvement=0)
}
\arguments{
\item{pop}{object of class `population´.}
\item{des}{object of class `design.rm´.}
\item{pmin}{expected proportion of the least detectable animal groups (those with lowest "exposure") that will be captured using the lowest of the effort values in `design.rm'.}
\item{pmax}{expected proportion of the most detectable animal groups (those with highest "exposure") that will be captured using the lowest of the effort values in `design.rm'.}
\item{improvement}{percentage improvement in detection probability from first to second occasion for animals with mean exposure. }
}
\details{
        Capture probabilities are modelled as a hazard rate function:
     
        p(detect) =  1 - exp{-(theta0+theta1*exposure)*l_s*(1+theta2*(s-1))}
where
theta0: is the intercept parameter,
theta1: is the parameter determining the effect of animal exposure,
theta2: is the parameter determining the `improvement´ in efficiency over occasions,
s  : is the number of the survey occasion in question, and
l_s: is the effort on survey occasion s,

The detection function parameters (the thetas) are calculated to be consistent with the arguments `pmin´, `pmax´ and `improvement´. 
By modifying these arguments the user can control the complexity of the observation model.

This means concretely:

1.)\cr
If `pmax´ = `pmin´ then theta1 = 0.
In this case the detection probabilities will not depend on the exposure. That means that the observation model does not involve any heterogeneity but the detection probability will be the same for all animals.  If you do want to consider heterogeneity in the observation model, `pmax´ has to be bigger than `pmin´.

2.)\cr
If `improvement´ = 0 then theta2 = 0.
In this case the observation model will not involve any efficiency improvement over the survey occasions. The only variation of detection probabilities on each occasion will result from different removal levels specified in \code{generate.design.rm}.
}
\value{
Returns an object of class `pars.survey.rm´ defining the survey parameters. This object can be passed on to the function \code{generate.sample.rm} as a parameter.
}
\seealso{
        \code{\link{setpars.population}},   \code{\link{generate.population}} 
     	\code{\link{generate.design.rm}},   \code{\link{generate.sample.rm}}
		\code{\link{summary.sample.rm}}, 	\code{\link{plot.sample.rm}}
}
\examples{
rm.reg<-generate.region(x.length=100, y.width=50)
rm.dens <- generate.density(rm.reg)
rm.poppars<-setpars.population(density.pop = rm.dens, number.groups = 100, size.method = "poisson",  
						size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
						exposure.min = 2, exposure.max = 10, exposure.mean = 3, exposure.shape = 0.5, 
						type.values=c("Male","Female"), type.prob=c(0.48,0.52))
rm.pop<-generate.population(rm.poppars)
rm.des<-generate.design.rm(rm.reg, n.occ = 5, effort=c(1,2,3,2,1))

rm.survpars<-setpars.survey.rm(pop=rm.pop, des=rm.des, pmin=0.03, pmax=0.95, improvement=0.05)
summary(rm.survpars)
}
\keyword{}


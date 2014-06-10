\name{int.est.pt}
\alias{int.est.pt}
\title{Point Transect Method Abundance Estimation: Interval Estimate}
\description{
  This function estimates a confidence interval for group abundance for the point transect method.
}
\usage{
     int.est.pt(sampl, ci.type = "boot.nonpar", nboot = 999, vlevels = c(0.025, 0.975), 
		 conditional = TRUE, model = "half.normal", plot = TRUE, show.all = FALSE, seed = NULL, ...)
}
\arguments{
\item{sampl}{object of class `sample.lt´.}
\item{ci.type}{="boot.nonpar" (no other methods implemented yet)}
\item{nboot}{number bootstrap replicates}
\item{vlevels}{percentage levels for confidence iterval}
\item{conditional}{=T for conditional likelihood; =F for full likelihood}
\item{model}{="half.normal" or "hazard.rate"}
\item{plot}{=T if you want a plot of boostrap distribution of Nhat.grp}
\item{plot.all.fits}{=F if you want to see the fit for every bootstrap sample as it is fitted.}
\item{...}{other plot arguments}
}
\value{
A object of class `int.est.pt´ containing the following items:

\item{levels}{percentage levels for confidence interval}
\item{ci}{the confidence interval}
\item{boot.mean}{mean of bootstrap estimates}
\item{boot.dbn}{a list with components "Nhat.grp", "Nhat.ind",
		"theta", "ese", "nbar" and "Es" (see \code{point.est.pt})
		each containing the full set of nboot estimates 
		of that parameter. (This is useful if you want to 
		create a plot of the bootstrap distribution of the 
		estimates and control bin width etc.)}
}
\details{
 The nonparametric bootstrap method resamples transects with replacement (each resample has the same number of transects as were in the original  
sampele). (Note that it is the transects that are resampled, not the points). Details of the method are given in Borchers et al. (2002), p146 (general  
method for line transect and point transect methods) and p156 (some specifics about point transect bootstrap methods).
}
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations. Springer. London. 314pp.
}
\seealso{
     \code{\link{setpars.survey.pt}}, \code{\link{generate.sample.pt}} 
     \code{\link{point.est.pt}}
}
\examples{
pt.reg <- generate.region(x.length = 100, y.width = 50)
pt.dens <- generate.density(pt.reg)

#heterogeneous population
pt.poppars<-setpars.population(density.pop = pt.dens, number.groups = 1000, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
pt.pop<-generate.population(pt.poppars)

pt.despars<-setpars.design.pt(pt.reg, n.transects=8, n.units=32, visual.range=3.5)
pt.des<-generate.design.pt(pt.despars)

pt.survpars<-setpars.survey.pt(pt.pop, pt.des, disthalf.min=2, disthalf.max=4)
pt.samp<-generate.sample.pt(pt.survpars)

# simulate (design and population randomisation):
pt.sim<-point.sim.pt(pop.spec=pt.poppars, design.spec=pt.despars, survey.spec=pt.survpars, B=9, seed=NULL, plot=TRUE)
summary(pt.sim)
plot(pt.sim)
plot(pt.sim, type="hist")
plot(pt.sim, type="box")

# simulate (design randomisation only):
pt.sim<-point.sim.pt(pop.spec=pt.pop, design.spec=pt.despars, survey.spec=pt.survpars, B=99, seed=NULL, plot=FALSE)
summary(pt.sim)
plot(pt.sim)
}
\keyword{}


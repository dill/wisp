\name{plot.sample.rm}
\alias{plot.sample.rm}
\title{Default Plotting Statement for Removal Method Sample Object}
\description{
        This function generates a graphical representation of the specified `sample.rm´ object.
}
\usage{ 
        plot(samp, which.occasion = 0, show.sizes = TRUE, show.exps = TRUE, dsf = 1, whole.population = FALSE)
}
\arguments{
\item{samp}{object of class `sample.rm´.}
\item{which.occasion}{specifies the survey occasion from which catches are shown in red in the output. If this is anything other than 0, captures from other occasions are shown in blue.} 
\item{show.sizes}{if TRUE, different animal group sizes are shown as dots of different sizes.}
\item{show.exps}{if TRUE different group exposures are shown in different shading.}
\item{dsf}{enlargement factor for dots.}
\item{whole.population}{if TRUE detected animal groups will be shown in red; undetected animal groups will be black.}
}
\details{
        This function shows which animals/groups were captured on a catch-effort survey. It shows a picture of the region and indicates the detected animal groups using red dots.
If \code{wich.occasion} is set 0 the removals from all occasions are shown.
}
\seealso{
		\code{\link{setpars.survey.rm}},   \code{\link{generate.sample.rm}} 			
		\code{\link{summary.sample.rm}}
}
\examples{
rm.reg<-generate.region(x.length=100, y.width=50)
rm.dens <- generate.density(rm.reg)

rm.poppars<-setpars.population(density.pop = rm.dens, number.groups = 100, size.method = "poisson",
                               size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                               exposure.min = 2, exposure.max = 10, exposure.mean = 3,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
rm.pop<-generate.population(rm.poppars)
rm.des<-generate.design.rm(rm.reg, n.occ = 5, effort=c(1,2,3,2,1))
rm.survpars<-setpars.survey.rm(pop=rm.pop, des=rm.des, pmin=0.03, pmax=0.95, improvement=0.05)
rm.samp<-generate.sample.rm(rm.survpars)

plot(rm.samp,whole.population=TRUE) 
}
\keyword{}


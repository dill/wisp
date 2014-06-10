\name{plot.sample.cr}
\alias{plot.sample.cr}
\title{Default Plotting Statement for Mark-Recapture Sample Object.}
\description{
        This function generates a graphical representation of the specified `sample.cr´ object.
}
\usage{
        plot(samp, which.occasion = 0, show.sizes = TRUE, show.exps = TRUE, dsf = 0.75,
             whole.population = FALSE, type = "hist")
}
\arguments{
\item{samp}{object of class `sample.cr´.}
\item{which.occasion}{specifies the survey occasion from which catches are shown in the output.} 
\item{show.sizes}{if TRUE, different animal group sizes are shown as dots of different sizes.}
\item{show.exps}{if TRUE different group exposures are shown in different shading.}
\item{dsf}{enlargement factor for group dots.}
\item{whole.population}{if TRUE undetected animal groups will be black, as well as the detected animal groups in red}
\item{type}{if "locations", the locations of the animals in the survey region is show, elese plots of capute histories are shown}
}
\details{
        This function shows which animals/groups were captured on a mark-recatpure survey. It shows a picture of the region and indicates the detected animal groups using red dots.  If \code{which.occasion} is set 0 the removals from all occasions are shown.
}
\seealso{
        \code{\link{setpars.survey.cr}},   \code{\link{generate.sample.cr}} 
		\code{\link{summary.sample.cr}}
}
\examples{
cr.reg<-generate.region(x.length=100, y.width=50)
cr.dens <- generate.density(cr.reg)

cr.poppars<-setpars.population(density.pop = cr.dens, number.groups = 100, size.method = "poisson",
                               size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                               exposure.min = 2, exposure.max = 10, exposure.mean = 3,
                               exposure.shape = 0.5, type.values = c("Male","Female"),
                               type.prob = c(0.48,0.52))
cr.pop<-generate.population(cr.poppars)

cr.des<-generate.design.cr(cr.reg, n.occ = 4)
cr.survpars<-setpars.survey.cr(cr.pop, cr.des, pmin.unmarked=0.00001, pmax.unmarked=0.5, improvement=0.01)
cr.samp<-generate.sample.cr(cr.survpars)

plot(cr.samp)
plot(cr.samp,type="locations", whole.population=TRUE, which.occ=0)
plot(cr.samp,type="locations", whole.population=TRUE, which.occ=1)
plot(cr.samp,type="locations", whole.population=TRUE, which.occ=2)
}
\keyword{}


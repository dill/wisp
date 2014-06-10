\name{plot.sample.lt}
\alias{plot.sample.lt}
\title{Default Plotting Statement for Survey Sample of Line Transect Method}
\description{
        This function generates a graphical representation of the specified `sample.lt´ object.
}
\usage{
        plot(samp, type = "hist", show.sizes = TRUE, show.exps = TRUE, dsf = 0.5,
             whole.population = FALSE, show.paths = TRUE, ...)
}
\arguments{
\item{samp}{object of class `sample.lt´.}
\item{type}{Denotes the type of plot that is shown.  
		* If "locations", the group locations in the region is shown, with the survey area shaded in, and each observation indicated by a red dot. 
		* else, a histogram of number of observations against perpendicular distance from transect is plotted.}
\item{show.sizes}{if TRUE different animal group sizes are shown as dots of different sizes.}
\item{show.exps}{if TRUE different group exposures are shown in different shading.}
\item{dsf}{enlargement factor for group dots.}
\item{whole.population}{if TRUE undetected animal groups will be shown in black, as well as detected animal groups shown in red.  TRUE is only possible if type="region".}
\item{show.paths}{if TRUE the transect paths will be added as arrows pointing into the direction of the current transect.}
}
\details{
        This function displays the result of a survey sample. It shows a picture of the region, the survey units and indicates the detected animal groups as red dots.}
\seealso{
        \code{\link{setpars.survey.lt}},   \code{\link{generate.sample.lt}}
     	\code{\link{summary.sample.lt}}
}
\examples{
lt.reg <- generate.region(x.length = 100, y.width = 50)
lt.dens <- generate.density(lt.reg)

#heterogeneous population
lt.poppars<-setpars.population(density.pop = lt.dens, number.groups = 1000, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
lt.pop<-generate.population(lt.poppars)

lt.despars<-setpars.design.lt(lt.reg, n.transects=10, n.units=10, visual.range=4, percent.on.effort=1)
lt.des<-generate.design.lt(lt.despars, seed=3)
lt.survpars<-setpars.survey.lt(lt.pop, lt.des, disthalf.min=2, disthalf.max=4)
lt.samp<-generate.sample.lt(lt.survpars)

plot(lt.samp)
plot(lt.samp, type="locations", dsf=0.25)
}
\keyword{}


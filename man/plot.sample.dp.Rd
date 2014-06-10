\name{plot.sample.dp}
\alias{plot.sample.dp}
\title{Default Plotting Statement for Survey Sample of Double Platform Method}
\description{
        This function generates a graphical representation of the specified `sample.dp´ object.
}
\usage{
        plot.sample.dp(samp, type="hist", show.sizes = TRUE, show.exps = TRUE, dsf = 1,
                       whole.population = FALSE, show.paths = FALSE, show.legend = FALSE)
}
\arguments{
\item{samp}{object of class `sample.pt´.}
\item{type}{Denotes the type of plot that is shown.  
        * If "locations", the group locations in the region is shown, with the survey area shaded in, and each observation indicated by a red dot. 
		* else, histograms of observations against perpendicular distance from transect are plotted.}
\item{show.sizes}{if TRUE different animal group sizes are shown as dots of different sizes.}
\item{show.exps}{if TRUE different group exposures are shown in different shading.}
\item{dsf}{enlargement factor for animal group dots.}
\item{whole.population}{if TRUE undetected animal groups will be shown in black, as well as detected animal groups shown in red.}
\item{show.paths}{if true the transect paths will be added as arrows pointing into the direction of the current transect.}
\item{show.legend}{if true a legend that provides explanations to the plot is shown.}
}
\details{
        This function displays the result of a survey sample. It shows a picture of the region and the survey units which include the detected animal groups. 
In order to differentiate which animal groups have been detected by which observer different colours are used Detections of observer 1 are coloured yellow, detections of observer 2 are coloured blue, animal groups that have been detected by both observers are coloured red.}
\seealso{
        \code{\link{setpars.survey.dp}},   \code{\link{generate.sample.dp}} 
     	\code{\link{summary.sample.dp}}
}
\examples{
dp.reg <- generate.region(x.length = 100, y.width = 50)
dp.dens <- generate.density(dp.reg)

#heterogeneous population
dp.poppars<-setpars.population(density.pop = dp.dens, number.groups = 1000, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5, type.values=c("Male","Female"),
                               type.prob=c(0.48,0.52))
dp.pop<-generate.population(dp.poppars)

dp.despars<-setpars.design.dp(dp.reg, n.transects=10, n.units=10, visual.range=2, percent.on.effort=1)
dp.des<-generate.design.dp(dp.despars, seed=3)
dp.survpars<-setpars.survey.dp(dp.pop, dp.des, adjust.interactive=FALSE, theta.obs1=0.35,
                               theta.obs2=0, theta.exp=3, theta.dist=-2)
dp.samp<-generate.sample.dp(dp.survpars)

plot(dp.samp)
plot(dp.samp, type="locations")
plot(dp.samp, type="locations", show.legend=FALSE)
}
\keyword{}


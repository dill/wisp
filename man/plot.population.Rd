\name{plot.population}
\alias{plot.population}
\title{Default Plotting Statement for an Animal Population }
\description{
        This function generates a graphical representation of the specified `population´ object.
}
\usage{
        plot.population(pop, show.sizes = TRUE, show.exps = TRUE, dsf = 0.75, group.col = "black",
                        type="details", title = "Group locations")
}
\arguments{
\item{pop}{object of class `population´.}
\item{show.sizes}{if true different animal group sizes are shown as dots of different sizes.}
\item{show.exps}{if true different group exposures are shown in different shading.}
\item{dsf}{enlargement factor for group dots.}
\item{group.col}{colour for group dots.}
\item{type}{If "details" plots group size distribution, exposure distribution and locations.  If "locations" shows a large plot of group locations.}
\item{title}{the title given to the population plot}
}
\seealso{
        \code{\link{setpars.population}},   \code{\link{generate.population}}
     	\code{\link{summary.population}}
}
\examples{
reg<-generate.region(x.length = 50, y.width = 80)
     
dens <- generate.density(reg)
pop.pars<-setpars.population(reg, density.pop = dens, number.groups = 100, size.method = "poisson",
                             size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                             exposure.min = 2, exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pop<-generate.population(pop.pars)

#Try the following one at a time to see the options available
plot(pop)                           	# default plot
plot(pop, type="locations")             # only plots group locations
plot(pop, show.sizes = FALSE)        	# ignore group size values
plot(pop, show.exps = FALSE)           	# ignore group exposure values
plot(pop, dsf = 0.5)                	# decrease dot sizes
}
\keyword{}


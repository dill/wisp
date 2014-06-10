\name{plot.sample.pl}
\alias{plot.sample.pl}
\title{Default Plotting Statement for Survey Sample of Plot Method}
\description{
        This function generates a graphical representation of the specified `sample.pl´ object.
}
\usage{
        plot(samp, show.sizes = TRUE, show.exps = TRUE, dsf = 0.75, whole.population = FALSE)
}
\arguments{
\item{samp}{object of class `sample.pl´.}
\item{show.sizes}{if TRUE different animal group sizes are shown as dots of different sizes.}
\item{show.exps}{if TRUE different group exposures are shown in different shading.}
\item{dsf}{enlargement factor for group dots.}
\item{whole.population}{if TRUE undetected animal groups will be shown in black, as well as detected animal groups shown in red}
}
\details{
        This function displays the result of a survey sample. It shows a picture of the region, the survey units and indicates the detected animal groups as red dots.}
\seealso{
        \code{\link{generate.sample.pl}},   \code{\link{summary.sample.pl}}
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

plot(pl.samp)
plot(pl.samp, whole.population=TRUE)
}
\keyword{}


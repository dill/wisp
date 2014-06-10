\name{setpars.design.pl}
\alias{setpars.design.pl}
\title{Specifies Survey Design Parameters for a Plot Sampling Survey.}
\description{
        This function stores the information needed to generate a survey design for a plot survey in an object of class `pars.design.pl´.
}
\usage{
        setpars.design.pl(reg, n.interval.x = 10, n.interval.y = 10, method = "random", area.covered = 0.05, jitter = "sync")
}
\arguments{
\item{reg}{object of class `region´.}
\item{n.interval.x}{number of intervals into which the x-axis is to be divided for placing plot sampling units.}
\item{n.interval.y}{number of intervals into which the y-axis is to be divided for placing plot sampling units.}
\item{method}{method of positioning plot sampling units in the region. Possible methods are:
             *  `random´ for selecting units from the `n.interval.x' \eqn{\times}{*} `n.interval.y' possible units with equal probability and without replacement (simple random sampling);
             *  `regular´ for a systematic survey with one unit in each of the `n.interval.x' \eqn{\times}{*} `n.interval.y' grid rectangles, with `area.covered' percentage of the survey region sampled; `jitter' determines how units are located within the grid rectangles (see below).}
\item{area.covered}{percentage of the area that is to be covered by the survey units.}
\item{jitter}{method of positioning survey units inside the grid rectangles. Possible methods are
             *  \code{sync} for positioning the survey units with the same random displacements inside each grid rectangle, 
             *  \code{unsync} for positioning the survey with independent random displacements inside each grid rectangle; in this case the displacements are drawn from the same bivariate uniform distribution for every grid rectangle.}
}
\details{
        Survey design parameters for the plot sampling method are defined. The design is composed of a set of `n.interval.x' \eqn{\times}{*} `n.interval.y' survey units (lines), which total `area.covered' percentage of the whole survey area. The method of locating the units is specified using the parameters `method' and `jitter' (see above). 
To implement the design, the region is divided into `n.interval.x' \eqn{\times}{*} `n.interval.y' gridrectangles of the same size. Survey units are then positioned in the rectangles, with each rectangle including at most one survey unit. The size of the units depends on `area.covered'. All animals within the survey units are detected on the subsequent survey.
This function specifies the "rules" to be used in generating a design, and returns them in an object of class `pars.design.pl'. It does not generate the design (which involves some randomization); this is done by passing the `pars.design.pl' object to `generate.design.pl', which introduces the random components. The same `pars.design.pl' object passed to `generate.design.pl' twice will result in different designs because of this randomness. 
}
\value{
Returns an object of class `pars.design.pl´ which can be passed on to the function \code{generate.design.pl} as a parameter, and which contains the following objects:
  \item{region}{the region over which the survey takes place}
  \item{n.interval.x}{Equal to the object 'n.interval.x' passed to the function}
  \item{n.interval.y}{Equal to the object 'n.interval.y' passed to the function}
  \item{method}{Equal to the object 'method' passed to the function}
  \item{percentage.area.covered}{Equal to the object 'area.covered' passed to the function}
  \item{jitter}{Equal to the object 'jitter' passed to the function}
  \item{parents}{details of the WiSP objects passed to the function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{generate.design.pl}}
}
\examples{
pl.reg <- generate.region(x.length = 100, y.width = 50)
pl.dens <- generate.density(pl.reg)
pl.poppars<-setpars.population(density.pop=pl.dens, number.groups = 100, size.method = "poisson", 
						size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta", 
						exposure.min = 2, exposure.max = 10, exposure.mean = 6, exposure.shape = 1)
pl.pop <- generate.population(pl.poppars, seed=456)

pl.despars<-setpars.design.pl(pl.reg, n.interval.x = 10, n.interval.y = 20,method = "random", area.covered = 0.2)
summary(pl.despars)
}
\keyword{}
 


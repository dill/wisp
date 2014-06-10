\name{plot.density.sample.3d}
\alias{plot.density.sample.3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 3D visualization of population density surface and animal realizations }
\description{
  Function that makes use of the RGL library to create an interactive 3D representation of the
  density surface created by Wisp (dictating the dispersion of animals within the study area) as
  well as the distribution of animals across the study area (this is best visualized by rotating
  the surface so it is viewed from directly overhead).
}
\usage{
plot.density.sample.3d(density, sampled = NULL, scale.fact = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{density}{ density object (created by \code{\link{generate.density}},}
  \item{sampled}{ sample object resulting from capture-recapture, line transect, or point 
	transect sampling.  If this argument 
	is omitted, then only the population density surface is produced.}
  \item{scale.fact}{ A scaling factor to adjust the size of the spheres representing individual animals. }
}
\details{
  The RGL window can be maximized, moved, or otherwise resized to make visibility easier. 
	Side-effect of calling this function is that an RGL window is opened and \emph{not closed}.  
	So if this function is called repeatedly, there can be a proliferation of RGL 
	windows on the user's computer.
}
\value{
  A 3D object created in an RGL window able to be rotated and viewed interactively.
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5 \url{http://www.ruwpa.st-and.ac.uk/estimating.abundance} }
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\note{ 

 If a sample object is provided along with a density object, then the individual animals placed onto
 the density surface are both uniquely sized, and differentially coloured.  The size of an individual's
 sphere is proportional to the animal's exposure (the larger the sphere, the easier the animal is to catch/detect).
 
 The colour scheme is slightly different dependent on whether the sampling was from capture-recapture or
 distance sampling.  With distance sampling designs, animal spheres are coloured as follows:
 \describe{
 		\item{blue}{animal is outside the covered region, hence undetectable}
 		\item{red}{animal is inside covered region, and detected}
 		\item{black}{animal is inside covered region, and \emph{not} detected}
 		}
 	With mark-recapture designs, the sphere colours represent:
 \describe{
 		\item{red}{animal was captured on more than 75\% of capture occasions}
 		\item{green}{animal was captured on fewer than 75\% but more than 50\% of capture occasions}
 		\item{blue}{animal was captured on fewer than 50\% of capture occasions}
 		\item{black}{animal was never captured on any capture occasion}
 		}
}
\seealso{ \code{\link{plot.density.population}}, \code{\link{add.hotspot}}, and \code{\link{set.stripe}}
}
\examples{
myreg <- generate.region(x.length =100, y.width = 50)
mydens <- generate.density(myreg,nint.x = 100, nint.y = 50, southwest = 1, southeast = 10, northwest = 20)
mydens<-add.hotspot(mydens, 20,10, 25,20)
mydens <- add.hotspot(mydens, 40,40,50,5)
mydens <- add.hotspot(mydens, 70,10,30,15)
mydens <- set.stripe(mydens, 40,0,70,50,value=0, width=10)
mydens <- set.stripe(mydens, 60,25,100,40,value=0, width=7)
plot.density.sample.3d(mydens)
#  Generate sample created by cr sampling
cr.poppars<-setpars.population(density.pop = mydens, number.groups = 100, size.method = "poisson",
                              size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                              exposure.min = 2, exposure.max = 10, exposure.mean = 3,
                              exposure.shape = 0.5)
cr.pop<-generate.population(cr.poppars)

cr.des<-generate.design.cr(myreg, n.occ = 6)
cr.survpars<-setpars.survey.cr(cr.pop, cr.des, pmin.unmarked=0.00001, pmax.unmarked=0.5, improvement=0.01)

cr.samp<-generate.sample.cr(cr.survpars)
summary(cr.samp)
plot.density.sample.3d(mydens,cr.samp)
#  Generate and view line transect sample from same underlying density
lt.poppars<-setpars.population(density.pop = mydens, number.groups = 200,
                              size.method = "poisson", size.min = 1, size.max = 30,
                              size.mean = 10, exposure.method = "beta", exposure.min = 0,
                              exposure.max = 1, exposure.mean = 0.4, exposure.shape = 0.5)
lt.pop<-generate.population(lt.poppars)
lt.despars<-setpars.design.lt(myreg, n.transects=10, n.units=10, visual.range=3, percent.on.effort=1)
lt.des<-generate.design.lt(lt.despars, seed=3)
lt.survpars<-setpars.survey.lt(lt.pop, lt.des, disthalf.min=1.5, disthalf.max=3)
lt.samp<-generate.sample.lt(lt.survpars)
summary(lt.samp)
plot.density.sample.3d(mydens,lt.samp, scale.fact=1.5)
#  Generate and view point transect sample from same underlying density
pt.poppars<-setpars.population(density.pop = mydens, number.groups = 200, size.method = "poisson",
                               size.min = 1, size.max = 30, size.mean = 10, exposure.method = "beta",
                               exposure.min = 0, exposure.max = 1, exposure.mean = 0.4,
                               exposure.shape = 0.5)
pt.pop<-generate.population(pt.poppars)

pt.despars<-setpars.design.pt(myreg, n.transects=8, n.units=32, visual.range=3.5)
pt.des<-generate.design.pt(pt.despars)

pt.survpars<-setpars.survey.pt(pt.pop, pt.des, disthalf.min=2, disthalf.max=4)
pt.samp<-generate.sample.pt(pt.survpars)
summary(pt.samp)
plot.density.sample.3d(mydens,pt.samp, scale.fact=1.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}


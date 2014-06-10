

\name{plot.sample.pt}
\alias{plot.sample.pt}
\title{Default Plotting Statement for Survey Sample of Point Transect Method}
\description{
     This function generates a graphical representation of the specified 
     `sample.pt´ object.}

\usage{
     plot(samp, show.sizes = T, show.exps = T, dsf = 1, 
                    whole.population = F)}

\arguments{
\item{samp}{object of class `sample.pt´.}

\item{show.sizes}{if true different animal group sizes are shown as dots of different sizes.}

\item{show.exps}{if true different group exposures are shown in different shading.}

\item{dsf}{enlargement factor for animal group dots.}

\item{whole.population}{if true detected animal groups will be shown in red colour,
          undetected animal groups will be shown in black colour.}
}
\details{
     This function displays the result of a survey sample. It shows a picture of 
     the region, the survey units and indicates the detected animal groups as 
     red dots.}
\seealso{
     \code{\link{setpars.survey.pt}}, \code{\link{generate.sample.pt}} to create sample objects
     \code{\link{summary.sample.pt}}
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
plot(pt.samp) 
# show whole population
plot(pt.samp, whole.population = T) 
# ignore sizes
plot(pt.samp, show.sizes = F, whole.population = T) 
# ignore exposures
plot(pt.samp, show.exps = F, whole.population = T) 
# enlarge dots
plot(pt.samp, dsf = 2, whole.population = T) 
# reduce dots
plot(pt.samp, dsf = 0.3, whole.population = T) 
}
\keyword{}


\name{plot.sample.no}
\alias{plot.sample.no}
\title{Default Plotting Statement for Survey Sample of Nearest-Object Method}
\description{
     This function generates a graphical representation of a sample
     of point-to-nearest-object method, generated by the function 
     \code{generate.sample.no}.
}
\usage{
     plot(samp, show.sizes = F, show.exps = F, dsf = 1, whole.population = F, pnlines =  
T, nnlines = T, ...)
     plot.sample.no(samp, show.sizes = F, show.exps = F, dsf = 1, whole.population = F,  
pnlines = T, nnlines = T, ...)
          }
\arguments{
\item{samp}{sample object (of class `sample.no�)}
\item{show.sizes}{if TRUE different animal group sizes are shown as dots of different
                  sizes.}
\item{show.exps}{if TRUE different group exposures are shown in different shading.}
\item{dsf}{enlargement factor for group dots.}
\item{whole.population}{if TRUE the whole population will be plotted.
                        Detected animal groups will be shown in red colour,
          	        undetected animal groups will be shown in black colour.}
\item{pnlines}{if TRUE the survey points and their nearest objects will be connected 
               with a black line, else there won�t be any line.}
\item{nnlines}{if TRUE the nearest-to-point objects and their nearest neighbour objects
	       (if calculated) will be connected with a red line, else there won�t be
	       any line.}
}
\seealso{
     \code{\link{generate.sample.no}}
     \code{\link{summary.sample.no}}
}
\examples{
     myreg<-generate.region(x.length = 50, y.width = 80)
          
     mydens <- generate.density()
     mypoppars<-setpars.population(myreg, density.pop = mydens, 
                       number.groups = 100, size.method = "poisson", 
                       size.min = 1, size.max = 5, size.mean = 1,     
                       exposure.method = "beta", exposure.min = 2, 
                       exposure.max = 10, exposure.mean = 6,
                       exposure.shape = 1)
     mypop<-generate.population(mypoppars)
     mydes <- generate.design.no(myreg, n.points = 4)
     mysamp<-generate.sample.no(mypop, mydes, with.neighbours=T)
     
     plot(mysamp)                       # default plotting
     plot(mysamp, show.sizes = T)       # show group sizes
     plot(mysamp, show.exps = T)        # show group exposures
     plot(mysamp, dsf = 2)              # enlarge dots
     plot(mysamp, dsf = 0.3)            # reduce dots
     plot(mysamp, whole.population = T) # show whole population
}
\keyword{}


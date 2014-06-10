

\name{plot.design.pt}
\alias{plot.design.pt}
\title{Default Plotting Statement for Survey Design of Point Transect Method}
\description{
     This function generates a graphical representation of the specified 
     `design.pt´ object.}

\usage{
     plot(des, newplot = T)}

\arguments{
\item{des}{object of class `design.pt´.}

\item{newplot}{if true the new plot is added to the preceding output.}
}
\details{
     This function displays a survey design. It shows a picture of the region 
     and the survey units.}
\seealso{
     \code{\link{setpars.design.pt}}, \code{\link{generate.design.pt}} to create design objects}
\examples{
     myreg<-generate.region(x.length = 50, y.width = 80)
     
     mydespars<-setpars.design.pt(myreg, n.transects = 3, n.units = 15, 
                       visual.range = 3)
     mydes<-generate.design.pt(mydespars)
     plot(mydes) 
     mydespars<-setpars.design.pt(myreg, visual.range = 5)  # new design
     mydes2 <- generate.design.pt(mydespars)              # generate new design
     plot(mydes2, newplot = F)                   # add new design to plot
}
\keyword{}


\name{transform.to.rmark}
\alias{transform.to.rmark}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Transforms sample.cr data to RMark format }
\description{
  Conversion function transforming Wisp objects of type \code{sample.cr} into
  R objects that can be analyzed by the routines in the R package RMark.
}
\usage{
transform.to.rmark(sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{ object created by \code{generate.sample.cr} or built-in dataset in Wisp }
}
\details{
The storage of the capture histories within a Wisp object of type \code{sample.cr} needs
to be transformed so these data can be analyzed by routines in the \pkg{RMark} library.  This
transformation is quite simple, and is conducted by this function.

Transformed objects may then be analyzed using \pkg{RMark}.  Routines in the \pkg{RMark} library can
create more sophisticated closed abundance models than are available in Wisp.
}
\value{
Data frame consisting of a single field, \code{ch}, a character variable containing the capture histories of each animal in the sample.
}
\references{ 
Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5 \url{http://www.ruwpa.st-and.ac.uk/estimating.abundance}  
 \pkg{RMark} home page by Jeff Laake, \url{http://www.phidot.org/software/mark/rmark/} 
 \pkg{RMark} chapter of \emph{Gentle introduction to Mark} \url{http://www.phidot.org/software/mark/docs/book/pdf/app_3.pdf} 
}
\author{ Eric Rexstad, RUWPA \email{ericr@mcs.st-and.ac.uk} }
\seealso{  \code{\link{generate.sample.cr}}, \code{\link{birds1997.samp.cr}}, \code{\link{chips.samp.cr}}, \code{\link{taxi.samp.cr}}, and \code{\link{voles1.samp.cr}} }
\examples{
#  Generate population, and take mark-recapture sample
cr.reg<-generate.region(x.length=100, y.width=50)
cr.dens<-generate.density(cr.reg)
cr.poppars<-setpars.population(density.pop = cr.dens, number.groups = 100, size.method = "poisson",
                              size.min = 1, size.max = 5, size.mean = 1, exposure.method = "beta",
                              exposure.min = 2, exposure.max = 10, exposure.mean = 3,
                              exposure.shape = 0.5, type.values = c("Male","Female"),
                              type.prob = c(0.48,0.52))
cr.pop<-generate.population(cr.poppars)

cr.des<-generate.design.cr(cr.reg, n.occ = 6)
cr.survpars<-setpars.survey.cr(cr.pop, cr.des, pmin.unmarked=0.00001, pmax.unmarked=0.5, improvement=0.01)

mark.recap.samp<-generate.sample.cr(cr.survpars)
summary(mark.recap.samp)

test <- transform.to.rmark(mark.recap.samp)

pdotshared <- list(formula=~1,share=TRUE)
ptimeshared <- list(formula=~time,share=TRUE)
ptime <- list(formula=~time)
ptime.mixtureshared <- list(formula=~time*mixture,share=TRUE)
ptimemixtureshared <- list(formula=~time+mixture,share=TRUE)
pmixture <- list(formula=~mixture)

#   Capture Closed models
#
require(RMark)
#  constant p=c
test.closed.m0 <- mark(test,model="Closed",model.parameters=list(p=pdotshared))
#  constant p and constant c but different
test.closed.mb <- mark(test,model="Closed")
#  time varying p=c
test.closed.mt <- mark(test,model="Closed",model.parameters=list(p=ptimeshared))
#  time varying p + additive c
test.closed.mtb <- mark(test,model="Closed",model.parameters=list(p=ptime))
#
#  Closed heterogeneity models
#
#  2 mixtures Mh2
test.closed.Mh2 <- mark(test,model="HetClosed",model.parameters=list(p=pmixture))
#  Closed Mth2 - p different for time and mixtures
test.closed.Mth2 <- mark(test,model="FullHet",
				model.parameters=list(p=ptime.mixtureshared),adjust=TRUE)
#  Closed Mth2 - p different for time; mixture additive
test.closed.Mth2.additive <- mark(test,model="FullHet",
				model.parameters=list(p=ptimemixtureshared),adjust=TRUE)

test.table <- model.table(adjust=TRUE)
test.table
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}


\name{generate.design.rm}
\alias{generate.design.rm}
\title{Survey Design of Removal Methods}
\description{
        Generates a removal method survey design.
}
\usage{
        generate.design.rm(reg, n.occ = 2, effort = rep(1, n.occ))
}
\arguments{
\item{reg}{object of class `region´.}
\item{n.occ}{number of survey occasions.}
\item{effort}{vector of catch effort levels to be used on each survey occasion.}
}
\details{
        The function generates a removal method design object specifying the number of capture occasions and effort levels used on each occasion. The effect of the effort levels in terms of catchability is determined by the capture function, which is specified using `setpars.survey.rm'. Conceptually, the function `generate.design.rm' involves only the aspects of the removal method survey that are directly under the control of those doing the survey, while `setpars.survey.rm' involves the aspects of the survey that affect which animals are detected, and involve some random component not directly under the control of those doing the survey.  The function generate.design.rm, can later be analysed using specific removal method functions such as \code{\link{point.est.ce}} for catch-effort method, or \code{\link{point.est.cir}} for the change-in-ratio method.
}
\value{
\code{generate.design.rm} returns an object of class `design.rm´ which has the following elements:
  \item{region}{object of class `region´.}
  \item{number.occasions}{number of survey occasions.}
  \item{effort}{vector of catch efforts of the survey occasions.}
}
\seealso{
        \code{\link{setpars.survey.rm}}
        \code{\link{generate.sample.rm}}
}
\examples{
reg<- generate.region(x.length = 50, y.width = 80)
des<- generate.design.rm(reg, n.occ = 4, effort = c(5,4,3,2))
summary(des)
}
\keyword{}

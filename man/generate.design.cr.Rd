\name{generate.design.cr}
\alias{generate.design.cr}
\title{Survey design of mark-recapture method}
\description{
        Generates a mark-recapture survey design.
}
\usage{
        generate.design.cr(reg, n.occ = 2, effort = rep(1, n.occ))
}
\arguments{
\item{reg}{object of class `region´.}
\item{n.occ}{number of survey (capture) occasions.}
\item{effort}{vector specifying level of effort used on each survey occasion.}
}
\details{
        The function generates a mark-recapture method design object specifying the number of capture occasions and effort levels used on each occasion. The effect of the effort levels in terms of catchability is determined by the capture function, which is specified using `setpars.survey.cr'. Conceptually, the function `generate.design.cr' involves only the aspects of the mark-recapture survey that are directly under the control of those doing the survey. The function \code{setpars.survey.cr} sets the parameters of the parts of the survey that involve some randomness.
}
\value{ 
\code{generate.design.cr} returns an object of class `design.cr´ which has the following elements:
  \item{region}{object of class `region´}
  \item{number.occasions}{number of survey occasions}
  \item{effort}{vector specifying level of effort used on each survey occasion}
  \item{parents}{Details of WiSP objects passed to function}
  \item{created}{Creation date and time}
}
\seealso{
        \code{\link{setpars.survey.cr}}, \code{\link{generate.sample.cr}}
}
\examples{
cr.reg<- generate.region(x.length = 50, y.width = 80)
cr.des<- generate.design.cr(cr.reg, n.occ = 4, effort = c(5,4,3,2))
summary(cr.des)
}
\keyword{}

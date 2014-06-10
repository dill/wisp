\name{detection.capture.recapture}
\alias{detection.capture.recapture}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculation of detection probability for individuals }
\description{
  It constructs a matrix with the dimensions
      "animal groups" and "number of search occasions"
      It calculates the detection probability of each
      animal group in each survey occasion.
      This function depends (aside of some parameters)
      of the exposure of each group, the survey effort
      and the survey time (survey event)
}
\usage{
detection.capture.recapture(theta0, theta1, theta2, exposure, effort)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta0}{ parameter of detection function, belonging to exposure }
  \item{theta1}{ parameter of detection function, belonging to exposure }
  \item{theta2}{ parameter of detection function, belonging to survey occasion }
  \item{exposure}{ Exposure value for each group }
  \item{effort}{ Search effort for each occasion }
}
\details{
  This is an internal function and need not be accessed by the user.
}
\value{
  \item{p.detect }{Matrix with detection probability of each group in each survey}
}
\references{ }
\author{ M. Erdelmeier }
\note{ }


\seealso{ }
\examples{}
\keyword{ }% __ONLY ONE__ keyword per line

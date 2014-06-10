\name{detection.removalmethods}
\alias{detection.removalmethods}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Compute detection probabilities for removal methods }
\description{
  calculate the detection probability for each group in
  each survey (depends on exposure of the group, the
  survey effort and the survey time respective number):
  p=1-exp[-(th0+th1*expos)*effort*(1+th2*(occasion.number-100]
}
\usage{
detection.removalmethods(theta0, theta1, theta2, exposure, effort)
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
\examples{ }
\keyword{ }% __ONLY ONE__ keyword per line

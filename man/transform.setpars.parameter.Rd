\name{transform.setpars.parameter}
\alias{transform.setpars.parameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Transforms detection function from user-friendly to manner used in computations }
\description{
  The function transforms the user defined parameter
      (they describe the detection function in a "user
      friendly" way)) into the parameter that are really
      used inside the detection function.
}
\usage{
transform.setpars.parameter(min.exposure, max.exposure, pmin, pmax, improvement)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{min.exposure}{ minimum of the exposure value range }
  \item{max.exposure}{ maximum of the exposure value range }
  \item{pmin}{ expected proportion of detected groups at first occasion, with
  minimum exposure and minimum effort }
  \item{pmax}{ expected proportion of detected groups in first occasion, with
  maximum exposure and minimum effort }
  \item{improvement}{ improvement in detection (per occasion at minimum effort and mean exposure) }
}
\details{
  This is an internal function and need not be accessed by the user.
}
\value{
  \item{theta0, theta1, theta2 }{Calculated parameters of the detection function}
}
\references{  }
\author{ M. Erdelmeier }
\note{ }


\seealso{  }
\examples{ }
\keyword{ }% __ONLY ONE__ keyword per line

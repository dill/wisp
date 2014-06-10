\name{seal.samp.rm}
\alias{seal.samp.rm}
\title{Data from the a removal method survey of a simulated seal population on a beach}
\description{
These simulated data are from a 5-sample removal method survey of a seal population on a beach. 
The simulated data are based on an aerial photograph of a real population. 
These data are identical to those in `seal.sample.ce`, except that they are of a different R class.

\itemize{
 \item{population}{object of class `population´.}
 \item{design}{object of class `design.rm´.}
 \item{removal}{Two dimensional detection matrix that indicates which animal
          groups have been captured on which occasion. A '1' in row i,
          column j indicates that the i-th animal/group was removed the
          occasion before j; a '0' means that it was not.}
 \item{detected}{Two dimensional detection matrix that indicates which animal
          groups have been detected and consequently removed on which
          occasion. A '1' in row i, column j indicates that the i-th
          animal/group was detected on occasion j; a '0' means that it
          was not.}
 \item{parents}{Details of WiSP objects passed to function}
 \item{created}{Creation date and time}
 \item{seed}{Equal to the argument 'seed' passed to the function.}
}
}
\usage{data(seal.samp.rm)}
\format{An object of class `sample.rm`}
\source{Aerial photograph}
\references{None}
\keyword{datasets}


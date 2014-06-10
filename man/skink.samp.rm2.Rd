\name{skink.samp.rm2}
\alias{skink.samp.rm2}
\title{Two-sample removal method sample of spotted skinks on North Brother Island, New Zealand}
\description{
These are data from a 2-sample removal method survey of spotted skinks on North Brother Island, New Zealand. 
The original data (`skink.sample.cr`) are mark-recapture data; removal method data were created by treating
marking as removal. 
These data are identical to the first two capture occasion data of `skink.sample.rm`.

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
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}
}
\usage{data(skink.samp.rm2)}
\format{An object of class `sample.rm`}
\source{P. Phillpot. 2000. unpublished M.Sc. thesis, Victoria University of Wellington. 
Skinks (Oligosoma lineoocellatum) on North Brother Island, November 1999.}
\references{P. Phillpot. 2000. unpublished M.Sc. thesis, Victoria University of Wellington. 
Skinks (Oligosoma lineoocellatum) on North Brother Island, November 1999.}
\keyword{datasets}


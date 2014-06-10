\name{samp2.cr}
\alias{samp2.cr}
\title{Simulated data from the an 8-sample mark-recapture method survey of a 
simulated population}
\description{
Simulated data from the an 8-sample mark-recapture method survey of a simulated 
population.

\itemize{
\item{"population"}{Sample information for the observed population; includes exposure, 
groupsize and types}
\item{"design"}{Design information including the number of transects and survey effort}
\item{"detected.observer1"}{An array the jth element of which is a 1 if the first observer saw the jth distinct animal and 0 if they did not.}
\item{"detected.observer2"}{An array the jth element of which is a 1 if the second observer saw the jth distinct animal and 0 if they did not.}
\item{"detected"}{An array the jth element of which is a 1 if the jth group was detected.}
 \item{"distance"}{Perpendicular distances from the observer to the detected animals.}
\item{"transect"}{Index showing from which transects the animals were detected}
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}
}
\usage{data(samp2.cr)}
\format{An object of class `sample.cr`}
\source{None}
\references{None}
\keyword{datasets}


\name{samp1.lt}
\alias{samp1.lt}
\title{Simulated data from the a line transect survey of a simulated population.}
\description{Simulated data from the a line transect survey of a simulated population. 
This survey is of the exact same population as samp2.lt, except that the transect lines
were rotated by 90 degrees. (Because WiSP always shows transect lines as going from top to
bottom of the figure, the population appears rotated by 90 degrees on this survey.)  
It is an object of class sample.lt containing the following items:

\itemize{
\item{"population"}{Sample information for the observed population; includes exposure, 
groupsize and types}
\item{"design"}{Design information including the number of transects and survey effort}
\item{"detected.observer1"}{An array the jth element of which is a 1 if the first observer saw the jth distinct animal and 0 if they did not.}
\item{"detected.observer2"}{An array the jth element of which is a 1 if the second observer saw the jth distinct animal and 0 if they did not.}
\item{"detected"}{An array the jth element of which is a 1 if the jth group was detected.} \item{"distance"}{Perpendicular distances from the observer to the detected animals.}
\item{"transect"}{Index showing from which transects the animals were detected}
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}
}
\usage{data(samp1.lt)}
\format{An R object of class 'sample.lt'}
\source{Simulated}
\references{None}
\keyword{datasets}


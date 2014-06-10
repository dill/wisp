\name{tortoise.samp.lt}
\alias{tortoise.samp.lt}
\title{Survey of polystyerene desert tortoise.}
\description{The data come from a field experiment where artificial desert 
tortoises were laid out along 3 100m x 1km long transects in the Nevada desert 
and teams of 3 tortoise biologists took turns to go and survey them.  
These data are from team 11.
It is an object of class sample.lt containing the following items:

\itemize{
\item{"population"}{WiSP object constructed to be consistent with data. (Note that although exposure and 
type are recorded as the same for all animals, in reality there were differences.)}
\item{"design"}{Design information including the number of transects and survey effort}
\item{"detected"}{An array the jth element of which is a 1 if the jth group was detected.} 
\item{"distance"}{Perpendicular distances from the observer to the detected animals.}
\item{"transect"}{Index showing from which transect the animals were detected}
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Date and time at which this sample.lt object was created.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}
}
\usage{data(tortoise.samp.lt)}
\format{An R object of class 'sample.lt'}
\source{Nevada desert field experiment.}
\references{Anderson et al. 2001 Field trials of line transect methods applied to estimation 
of desert tortoise abundance.  Journal of Wildlife Management 65: 585-597.}
\keyword{datasets}


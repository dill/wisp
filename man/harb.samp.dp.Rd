\name{harb.samp.dp}
\alias{harb.samp.dp}
\title{Harbour Porpoise sample data from 1994 SCANS survey}
\description{
The data in harb.samp are harbour porpoise sightings data from the shipboard 
component of the 1994 Small Cetacean Abundance in the North Sea (SCANS) survey in the North Sea. An  
analysis of these data is published in Borchers et al. (1998).  The dataset harb.samp.dp is a sample  
object of class sample.dp and contains the following items:

\itemize{
\item{"population"}{Sample information for the observed population; includes exposure, 
groupsize and types}
\item{"design"}{Design information including the number of transects and survey effort}
\item{"detected.observer1"}{An array the jth element of which is a 1 if the first observer saw the jth  
distinct animal and 0 if they did not.}
\item{"detected.observer2"}{An array the jth element of which is a 1 if the second observer saw the jth  
distinct animal and 0 if they did not.}
\item{"detected"}{An array the jth element of which is a 1 if the jth group was detected.}  
\item{"distance"}{Perpendicular distances from the observer to the detected animals.}
\item{"transect"}{Index showing from which transects the animals were detected}
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}
}
\usage{data(harb.samp.dp)}
\format{An R sample object of class 'sample.dp'}
\source{1994 SCANS survey in the North Sea}
\references{Borchers, D.L., Buckland, S.T., Goedhart, P.W., Clarke, E.D. and Hedley, S.L. 1998. 
Horvitz-Thompson estimators for double-platform line transect surveys. Biometircs 54: 1221-1237.}
\keyword{datasets}


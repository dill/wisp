\name{taxi.samp.cr} 
\alias{taxi.samp.cr}
\title{Edinburgh taxicab data}
\description{ The data are in a sample object of class 'sample.cr'
that contains the matrix of capture histories of Edinburgh taxicabs 
observed over 10 occasions.
\itemize{
\item{"population"}{Arbitrary population information that is required by the sample  
object - you can ignore it}
\item{"design"}{Arbitrary design information required by the sample object - you can  
ignore it}
\item{"capture"}{
A 283 * 10 matrix of capture histories of 1s and 0s where the rows represent the  
distinct taxicabs 
and the columns represent the sighting occasions.  If taxicab i was seen on occasion  
j then the
(i,j)th entry of the capture matrix will be 1; otherwise it will be 0} 
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}}
\usage{data(taxi.samp.cr)} 
\format{An R sample object of class'sample.cr'} 
\source{ 
Carothers, A.D. (1973). "Capture-recapture methods applied to a population with  
known parameters"
Journal of Animal Ecology 42, 125-146.
} 
\references{ 
Ashbridge and Goudie (2000).  Coverage-Adjusted Estimators for Mark-Recapture in  
Heterogeneous
Populations.  Communications in Statistics: Simulation and Computation 29(4),  
1215-1237.}
\keyword{datasets}


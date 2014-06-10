\name{hare.samp.cr} 
\alias{hare.samp.cr}
\title{Snowshoe hare data}
\description{These data come from a 6-sample mark-recapture survey of snowshoe hares.
They are in an object of class 'sample.cr' containing the following:

\itemize{
\item{"population"}{Fabricated population information (consistent with the data) that is required by WiSP  
- you can ignore it.}
\item{"design"}{Design information (consistent with the data) that is required by WiSP - you can ignore  
it.}
\item{"capture"}{
A 68 * 6 matrix of capture histories of 1s and 0s where the rows represent the distinct hares 
and the columns represent the capture occasions.  If hare i was caught on occasion j then the
(i,j)th entry of the capture matrix will be 1; otherwise it will be 0} 
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
}}
\usage{data(hare.samp.cr)} 
\format{An R sample object of class'sample.cr'} 
\source{ 
Collected but unpublished snowshoe hare data by Burnham and Cushwa.
} 
\references{ 
Ashbridge and Goudie (2000).  Coverage-Adjusted Estimators for Mark-Recapture in Heterogeneous
Populations.  Communications in Statistics: Simulation and Computation 29(4), 1215-1237.}
\keyword{datasets}


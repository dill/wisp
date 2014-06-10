\name{chips.samp.cr} 
\alias{chips.samp.cr}
\title{Sample from a mark-recapture survey of an eastern chipmunk population}
\description{Mark-recapture survey data (an object of class 'sample.cr') containing the matrix of capture histories of eastern chipmunks caught over 13 capture occasions. While the capture frequency data are real, the capture history data have been fabricated (to be consistent with the capture frequency data) in order to allow estimation in WiSP. (Dorazio and Royle do not provide capture history data.
\itemize{
\item{"population"}{Fabricated population information (consistent with the data) that is required by WiSP - you can ignore it.}
\item{"design"}{Design information (consistent with the data) that is required by WiSP - you can ignore it.}
\item{"capture"}{
A 72 * 13 matrix of capture histories of 1s and 0s where the rows represent the distinct chipmunks 
and the columns represent the capture occasions.  If chipmunk i was caught on occasion j then the
(i,j)th entry of the capture matrix will be 1; otherwise it will be 0} 
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}}
\usage{data(chips.samp.cr)} 
\format{An R sample object of class 'sample.cr'} 
\source{
Mares, M.A., Streilien, K.E. and Willig, M.R. (1981). "Experimental assessment of several population
estimation techniques on an introduced population of eastern chipmunks" Journal of Mammalogy 62, 315-328.
} 
\references{ 
Ashbridge and Goudie (2000).  Coverage-Adjusted Estimators for Mark-Recapture in Heterogeneous
Populations.  Communications in Statistics: Simulation and Computation 29(4), 1215-1237.}
\keyword{datasets}


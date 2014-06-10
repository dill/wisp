\name{birds2001.samp.cr}
\alias{birds2001.samp.cr}
\title{Dorazio and Royle's Mark-recapture bird data for 2001}
\description{The data are in an object of class 'sample.cr' containing the matrix of 
capture histories of species observed at up to 36 locations along a single route of the  
'North 
American Breeding Bird Survey' consisting of 50 sighting locations. While the capture  
frequency data are real, the capture history data have been fabricated (to be  
consistent with the capture frequency data) in order to allow estimation in WiSP.  
(Dorazio and Royle do not provide capture history data.) 
\itemize{
\item{"population"}{Fabricated population information (consistent with the data) that  
is required by WiSP - you can ignore it.}
\item{"design"}{Design information (consistent with the data) that is required by WiSP  
- you can ignore it.}
\item{"capture"}{A 67 * 50 matrix of capture histories of 1s and 0s where the rows  
represent the
distinct birds and the columns represent the sighting locations.  If bird i was seen at  
sighting
location j then the (i,j)th entry of the capture matrix will be 1; otherwise it will be  
0}
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}
}
\usage{data(birds2001.samp.cr)}
\format{An R sample object of class 'sample.cr'}
\source{North American Breeding Bird Survey 2001}
\references{
Dorazio and Royle (2003).  Mixture Models for Estimating the Size of a Closed  
Population When Capture
Rates Vary among Individuals.  Biometrics 59, 351-364.}
\keyword{datasets}


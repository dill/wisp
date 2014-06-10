\name{voles1.samp.cr} 
\alias{voles1.samp.cr}
\title{Meadow vole data}
\description{ The data are in a sample object of class 'sample.cr'
that contains the matrix of capture histories of meadow voles 
caught over 5 capture occasions.
\itemize{
\item{"population"}{Arbitrary population information that is required by the sample  
object - you can ignore it}
\item{"design"}{Arbitrary design information required by the sample object - you can  
ignore it}
\item{"capture"}{
A 102 * 5 matrix of capture histories of 1s and 0s where the rows represent the  
distinct meadow voles 
and the columns represent the capture occasions.  If meadow vole i was caught on  
occasion j then the
(i,j)th entry of the capture matrix will be 1; otherwise it will be 0} 
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}}
\usage{data(voles1.samp.cr)} 
\format{An R sample object of class'sample.cr'} 
\source{ 
Pollock, K.H., Nichols, J.D., Brownie, C. and Hines, J.E. (1990). "Statistical  
inference for capture-recapture experiments," Wildlife Monographs 107, 1-97.
} 
\references{ 
Ashbridge and Goudie (2000).  Coverage-Adjusted Estimators for Mark-Recapture in  
Heterogeneous
Populations.  Communications in Statistics: Simulation and Computation 29(4),  
1215-1237.}
\keyword{datasets}


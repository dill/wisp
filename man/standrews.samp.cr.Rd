\name{standrews.samp.cr}
\alias{standrews.samp.cr}
\title{St Andrews golf tee data}
\description{ The data in eg1.data are data from a survey of 250
clusters of golf tees in grass,  conducted by eight 3 rd and 4th year
statistics students at the University of St Andrews. standrews.samp is
a sample object of class sample.cr and contains the following
items:
\itemize{
\itemize{
\item{"population"}{Sample information for the observed population; includes exposure, groupsize and  
types}
\item{"design"}{Arbitrary design information required by the sample object - you can ignore it}
\item{"capture"}{A 162 * 8 matrix of capture histories of 1s and 0s where the rows represent the
distinct detected tees and the columns represent the eight observers. If tee i was seen by observer
j then the (i,j)th entry of the capture matrix will be 1; otherwise it will be 0} 
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}
\item{"population"}{Arbitrary population information that is required by the sample object - you can  
ignore it}
}
}
\usage{data(standrews.samp.cr)}
\format{An R sample object of class 'sample.cr'}
\source{Statistics honours class, University of St Andrews }
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations.  
Springer. London. 314pp.}
\keyword{datasets}


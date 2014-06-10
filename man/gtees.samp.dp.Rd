\name{gtees.samp.dp}
\alias{gtees.samp.dp}
\title{St Andrews golf tee data}
\description{ The data in eg1.data are data from a survey of 250
clusters of golf tees in grass,  conducted by 3 rd and 4th year
statistics students at the University of St Andrews. golf.samp is
a sample object of class sample.dp and contains the following
items:

\itemize{
\item{"population"}{Sample information for the observed population; includes exposure, groupsize and  
types. NOTE: the group location and transect location data in this object have not yet been formatted for  
a wisp object. Plots of group and transect locations will produce rubbish. This will be rectified in due  
course.}
\item{"design"}{Design information including the number of transects and survey effort}
\item{"detected.observer1"}{An array the jth element of which is a 1 if the first observer saw the jth  
distinct animal and 0 if they did not.}
\item{"detected.observer2"}{An array the jth element of which is a 1 if the second observer saw the jth  
distinct animal and 0 if they did not.}
\item{"distance"}{Perpendicular distances from the observer to the detected animals.}
\item{"transect"}{Index showing from which transects the animals were detected}
\item{parents}{Information that is required by WiSP - you can ignore it.}
\item{created}{Information that is required by WiSP - you can ignore it.}
\item{seed}{Information that is required by WiSP - you can ignore it.}
}
}
\usage{data(golf.samp.dp)}
\format{An R sample object of class 'sample.dp'}
\source{Statistics honours class, University of St Andrews }
\references{
Borchers, D.L., Buckland, S.T. and Zucchini, W. 2002. Estimating animal abundance: closed populations.  
Springer. London. 314pp.}
\keyword{datasets}


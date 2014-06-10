\name{import.rm}
\alias{import.rm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Import summary statistics from removal experiments for abundance estimation }
\description{
  Import data from actual field data for analysis with the removal estimator of Wisp.
}
\usage{
import.rm(catch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{catch}{ Vector of catch statistics, with length equal to number of sampling occasions }
}

\value{
  Object of class \code{sample.rm} that can be submitted to \code{point.est.rm} for abundance estimation
}
\references{ Borchers, Buckland, and Zucchini (2002), Estimating animal abundance: closed populations.  Chapter 5 \url{http://www.ruwpa.st-and.ac.uk/estimating.abundance} }
\author{ David Borchers, \email{dlb@mcs.st-and.ac.uk} }
\seealso{ \code{\link{generate.sample.rm}}, \code{\link{point.est.rm}} }
\examples{
#  an external file containing data to be read (called 'egdat.txt')
#------------------------------------------
#Habitat	Year	Season	Block	occ1	occ2	occ3	occ4	occ5	occ6	occ7	occ8	occ9
#Forest	2001	Summer	A	4	1	3	2	0	1	2	0	0
#Forest	2001	Summer	B	4	3	3	0	1	0	0	0	0
#Forest	2001	Summer	C	6	3	2	2	1	0	0	2	1
#Forest	2001	Summer	D	3	1	2	2	0	0	1	1	0
#Forest	2001	Summer	E	1	1	2	1	0	0	0	0	0
#Forest	2001	Winter	F	3	4	0	1	0	1	0	0	0
#Forest	2001	Winter	G	1	2	1	1	0	0	0	0	0
#Forest	2001	Winter	H	3	1	2	0	1	0	0	0	0
#Forest	2001	Winter	A	3	2	2	1	0	1	0	1	0
#Forest	2001	Winter	B	4	3	2	0	2	1	1	0	0
#------------------------------------------
# need to put file egdat.txt into working directory before executing next line:
rdat = read.table(file="egdat.txt",header=TRUE,sep="\t")

# Each row of rdat is a separate removal method survey data
# Here's some code to import the caputre columns of any given 
# row (row=1 here) of the file, convert it to a WiSP object and estimate abundance
row <- 1 # choose row
occasions <- 5:13 # cols with capture occasion data (need to set this manually)
max.occ <- length(occasions) # max number occasions in rdat
catch <- rdat[row,occasions] # get capture history rows
catch <- as.vector(catch[!is.na(catch)]) # remove missing values at end of row and convert to vector
#
# So now catch is a vector containing just the removal data from a single survey
samp <- import.rm(catch) # convert to wisp object
est <- point.est.rm(samp) # estimate abundance
summary(est)
plot(est)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{}


generate.region<-function (x.length = 100, y.width = 100, habitat = matrix(0, 1, 1)) 
{
 if (!is.numeric(y.width) | !is.numeric(x.length)) 
   stop("\n*** y.width and x.length must be numeric values\n")
 if ((y.width <= 0) | (x.length <= 0)) 
   stop("\n*** y.width and x.length must be greater than 0\n")
 if (!is.matrix(habitat)) 
   stop("\n*** Habitat must be given as a matrix\n")
 reg <- list(length=x.length, width=y.width, habitat=habitat, parents=NULL, created=date())
# names(reg) <- c("length", "width", "habitat")
 class(reg) <- "region"
 return(reg)
}


is.region <- function (reg) 
{
    # test if <reg> is of the type "region "
    inherits(reg, "region")
}


#equal.region <- function(reg1, reg2) # changed because got error - should work?
equal <- function(reg1, reg2)

#-----------------------------------------------------
# description:
#   The function tests if two regions are identical.
#-----------------------------------------------------
# I'm not sure that this function is not redundant in
# more recent versions of WiSP: should check at some
# point, but meanwhile its in here for safety.
# DLB June 2004
#-----------------------------------------------------
{
   # test given regions
   if (!is.region(reg1) | !is.region(reg2))
      stop ("\n*** <reg1> and <reg2> must be of type 'region'\n")

   # compare length
   eq <- T      # init
   if (reg1$length != reg2$length)
      eq <- F

   # compare width   
   if (reg1$width != reg2$width)
      eq <- F

   # compare habitat
   hab1 <- reg1$habitat
   hab2 <- reg2$habitat
   if (any(dim(hab1) != dim(hab2)))
   {
      # different matrix dimensions
      eq <- F
   } else
   {
      # same matrix dimansions: test matrix values
      if (any(hab1 != hab2))
         eq <- F
   }

   # return compare result
   return(eq)
}


summary.region<-function(reg)
{
# check class:
 if (!is.region(reg)) stop("\nThe parameter <reg> must be of type 'region'.\n")
 cat("\n")
 cat("SURVEY REGION SUMMARY\n")
 cat("---------------------\n")
 cat("creation date   :", reg$created,"\n")
 cat("\n")
 cat("Dimensions (length x width):", reg$length, "x", reg$width, "\n")
 cat("Surface Area               :", reg$length*reg$width, "\n")
}




plot.region<-function (reg, reset.pars=TRUE) 
#-----------------------------------------------------------------------
# Plots region with correct aspect ratio and axes but no title.
# If reset.pars==TRUE graphical parameters are restored to what they
# were before calling the function.
# If you're going to plot something in the region (e.g. group locations)
# call plot.region with reset.pars=FALSE, else the coordinate systems
# of the region and the thing subsequently plotted will not match and
# you'll get a mess.
#-----------------------------------------------------------------------
{
 if (!is.region(reg)) 
   stop("\nThe parameter <reg> must be of class 'region'.\n")
# if ((newplot != TRUE) & (newplot != FALSE)) 
#   stop("\nThe parameter <newplot> must be TRUE or FALSE.\n")
#if(newplot) plot.new()
 old.par <- par(no.readonly = TRUE)
 len <- reg$length
 width <- reg$width
 xlimit<-c(0,reg$len)
 ylimit<-c(0,reg$width)
 lims <- sqlimits(xlimit, ylimit)
 par(usr = c(lims$xlims[1], lims$xlims[2], lims$ylims[1], lims$ylims[2]))
 polygon(xlimit[c(1,2,2,1)], ylimit[c(1,1,2,2)])
 axis(1,pretty(seq(xlimit[1],xlimit[2],length=10)),pos=0)
 axis(2,pretty(seq(ylimit[1],ylimit[2],length=10)),pos=0)
 if(reset.pars) par(old.par)
}



sqlimits<-function (xlim, ylim) 
#-------------------------------------------------------------------
# This function is taken directly from library(blighty).
# (Many thanks to its author, David Lucy!)
# It calculates the plot region dimensions that allow subsequent
# plotting preserving aspect ratio range(ylim)/range(xlim).
# See plot.region() for how to use it.
#-------------------------------------------------------------------
{
    x1 <- xlim[1]
    x2 <- xlim[2]
    y1 <- ylim[1]
    y2 <- ylim[2]
    frame()
    fig.ratio <- (x2 - x1)/(y2 - y1)
    plot.ratio <- par("pin")[1]/par("pin")[2]
    if (fig.ratio >= plot.ratio) {
        x1lim <- x1
        x2lim <- x2
        ydist <- y2 - y1
        total.ydist <- (ydist * fig.ratio * (1/plot.ratio))
        diff.ydist <- (total.ydist - ydist)/2
        y1lim <- y1 - diff.ydist
        y2lim <- y2 + diff.ydist
    }
    if (fig.ratio < plot.ratio) {
        y1lim <- y1
        y2lim <- y2
        xdist <- x2 - x1
        total.xdist <- (xdist * (1/fig.ratio) * plot.ratio)
        diff.xdist <- (total.xdist - xdist)/2
        x1lim <- x1 - diff.xdist
        x2lim <- x2 + diff.xdist
    }
    xlims <- c(x1lim, x2lim)
    ylims <- c(y1lim, y2lim)
    return(list(xlims=xlims, ylims=ylims))
}




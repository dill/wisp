calculate.plot.margin <- function(reg.x, reg.y, area.x, area.y)

   #-----------------------------------------------------
   # description:
   #   The function calculates the x and y width of the 
   #   outer border of the region that shall be plotted 
   #   later.
   #   The aim is to calculate the outer margins in the
   #   way that population area is right proportioned.
   #   That means that its borders are right scaled in
   #   ratio (reg.x:reg.y).
   #
   # author: M. Erdelmeier
   #-----------------------------------------------------

   #-----------------------------------------------------
   # input/output-variables
   #-----------------------------------------------------

   # name   | type | I/O | description
   #---------------------------------------------------------------------
   # area.x | real |  I  | x-length of possible plot area
   # area.y | real |  I  | y-length of possible plot area 
   # margin | list |  O  | object that contains the calculated width of
   #        |      |     | the x/y margins so that the plot is right
   #        |      |     | proportioned
   # reg.x  | real |  I  | x-length of population region
   # reg.y  | real |  I  | y-length of population region 


   #-----------------------------------------------------
   # used objects
   #-----------------------------------------------------

   # name        | type         | R/W | description
   #---------------------------------------------------------------------


   #-----------------------------------------------------
   # local variables
   #-----------------------------------------------------

   # name         | type | description
   #-----------------------------------------------------------------
   # margin.width | real | minimum width of the margins
   # margin.x     | real | calculated margin in x direction
   # margin.y     | real | calculated margin in y direction


   #-------------------------------------------------------
   # programming part
   #-------------------------------------------------------

{
   # reduce area dimensions by margin width  because we need a 
   # certain outer margin
   margin.width <- 0.5
   area.x <- area.x- 2*margin.width
   area.y <- area.y- 2*margin.width 

   # calculate outer border of plot area (depending whether plot is limited
   # in x or y direction)
   if ((area.y/area.x) < (reg.y/reg.x))
   {
      # plot is limited vertically: calculate horizontal border
      margin.x <- margin.width + 0.5 * (area.x - area.y * (reg.x/reg.y))
      margin.y <- margin.width 

   } else
   {
      # plot is limited horizontally: calculate vertical border
      margin.x <- margin.width 
      margin.y <- margin.width + 0.5 * (area.y - area.x * (reg.y/reg.x))
   }

   # return result
   margin <- list(x=margin.x, y=margin.y)
   return(margin)
}



#-------------------------------------------------------
# Obscure: to remove unobserved data from sample objects
#-------------------------------------------------------

obscure<-function(x,...) {UseMethod("obscure")}


plot.text<-function(x,col="black", cex=1)
#------------------------------------------------------------------------
# Utility function to put a message in the plot window used when 
# there's no appropriate thing to plot.
#------------------------------------------------------------------------
{
 if(!is.character(x)) stop("Argument <x> must be a character variable.\n")
 plot(c(0,100),c(0,100),type="n",bty="n",ann=FALSE,xaxt="n",yaxt="n")
 text(50,50,label=x,col=col,cex=cex)
}



# Some miscellalneous functions:

n.sturges<-function(x)
#------------------------------------------------------------------------
# Uses Sturges' Rule to calculate number of intervals for histogram of x.
#------------------------------------------------------------------------
{
 round(1+log2(length(x)))
}



#===========================================================
# equal (generic function)
#===========================================================

equal <- function(obj1, obj2)

   #-----------------------------------------------------
   # description:
   #    The function tries to apply the 'equal' function
   #    corresponding to the given parameter <obj1>.
   #
   #    If for example <obj1> is of type 'region',
   #    the function applies the method 
   #       equal.region (obj1, obj2)
   #    and returns the result of the comparison.
   #
   #    This function only works properly if a method
   #    'equal' is defined for <obj1>.
   #
   # author: M. Erdelmeier
   #-----------------------------------------------------

   #-----------------------------------------------------
   # input/output-variables:    #-----------------------------------------------------

   # name | type   | I/O | description
   #---------------------------------------------------------------------
   # obj1 | object |  I  | object for comparison
   # obj1 | object |  I  | object for comparison


   #-----------------------------------------------------
   # used objects
   #-----------------------------------------------------

   # name        | type         | R/W | description
   #---------------------------------------------------------------------


   #-----------------------------------------------------
   # local variables
   #-----------------------------------------------------

   # name     | type    | description
   #-----------------------------------------------------------------


   #-------------------------------------------------------
   # programming part
   #-------------------------------------------------------

{
   # use 'equal' method belonging to <obj1>
   UseMethod("equal", obj1, obj2)
}

#-------------------------------------------------------
# Obscure: to remove unobserved data from sample objects
#-------------------------------------------------------

obscure<-function(x,...) {UseMethod("obscure")}




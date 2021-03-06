generate.density<-function (reg, nint.x = 100, nint.y = 100, southwest = 1, southeast = 1, 
    northwest = 1) 
{
    if (!is.region(reg)) 
        stop("\n*** The parameter <reg> must be of type 'region'.\n")
    if (!is.numeric(nint.x) | !is.numeric(nint.y)) 
        stop("\n<nint.x> and <nint.y> must be an integer value.")
    if ((nint.x != as.integer(nint.x)) | (nint.y != as.integer(nint.y))) 
        stop("\n<nint.x> and <nint.y> must be an integer value.")
    if ((nint.x <= 0) | (nint.y <= 0)) 
        stop("\n<nint.x> and <nint.y> must be at minimum one.")
    if (!is.numeric(southwest) | !is.numeric(southeast) | !is.numeric(northwest)) 
        stop("\nAll corner values must be numerical.")
    if ((southwest < 0) | (southeast < 0) | (northwest < 0)) 
        stop("\nAll corner values must be zero or greater.")
    parents<-list(wisp.id(reg,newname=as.character(substitute(reg))))
    dx <- seq(0, 1, length = nint.x)
    dy <- seq(0, 1, length = nint.y)
    dm <- matrix(southwest, nrow = nint.x, ncol = nint.y)
    dm <- dm + dx * (southeast - southwest)
    dm <- dm + rep(dy, rep(nint.x, nint.y)) * (northwest - southwest)
    if (min(dm) < 0) 
        stop("\nThe given corner values lead to negative values in density.")
    density <- list(reg=reg, n.interval.x = nint.x, n.interval.y = nint.y, matrix = dm, parents=parents, created=date())
    class(density) <- "density.population"
    return(density)
}

 genden.data<-function (reg, xylocations, nint.x = 100, nint.y = 100, gam.param=1) 
#   Arguments:
#			reg - study region
#			xylocations - dataframe containing x, y where groups were located
#			nint.x, nint.y - increments for constructing the prediction grid mesh
#			gam.param - gamma of Simon's mgcv to allow oversmoothing  
#
#			Rexstad--July 2008 for poster associated with ISEC conference
{
		require(mgcv)
    if (!is.region(reg)) 
        stop("\n*** The parameter <reg> must be of type 'region'.\n")
    if (!is.numeric(nint.x) | !is.numeric(nint.y)) 
        stop("\n<nint.x> and <nint.y> must be an integer value.")
    if ((nint.x != as.integer(nint.x)) | (nint.y != as.integer(nint.y))) 
        stop("\n<nint.x> and <nint.y> must be an integer value.")
    if ((nint.x <= 0) | (nint.y <= 0)) 
        stop("\n<nint.x> and <nint.y> must be at minimum one.")
    if (names(xylocations)[1] != "x" & names(xylocations)[2] != "y") 
        stop("\nData frame of locations must contain only x and y")
    parents<-list(wisp.id(reg,newname=as.character(substitute(reg))))
    cat("With >100 x-y coordinates, expect this to take ~1 minute\n")
    
#		Relativize x,y locations to the unit square based on dimensions of 'region'    
		xylocations$x <- xylocations$x / reg$length
		xylocations$y <- xylocations$y / reg$width
		xylocations$group <- 1			#	response variable to be modelled by the gam
#			Manufacture locations where groups are *not* located (equal in number to detects)
	  num.locations <- length(xylocations$x)
		absences <- data.frame(group=rep(0,num.locations), 
														x=runif(num.locations), y=runif(num.locations))
#				Merge group locations with group absences for dataframe to model														
		presence.absence <- rbind(xylocations, absences)														
		fitted.gam <- gam(group ~ s(x,k=15)+s(y,k=15)+s(x,y,k=45), gamma=gam.param, data=presence.absence, family=poisson())
#            build prediction grid
    pred.grid <- expand.grid(x = seq(0, 1, length = nint.x), y = seq(0, 1, length = nint.y))
		density.matrix <- predict.gam(fitted.gam, newdata=pred.grid, type="response")
		density.matrix <- matrix(density.matrix, ncol=nint.y, nrow=nint.x)
    if (min(density.matrix) < 0) 
        stop("\nNegative values in density generated by the gam fit.")
    density <- list(reg=reg, n.interval.x = nint.x, n.interval.y = nint.y, 
										matrix = density.matrix, parents=parents, created=date(), gam.model=fitted.gam)
    class(density) <- "density.population"
    return(density)
}

add.hotspot<-function (dens, x, y, altitude, sigma) 
{ #removed reg from argument list
    if (!is.density.population(dens)) 
        stop("\nThe parameter <dens> is not of type 'density.population'.\n")
#    if (!is.region(reg)) 
#        stop("\nThe parameter <reg> is not of type 'region'.\n")
    length <- dens$reg$length
    width <- dens$reg$width
    nx <- dens$n.interval.x
    ny <- dens$n.interval.y
    if (!is.numeric(x) | !is.numeric(y)) 
        stop("\nThe x/y-position must be numeric.\n")
    if ((x < 0) | (y < 0) | (x > length) | (y > width)) 
        stop("\nThe x/y-position must be inside the region.\n")
    if (!is.numeric(altitude)) 
        stop("\nThe altitude must be numeric.\n")
    if (!is.numeric(sigma)) 
        stop("\n<sigma> must be numeric.\n")
    if (sigma <= 0) 
        stop("\nsigma must be greater then zero.\n")
    xcoords <- rep(((0:(nx - 1)) + 0.5) * length/nx, rep(ny, 
        nx))
    ycoords <- rep(((0:(ny - 1)) + 0.5) * width/ny, nx)
    coords <- matrix(0, nrow = nx * ny, ncol = 2)
    coords[, 1] <- xcoords
    coords[, 2] <- ycoords
    distance <- sqrt((x - coords[, 1])^2 + (y - coords[, 2])^2)
    distance <- matrix(distance, ncol = ny, byrow = TRUE)
    hs <- dnorm(distance, mean = 0, sd = sigma)
    hs.max <- dnorm(0, mean = 0, sd = sigma)
    hs <- hs/hs.max
    hs <- hs * altitude
    density <- dens$matrix + hs
    if (any(density < 0)) 
        stop("\nThe given data leads to negative density values.\n")
    dens$matrix <- density
    dens$created=date()
    return(dens)
}


set.stripe<-function (dens, x1, y1, x2, y2, value = 0, width = 1) 
{ #removed reg from argument list
    if (!is.density.population(dens)) 
        stop("\nThe parameter <dens> is not of type 'density.population'.\n")
#    if (!is.region(reg)) 
#        stop("\nThe parameter <reg> is not of type 'region'.\n")
    r.length <- dens$reg$length
    r.width <- dens$reg$width
    r.nx <- dens$n.interval.x
    r.ny <- dens$n.interval.y
    r.density <- dens$matrix
    if (!is.numeric(value)) 
        stop("\nThe given density value must be numeric.\n")
    if (value < 0) 
        stop("\nThe density value cannot be negative.\n")
    if (!is.numeric(x1) | !is.numeric(y1) | !is.numeric(x2) | 
        !is.numeric(y2)) 
        stop("\nThe stripe coordinates must be numeric.\n")
    if ((x1 == x2) & (y1 == y2)) 
        stop("\nThe stripe coordinates must be different.\n")
    if (!is.numeric(width)) 
        stop("\nThe line width must be numeric.\n")
    if (width <= 0) 
        stop("\nThe line width must be greater than zero.\n")
    vx <- rep(((0:(r.nx - 1)) + 0.5)/r.nx * r.length, r.ny)
    vy <- rep(((0:(r.ny - 1)) + 0.5)/r.ny * r.width, rep(r.nx, 
        r.ny))
    cells.x <- matrix(vx, nrow = r.nx)
    cells.y <- matrix(vy, nrow = r.nx)
    p1 <- x1
    p2 <- y1
    n1 <- (y2 - y1)
    n2 <- -(x2 - x1)
    a <- 1/sqrt(n1^2 + n2^2)
    n1 <- a * n1
    n2 <- a * n2
    d <- abs(n1 * (cells.x - p1) + n2 * (cells.y - p2))
    inside1 <- d <= (0.5 * width)
    p1 <- 0.5 * (x1 + x2)
    p2 <- 0.5 * (y1 + y2)
    n1 <- x2 - x1
    n2 <- y2 - y1
    a <- 1/sqrt(n1^2 + n2^2)
    n1 <- a * n1
    n2 <- a * n2
    d <- abs(n1 * (cells.x - p1) + n2 * (cells.y - p2))
    length <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
    inside2 <- d <= (0.5 * length)
    inside <- inside1 & inside2
    if (!any(inside)) 
        cat("\nwarning: no part of the region is set to zero.\n")
    r.density <- ifelse(inside == TRUE, value, r.density)
    dens$matrix <- r.density
    dens$created=date()
    return(dens)
}



summary.density.population<-function(dens)
{
# check class:
 if (!is.density.population(dens)) stop("\nThe parameter <dens> must be of type 'density.population'.\n")
 cat("\n")
 cat("DENSITY SURFACE SUMMARY\n")
 cat("-----------------------\n")
 cat("creation date   :", dens$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(dens$parents)) {
   cat("      ",paste("(",dens$parents[[i]]$class,", ",dens$parents[[i]]$name,", ",dens$parents[[i]]$created,")",sep=""),"\n")
 }
 cat("\n")
 cat("Grid resolution (length; width):", dens$n.interval.x,";",dens$n.interval.y, "\n")
 cat("Mimimum (relative) height      :", min(dens$matrix), "\n")
 cat("Maximum (relative) height      :", max(dens$matrix), "\n")
}




plot.density.population<-function (dens, method = "persp", eye.horiz=330, eye.vert=35, 
    ...) 
{
    if (!is.density.population(dens)) 
        stop("\nThe parameter <dens> is not of type 'density.population'.\n")
# deleted reg from argument list
#    if (!is.region(reg)) 
#        stop("\nThe parameter <reg> is not of type 'region'.\n")
    if (!any(method == c("persp", "image"))) 
        stop("\nThe plot method must be <persp> or <image>.\n")
    if (!is.numeric(eye.horiz) | !is.numeric(eye.vert)) 
        stop("\nThe eye parameter must be numeric.\n")
    if ((eye.horiz < 0) | (eye.vert < 0) | (eye.horiz > 360) | 
        (eye.vert > 360)) 
        stop("\nThe eye parameter must be between 0 and 360 degrees.\n")
    if (method == "persp") 
        persp.density.population(dens, dens$reg, eye.horiz = eye.horiz, 
            eye.vert = eye.vert, ...)
    if (method == "image") 
        image.density.population(dens, dens$reg, ...)
}


image.density.population <- function (dens, reg, reset.par=TRUE, ...) 
{
   # test if <dens> is an object of type "density.population"
   if (!is.density.population(dens))
      stop ("\nThe parameter <dens> is not of type 'density.population'.\n")

   # test if <reg> is an object of type "region"
   if (!is.region(reg))
      stop ("\nThe parameter <reg> is not of type 'region'.\n")

   # get important information
   nx <- dens$n.interval.x
   ny <- dens$n.interval.y
   len <- reg$length
   width <- reg$width

   # calculate the x- and y-coordinates of the centers of cells
   # of the density grid
   x <- ( ( (0:(nx-1)) + 0.5 ) / nx ) * len
   y <- ( ( (0:(ny-1)) + 0.5 ) / ny ) * width

   # store graphical parameter
   par.was <- par(no.readonly=TRUE)

   # calculate dimensions of plot borders (ratio corresponding to
   # length and width of region, but appropriate to plot area)
   margin <- calculate.plot.margin (reg.x=len, reg.y=width,
             area.x=par.was$fin[1], area.y=par.was$fin[2])

#   # set plot margins
   par (mai=c(margin$y, margin$x, margin$y, margin$x))

   # plot the current density
   z <- dens$matrix
   max.color <- max(10, min(1000, nx+ny))
   image(x, y, z, xlab = "", ylab = "", col = heat.colors(max.color)[max.color:1],...)
   # restore graphical parameter
   if(reset.par) par(par.was)
}


#===========================================================
# is.density.population 
#===========================================================

is.density.population <- function (dens) 

   #----------------------------------------------------------------
   # description:
   #   tests if the given object <dens> if of type 
   #   "density.population".
   #
   # author: Martin Erdelmeier
   #----------------------------------------------------------------

{
    ## test if <dens> is of the type "density.population"
    inherits(dens, "density.population")
}


#===========================================================
# persp.density.population
#===========================================================

persp.density.population <- function (dens, reg, eye.horiz, eye.vert,...) 

   #--------------------------------------------------------------
   # description:
   #    The given density is plotted as a perspective image.
   #    
   #
   # author: Martin Erdelmeier
   #-----------------------------------------------------------------


   #----------------------------------------
   # input/output-variables
   #----------------------------------------

   # name   | type       | I/O | description
   #-----------------------------------------------------------------
   # dens   | dens.pop.- |  I  | the density that shall be plotted
   #        | object     |     | (in degrees)
   # eye.   | real       |  I  | horizontal rotation of plot
   # horiz  |            |     | (in degrees)
   # eye.   | real       |  I  | vertical rotation of plot
   # vert   |            |     |
   # reg    | region-    |  I  | region whose density shall be plotted
   #        | object     |     |


   #----------------------------------------
   # local variables
   #----------------------------------------

   # name      | type    | description
   #-----------------------------------------------------------------
   # len       | real    | length of the region
   # nx        | int     | number of x-intervals in density
   # ny        | int     | number of y-intervals in density
   # width     | real    | width of the region
   # x         | real    | vector with x-coordinates of the density grid
   # y         | real    | vector with y-coordinates of the density grid
   # z         | matrix  | density-values
   #           | of real |
   # z.scaling | real    | scaling factor so that z axis is "well scaled"
   #           |         | in comparison to x and y axis

{
   #----------------------------------------
   # programming part
   #----------------------------------------

   # test if <dens> is an object of type "density.population"
   if (!is.density.population(dens))
      stop ("\nThe parameter <dens> is not of type 'density.population'.\n")

   # test if <reg> is an object of type "region"
   if (!is.region(reg))
      stop ("\nThe parameter <reg> is not of type 'region'.\n")

   # get important information
   nx <- dens$n.interval.x
   ny <- dens$n.interval.y
   len <- reg$length
   width <- reg$width
   
   # calculate the x- and y-coordinates of the centers of cells
   # of the density grid 
   x <- ( ( (0:(nx-1)) + 0.5 ) / nx ) * len
   y <- ( ( (0:(ny-1)) + 0.5 ) / ny ) * width
   z <- dens$matrix

   # plot the current density of the region (x and y axis is plotted
   # with right scale. z is transformed to maximum of length and width
   # of the region)
   z.scaling <- max(len, width)/max(z)
   persp(x, y, z, xlab = "x", ylab = "y", zlab="Density", zlim=c(0, max(z)), box=TRUE, theta=eye.horiz, phi=eye.vert,scale=FALSE, expand=z.scaling,...)
}



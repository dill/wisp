plot.density.sample.3d <- function(density, sampled=NULL, scale.fact=0.5)
{
#			Purpose:		
#					fancy (rotatable 3 dimensional) plotting of populations and samples created by Wisp
#			Arguments:
#				density - 	an object of type 'density.population'
#				sampled -		object created by sampling using either line, point, or mark-recapture
#										if this argument is NULL, then only the 3d surface is drawn
#				scale.fact -	magnification factor for the sizes of the spheres representing animal locations

#		Check that arguments are of necessary type, return otherwise
	if (class(density) != "density.population") {
			stop("First argument is not a density object") }
	if (!is.null(sampled)) {
		if (!(class(sampled) %in% c("sample.cr","sample.lt", "sample.pt"))) {
				stop("Second argument is not a mark-recapture or distance sample") }
	}
		
	require(rgl)
	rgl.open()
# 	Clear scene:
  clear3d("all")               # remove all shapes, lights, bounding-box, and restore viewpoint
  
# 	Setup environment:
  bg3d(col="#cccccc")     # setup background
  light3d()               # setup head-light
  
# 	Parse information about density surface from density object
  region.length <- density$reg$length
  region.width <-  density$reg$width
  terrain<-density$matrix
  
# 	Define colors for terrain
  zlim <- range(terrain)
  zlen <- zlim[2] - zlim[1] + 1
  colorlut <- terrain.colors(82) 
  col1 <- colorlut[9*sqrt(3.6*(terrain-zlim[1])+2)]
  
#		 Set color to (water-)blue for regions with zero 'altitude' 
  col1[terrain==0]<-"#0000FF"
  
# 	Add terrain surface shape (i.e. population density):
  surface3d( 
      1:region.length,seq(1,region.width, length.out=dim(terrain)[2]),terrain,
      col=col1,spec="#000000", ambient="#333333", back="lines"
  )
  if (!is.null(sampled)) {
#			Parse information about individual animals from sampled object
	  location.x <- sampled$population$posx
	  location.y <- sampled$population$posy
	  exposure <- sampled$population$exposure
		if (class(sampled) == "sample.cr") {
		  caught <- rowSums(sampled$capture) / sampled$design$number.occasions
	  } else {
			detected <- sampled$detected
	  }
		caught.color <-  vector(mode="character",length(exposure))

# Define colors for simulated populations 
#		(based on proportion of occasions	they were captured)
		if (class(sampled) == "sample.cr") {
			for (i in 1:length(caught)) {
				if (caught[i] > 0.75) { caught.color[i] <- "#ff0000"
				} else 
					if (caught[i] > 0.50) { caught.color[i] <- "#00ff00"
				} else 
					if (caught[i] > 0.) { caught.color[i] <- "#0000ff"
				} else { caught.color[i] <- "#000000" }
			}
			} else {  # a distance sample
				for (i in 1:length(exposure)) {
					if (is.na(detected[i])) { caught.color[i] <- "#0000ff"
					} else {
					if (detected[i] == 1) { caught.color[i] <- "#ff0000" 
				} else { caught.color[i] <- "#000000" }
				}
				}
			}	
	  
#   Add simulated populations as sphere-set shape
		funny.factor <- dim(terrain)[2]/region.width
	  spheres3d(
	    location.x,
	    location.y,
	    terrain[cbind( ceiling(location.x),ceiling(location.y*funny.factor) )]+0.5,
	    radius=scale.fact*exposure, col=caught.color#, alpha=(1-(pop[,5])/10 )
	  )
	
		big.title <- paste("Wisp simulated popn. density ", 
												match.call()[2], " sampled by ", 
												match.call()[3], sep="")
			if (class(sampled) == "sample.cr") {
		sub.title <- "Black=never caught, Red=caught>75% occasions, Green=caught>50%, Blue=caught<50%"
		} else {
			sub.title <- "Blue=outside covered region, Red=detected, Black=not detected in covered region; radius=exposure"
		}
		title3d(main=big.title, sub=sub.title)
		rgl.viewpoint(theta=0, phi=-60, fov=40, zoom=1, scale=par3d("scale"), interactive=TRUE)
	}
}
#--------------------------------------------------------------------------------------------------------
transform.to.rmark <- function(sample)
{
#		Purpose:  transform Wisp sample object, to form for analysis with RMark (by Laake)
#		Argument
#				sample -	object of type 'sample.cr' created by Wisp
#		Returns
#				a dataframe with field named 'ch' of time character containing capture histories
cap.history <- sample$capture
f.sub.i <- rowSums(cap.history)
captured.animals <-  cap.history[ f.sub.i != 0, ]
captured.animals.compact <- as.character(apply(captured.animals, 1, paste, collapse=""))
return( data.frame(ch=I(captured.animals.compact)))
}

#----------------------------------------------------------------------------------

# -------------------------------- start of functions --------------------------------

import.rm=function(catch)
#----------------------------------------------------------------------------------------
# Takes a vector of catch frequencies for each capture occasion (catch) and creates a
# WiSP object of class sample.rm corresponding to these capture histories.
# Does so by creating a default object and filling in the capture data in the matrices
# $detected (capture histories) and $removal (occasions by which animals were removed).
#----------------------------------------------------------------------------------------
# dlb March 2006
#----------------------------------------------------------------------------------------
{
# get number occasions and total catch:
  n.occ=length(catch)
  ntot=max(sum(catch),1)
# make a default region, density and population
  reg<-generate.region()
  dens=generate.density(reg)
# make a default population of minimum size ntot
  poppars<-setpars.population(dens, number.groups=ntot)
  pop<-generate.population(poppars)
# make default design with right number of occasions
  des<-generate.design.rm(reg, n.occ=n.occ)
# and survey paramters (pmin is as close to 1 as possible so that all ntot are detected):
  survpars<-setpars.survey.rm(pop=pop, des=des, pmin=0.99999999999999)
# finally sample:
  samp<-generate.sample.rm(survpars)
  osamp=obscure(samp)
# now make the capture matrices "detected" and "removal":
  detected=removal=matrix(0, nrow=ntot, ncol=n.occ)
  ndone=0
  for (j in 1:n.occ) {
    if(catch[j]>0) {
      for(i in (ndone+1):(ndone+catch[j])) {
        detected[i,j]=1                # a 1 in column for the occasion on which the animal was captured
        if(j<n.occ) removal[i,(j+1)]=1 # a 1 in column for the occasion by which the animal was removed
      }
      ndone=ndone+catch[j]
    }
  }
# put detected and removal into the sample.rm object osamp:
  osamp$detected=detected
  osamp$removal=removal
# and return it:
  return(osamp)
}
#-------------------------------------------------------------------------------------------
 sample.lt.to.ddf <- function(the.sample)
 {
 #		Purpose:  convert a wisp object of type sample.lt into format acceptable to ddf
 #							NB: ddf can't handle point transect data, so no need to tolerate sample.pt objects
 #									not currently written to handle class 'sample.dp' but ought to do so
 #
 #		Arguments:
 #				the.sample -	object of class 'sample.lt'
 #		Value:
 #				data.frame containing following fields:
 #					object -		detected object number
 #					observer -	who made the detection  (must be observer 1 for the moment)
 #					detected -	must be 1 (for now)
 #					distance -	distance of object from transect
 #					size -			group size
 #					sex -				sex of cluster (species exhibits sexual segregation)
 #					exposure -	detectability of group
    if (!class(the.sample) == "sample.lt") {
    	stop("Only sample objects derived from line transect samples can be converted for ddf")
    }
    n.transects <- the.sample$design$n.transects  
    transect.x <- the.sample$design$pos.x[1:n.transects]
    n.groups <- length(the.sample$population$posx)
    visual <- the.sample$design$visual.range
    
    detectable <- rep(FALSE, n.groups)
    distance <- rep(NA, n.groups)
    transect <- rep(NA, n.groups)
    exposure <- rep(NA, n.groups)
    sex <- rep(NA, n.groups)
    sequential.transect.number <- 0

    for (itransect in 1:n.transects) {
      w <- abs(the.sample$population$posx - transect.x[itransect])
      group.covered <- (w < visual)
      detectable[group.covered] <- TRUE
      distance[group.covered] <- w[group.covered]
      transect[group.covered] <- itransect
    }
    p.detect <- rep(0,n.groups)
    p.detect[detectable] <- detection.transectmethods(distance[detectable], 
                            the.sample$population$exposure[detectable], 
														eval(parse(text=paste(the.sample$parents[[1]]$name,"$theta",sep=""))))
    detected <- rbinom(n.groups, 1, p.detect)
    detected[!detectable] <- NA
    momento <- data.frame(distance, size=the.sample$population$groupsize,
                          sex=the.sample$population$types, 
				  exposure=the.sample$population$exposure, seen=detected)
    momento <- subset(momento, seen==1, select=-seen)
    momento$object <- seq(1, length(momento$distance))
    momento$observer <- 1
    momento$detected <- 1
    momento
 }

 dp.to.ddf <- function(the.sample) {
    if (!class(the.sample) == "sample.dp" ) {
        stop("Only sample objects derived from double platform samples can be converted for ddf")
    }
	distance <- the.sample$distance
	size <- the.sample$population$groupsize
	sex <- the.sample$population$types
	exposure <- the.sample$population$exposure
	seen <- the.sample$detected
	obs1 <- the.sample$detected.observer1
	obs2 <- the.sample$detected.observer2
	result <- data.frame(distance, size, sex, exposure, seen, obs1, obs2)
	result <- subset(result, seen == 1, select = -seen)
	result$object <- seq(1, length(result$distance))
	result$detected <- 1
	test <- reshape(result, varying=list(c("obs1","obs2")), 
		          direction="long", timevar="observer", 
			    times=c(1,2), drop="detected")
	names(test)[7] <- "detected"
	return(test)
}

point.est.compare <- function(...) {
# Suggested function to compare and tabulate results from various models.
# Accepts almost anything, but only returns results for lists with
# $AIC, $Nhat.grp, $Nhat.ind, and $Es.
# Provided all objects have $AIC, AIC weights and weighted averages are
# calculated.
# Output is displayed (to 2 sig.fig.) and returned invisibly.

# Mike Meredith 4 June 07
   x <- list(...)
 # Get names:
   if (is.null(names(x))) names(x) <- "" 
   names(x)[names(x) == ""] <- NA 
   mc <- match.call()[-1] 
   nms <- ifelse(is.na(names(x)), as.character(mc), names(x)) 
 # Extract values:
   res <- matrix(NA, length(x), 6)
   for(i in seq_along(x)) 
      if(is.list(x[[i]])) {
         if(!is.null(x[[i]]$AIC)) res[i,1] <- x[[i]]$AIC
         if(!is.null(x[[i]]$Nhat.grp)) res[i,4] <- x[[i]]$Nhat.grp
         if(!is.null(x[[i]]$Nhat.ind)) res[i,5] <- x[[i]]$Nhat.ind
         if(!is.null(x[[i]]$Es)) res[i,6] <- x[[i]]$Es
   }
   res[,2] <- res[,1] - min(res[,1], na.rm=TRUE)    # deltaAIC
   Mod.llh <- exp(-res[,2]/2)
   res[,3] <- Mod.llh / sum(Mod.llh, na.rm=TRUE)
   WtAv <- colSums(res[,4:6] * res[,3])
   dimnames(res) <- list(nms, c("AIC", "deltaAIC", "AICweight", "Nhat.groups",
      "Nhat.individual", "Group size"))
   res <- res[order(res[,2]),]
   res1 <- rbind(res, "Weighted Av." = c(rep(NA,3), WtAv))
   if(is.na(sum(WtAv)))
      cat("AIC not given for all models; can\'t calculate weighted average\n")
   print(res1, digits=2, na.print=" ", print.gap=3)
   invisible(res1)
}


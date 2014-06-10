#Line transect method updates

setpars.design.lt<-function (reg, n.transects = 1, n.units = 1, visual.range, percent.on.effort = 1) 
{
    if (!is.region(reg)) 
        stop("\n*** The parameter <reg> is not of type 'region'.\n")
    if (!is.numeric(n.transects)) 
        stop("\n*** The number of transects must be numeric.\n")
    if (n.transects != as.integer(n.transects)) 
        stop("\n*** The number of transects must be of type integer.\n")
    if (n.transects < 1) 
        stop("\n*** The number of transects must be at least 1.\n")
    if (!is.numeric(n.units)) 
        stop("\n*** The number of units must be numeric.\n")
    if (n.units != as.integer(n.units)) 
        stop("\n*** The number of units must be of type integer.\n")
    if (n.units < 1) 
        stop("\n*** The number of units must be at least 1.\n")
    if ((n.units/n.transects) != as.integer(n.units/n.transects)) 
        stop("\n*** n.units must be a multiple of n.transects.\n")
    if (!is.numeric(visual.range)) 
        stop("\n*** The visual range must be numeric.\n")
    if (visual.range < 0) 
        stop("\n*** The visual range cannot be negative.\n")
    if (!is.numeric(percent.on.effort)) 
        stop("\n*** The value 'percent.on.effort' must be numeric.\n")
###		Clarification about percentage and proportions Mike Merridith June 2007
#    if ((percent.on.effort < 0) | (percent.on.effort > 1)) 
#        stop("\n*** The value 'percent.on.effort' must be inside [0, 1].\n")
### Allow values <=100; if >1, divide by 100 to convert to proportion
    if ((percent.on.effort < 0) | (percent.on.effort > 100)) 
        stop("\n*** The value 'percent.on.effort' must be inside [0, 100].\n")
    if (percent.on.effort > 1)
        percent.on.effort <- percent.on.effort / 100 
### Still assumes that <=1 means proportion, so if you _do_ want 0.5%...!! (MM 1 June 07)
    if ((percent.on.effort < 0) | (percent.on.effort > 100)) 
        stop("\n*** The value 'percent.on.effort' must be inside [0, 100].\n")
    if (2 * visual.range * n.transects >= reg$length) 
        stop("\n*** The visual range of neighboured paths are superposed.\n")
    parents<-list(wisp.id(reg,newname=as.character(substitute(reg))))
    pars.design.lt <- list(region = reg, n.transects = n.transects, n.units = n.units, 
       visual.range = visual.range, percentage.on.effort = percent.on.effort, parents=parents, created=date())
    class(pars.design.lt) <- "pars.design.lt"
    return(pars.design.lt)
}


is.pars.design.lt<-function (despars) 
{
 inherits(despars,"pars.design.lt")
}



summary.pars.design.lt<-function(des, digits=5)
{
# check class:
 if (!is.pars.design.lt(des)) stop("\nThe parameter <des> must be of class 'pars.design.lt'.\n")
 cat("\n")
 cat("LINE TRANSECT DESIGN PARAMETER SUMMARY\n")
 cat("--------------------------------------\n")
 cat("creation date   :", des$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(des$parents)) {
   cat("      ",paste("(",des$parents[[i]]$class,", ",des$parents[[i]]$name,", ",des$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(des$seed)) cat("random number seed used: ",des$seed,"\n")
 cat("\n")
 A<-des$reg$length*des$reg$width
 a<-des$n.transects*2*des$visual.range*des$reg$width*des$percentage.on.effort
 cat("Number of transects              :", des$n.transects,"\n")
 cat("Number of units                  :", des$n.units,"\n")
 cat("Strip half-width                 :", des$visual.range,"\n")
 cat("Percentage of transects on effort:", signif(100*des$percentage.on.effort,digits),"%\n")
 cat("Covered area                     :", a,"\n")
 cat("Coverage probability             :", signif(a/A,digits),"\n")
 cat("\n")
 cat("Region dimensions (length x width):", des$reg$length, "x", des$reg$width, "\n")
}



plot.pars.design.lt<-function(x,col="black")
{
 plot.text("There is no useful plot for this class of object",col=col)
}



generate.design.lt<-function (pars.design.lt, seed=NULL) 
{
 if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) stop("\nThe parameter <seed> must be a number or a WiSP object.\n")
 if (is.wisp.class(seed)) {
   if (is.element("seed",names(seed))) {
     if(!is.null(seed$seed)) {
       seed<-seed$seed
       set.seed(seed) # use seed stored in passed wisp object
     }
   }
 }
 if(is.numeric(seed)) set.seed(seed)
    parents<-list(wisp.id(pars.design.lt,newname=as.character(substitute(pars.design.lt))))
    pars <- pars.design.lt
    reg <- pars$region
    n.transects <- pars$n.transects
    n.units <- pars$n.units
    range <- pars$visual.range
    covered <- pars$percentage.on.effort
    length <- reg$length
    width <- reg$width
    dx <- length/n.transects
    startx<-runif(1, 0, dx)
    ntrans<-n.transects
    nunits<-n.units
    paths.x <- (0:(n.transects-1)) * dx + startx
    if(startx<range || startx>=(dx-range)) {   # deal with transects that overlap edge of region
      paths.x <- (0:n.transects) * dx + startx #(need extra transect 'cause overlap edge)
      ntrans<-n.transects+1
      nunits<-n.units+1
    }
    if(startx>(dx-range)) paths.x<-paths.x-dx
    units.per.path <- n.units/n.transects
    units.x <- rep(paths.x, rep(units.per.path, ntrans))
    unit.len <- covered * (width/units.per.path)
    displacement <- runif(1, 0, (width/units.per.path - unit.len))
    unit.start.up <- width * (0:(units.per.path - 1))/units.per.path + displacement
    unit.end.up <- unit.start.up + unit.len
    unit.start.down <- width - unit.start.up
    unit.end.down <- width - unit.end.up
    units.start <- matrix(0, nrow=ntrans, ncol = units.per.path)
    units.end <- matrix(0, nrow=ntrans, ncol = units.per.path)
    for (i in 1:ntrans) {
        if ((i/2) == as.integer(i/2)) {
            units.start[i, ] <- unit.start.down
            units.end[i, ] <- unit.end.down
        }
        else {
            units.start[i, ] <- unit.start.up
            units.end[i, ] <- unit.end.up
        }
    }
    units.start <- as.vector(t(units.start))
    units.end <- as.vector(t(units.end))
    des <- list(region = reg, n.transects = ntrans, effective.n.transects = n.transects, n.units = nunits, 
        pos.x = units.x, start.y = units.start, end.y = units.end, 
        visual.range = range, percentage.on.effort=pars.design.lt$percentage.on.effort, parents=parents, created=date(), seed=seed)
    class(des) <- "design.lt"
    return(des)
}

is.design.lt <- function (des) 
{
 inherits(des, "design.lt")
}


summary.design.lt<-function(des, digits=5)
{
# check class:
 if (!is.design.lt(des)) stop("\nThe parameter <des> must be of class 'design.lt'.\n")
 cat("\n")
 cat("LINE TRANSECT DESIGN SUMMARY\n")
 cat("----------------------------\n")
 cat("creation date   :", des$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(des$parents)) {
   cat("      ",paste("(",des$parents[[i]]$class,", ",des$parents[[i]]$name,", ",des$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(des$seed)) cat("random number seed used: ",des$seed,"\n")
 cat("\n")
 A<-des$reg$length*des$reg$width
 a<-des$effective.n.transects*2*des$visual.range*des$reg$width*des$percentage.on.effort
 cat("Number of transects              :", des$n.transects,"\n")
 cat("Number of units                  :", des$n.units,"\n")
 cat("Strip half-width                 :", des$visual.range,"\n")
 cat("Percentage of transects on effort:", signif(100*des$percentage.on.effort,digits),"%\n")
 cat("Covered area                     :", a,"\n")
 cat("Coverage probability             :", signif(a/A,digits),"\n")
 cat("\n")
 cat("Region dimensions (length x width):", des$reg$length, "x", des$reg$width, "\n")
}


plot.design.lt<-function (des, show.paths = FALSE) 
{
 if (!is.design.lt(des)) stop("\n*** <des> must be of type 'design.lt'\n")
 old.par<-par(no.readonly=TRUE)
 plot.region(des$region, reset.pars=FALSE)
 plot.strips.lt(des, show.paths=show.paths)
 par(old.par)
}


plot.strips.lt<-function (des, show.paths = FALSE) 
#-----------------------------------------------------------------------------
# Plots survey strips. You should call plot.region() with reset.pars=FALSE
# once before calling this function, to draw the region and get the correct
# aspect ratio for the plot.
#-----------------------------------------------------------------------------
{
    if (!is.design.lt(des) & !is.design.dp(des)) stop("\n*** <des> must be of type 'design.lt' or 'design.dp'\n")
    if ((show.paths != TRUE) & (show.paths != FALSE)) stop("\n*** <show.paths> must be TRUE or FALSE.\n")
    reg <- des$region
    pos.x <- des$pos.x
    start.y <- des$start.y
    end.y <- des$end.y
#    range <- des$visual.range
#    left <-pos.x - range
#    right<-pos.x + range
    range <- des$visual.range
    left <- pos.x - range
    right <- pos.x + range
    #get region data
    len <- des$region$length
    wid <- des$region$width
    left[left<0]<-0
    right[right>des$region$length]<-des$region$length
    rect(left, start.y, right, end.y, col = "aquamarine", 
        border = par("fg"))
    if (show.paths == T) {
        path.x <- unique(pos.x)
        n.path <- length(path.x)
        path.start <- rep(0, n.path)
        path.end <- rep(0, n.path)
        for (i in 1:n.path) {
            if ((i%%2) == 1) {
                path.start[i] <- 0 - wid * 0.1
                path.end[i] <- wid + wid * 0.1
            }
            else {
                path.start[i] <- wid + wid * 0.1
                path.end[i] <- 0 - wid * 0.1
            }
        }
        arrows(path.x, path.start, path.x, path.end, lty = 2, 
            xpd = T, code = 2)
    }
#    par(new = par.was$new)
}




setpars.survey.lt<-function (pop, des, disthalf.min, disthalf.max, model="half.normal") 
{
 if(model!="half.normal") stop("Argument <> must be 'half.normal' (other methods not yet implemented)")
    if (!is.population(pop)) 
        stop("\n*** The parameter <pop> must be of type 'population'.\n")
    if (!is.design.lt(des)) 
        stop("\n*** The parameter <des> must be of type 'design.lt'.\n")
    if (!equal(pop$region, des$region)) 
        stop(paste("\n*** The given population and design were defined", 
            "with different regions.\n"))
    min.exposure <- pop$minexposure
    max.exposure <- pop$maxexposure
    if (!is.numeric(disthalf.min) | !is.numeric(disthalf.max)) 
        stop("\n<disthalf.min> and <disthalf.max> must be numeric.\n")
    if ((disthalf.min <= 0) | (disthalf.max <= 0)) 
        stop("\n<disthalf.min> and <disthalf.max> cannot be zero or less.\n")
    if (disthalf.min > disthalf.max) 
        stop("\n<disthalf.min> cannot be greater than <disthalf.max>.\n")
    if ((min.exposure == max.exposure) & (disthalf.min != disthalf.max)) 
        stop(paste("\nExposure boundaries are equal. Therefore", 
            "<disthalf.min> and <disthalf.max> cannot be different.\n"))
 if(model=="half.normal") {
  if (min.exposure == max.exposure) theta1 <- 0
  if (min.exposure != max.exposure) 
    theta1 <- (log(disthalf.min)-log(disthalf.max))/(min.exposure-max.exposure)
  theta0 <- log(disthalf.min/sqrt(-2*log(0.5)))-theta1 * min.exposure
  if (is.na(theta0) | is.na(theta1)) 
    stop("\nParameters lead to invalid data.\n")
  if ((theta0 == Inf) | (theta1 == Inf)) 
    stop("\nParameters lead to invalid data.\n")
   theta<-c(theta0, theta1)
 }
 parents<-list(wisp.id(pop,newname=as.character(substitute(pop))), 
   wisp.id(des,newname=as.character(substitute(des))))
 pars.survey.lt <- list(population=pop, design=des, theta=theta, model=model, parents=parents, created=date())
 class(pars.survey.lt) <- "pars.survey.lt"
 return(pars.survey.lt)
}



detection.transectmethods<-function (distance, exposure, theta, model="half.normal") 
{
 p.detect<-switch(model,
  half.normal=exp(-0.5*distance^2/(exp(theta[1] + theta[2]*exposure))^2),
  hazard.rate=1 - exp(-(x/(exp(theta[1] + theta[3]*exposure)))^(-theta[2]))
 )
 return(p.detect)
}


is.pars.survey.lt<-function (pars) 
{
 inherits(pars, "pars.survey.lt")
}


summary.pars.survey.lt<-function(pars, digits=5, plot=FALSE) 
{
    if (!is.pars.survey.lt(pars)) 
        stop("\nThe parameter <pars> must be of type 'pars.survey.lt'.\n")
    cat("\n")
    cat("SURVEY PARS SUMMARY (LINE TRANSECT METHOD)\n")
    cat("------------------------------------------\n")
 cat("creation date   :", pars$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(pars$parents)) {
   cat("      ",paste("(",pars$parents[[i]]$class,", ",pars$parents[[i]]$name,", ",pars$parents[[i]]$created,")",sep=""),"\n")
 }
 cat("\n")
    w <- pars$design$visual.range
    K <- pars$design$effective.n.transects
    frac.L.in<-pars$design$effective.n.transects/pars$design$n.transects
    L <- sum(abs(pars$design$start.y - pars$design$end.y))*frac.L.in
    a <- 2 * w * L
    A <- pars$population$region$width * pars$population$region$length
 model<-pars$model
    cat("Truncation distance               :", w, "\n")
    cat("Number of lines                   :", K, "\n")
    cat("Total line length in survey region:", L, "\n")
    cat("Survey area                       :", A, "\n")
    cat("Covered area                      :", a, "\n")
    cat("Percentage of survey area covered :", 100 * a/A, "%\n")
    cat("\n")
    cat("Detection function: \n")
 if(model=="half.normal") {
    cat("------------------ \n")
    cat("  Half-normal model: \n")
    cat("    p(detect) = exp(-0.5 * distance^2/(exp(theta0 + theta1 * exposure))^2)\n")
    cat("  Parameters: \n")
    cat("    theta0 = ", pars$theta[1], "; theta1 = ", pars$theta[2], 
        "\n")
 }
    mu.minexp<-integrate(detection.transectmethods, lower=0, upper=w, exposure=pars$population$minexposure, 
                 theta=pars$theta)$value
    if (pars$population$minexposure != pars$population$maxexposure) {
      mu.maxexp<-integrate(detection.transectmethods, lower=0, upper=w, 
                   exposure=pars$population$maxexposure, theta=pars$theta)$value
    N<-length(pars$population$exposure)
    mu<-rep(0,N)
    for(i in 1:N) mu[i]<-integrate(detection.transectmethods, lower=0, upper=w, 
                   exposure=pars$population$exposure[i], theta=pars$theta)$value
    mu<-mean(mu)
      cat("\n")
      cat("Effective strip with at minimum exposure:", signif(mu.minexp,digits), "\n")
      cat("Effective strip with at maximum exposure:", signif(mu.maxexp,digits), "\n")
      cat("Mean effective strip with               :", signif(mu,digits), "\n")
    }else {
      cat("Effective strip with:", signif(mu.minexp,digits), "\n")
    }
 if(plot) plot(pars)
}




plot.pars.survey.lt<-function(pars)
{
 pars.old<-par(no.readonly=TRUE)
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 w<-pars$design$visual.range
 x <- seq(0, w, length = 100)
 minexp <- rep(pars$population$minexposure, 100)
 maxexp <- rep(pars$population$maxexposure, 100)
 fmax <- detection.transectmethods(x, maxexp, theta=pars$theta)
 plot(x, fmax, type = "l", ylim = c(0, 1), xlab = "Distance", ylab = "Detection probability", lwd=3*cex)
 if (pars$population$minexposure != pars$population$maxexposure) {
   fmin <- detection.transectmethods(x, minexp, theta=pars$theta)
   lines(x, fmin, lty = 2, lwd=3*cex)
   title("Detection functions for min and max exposure")
 }
 else {
   title("Detection function")
 }
 par(pars.old)
}



generate.sample.lt<-function (pars, seed=NULL) # change help function so 1st arg is "pars", not "pars.survey.lt"
{
 if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) stop("\nThe parameter <seed> must be a number or a WiSP object.\n")
 if (is.wisp.class(seed)) {
   if (is.element("seed",names(seed))) {
     if(!is.null(seed$seed)) {
       seed<-seed$seed
       set.seed(seed) # use seed stored in passed wisp object
     }
   }
 }
 if(is.numeric(seed)) set.seed(seed)
    parents<-list(wisp.id(pars,newname=as.character(substitute(pars))))
    pop <- pars$population
    des <- pars$design
    theta<-pars$theta
    pos.x <- pop$posx
    pos.y <- pop$posy
    exposure <- pop$exposure
    n.groups <- length(pos.x)
    unit.x <- des$pos.x
    start.y <- des$start.y
    end.y <- des$end.y
    range <- des$visual.range
    n.units <- des$n.units
    n.transects <- des$n.transects
    detectable <- rep(F, n.groups)
    distance <- rep(NA, n.groups)
    transect <- rep(NA, n.groups)
    for (i in 1:n.units) {
        if (start.y[i] <= end.y[i]) 
            inside <- (pos.y >= start.y[i]) & (pos.y <= end.y[i])
        if (start.y[i] > end.y[i]) 
            inside <- (pos.y <= start.y[i]) & (pos.y >= end.y[i])
        w <- abs(pos.x - unit.x[i])
        inside <- inside & (w <= range)
        detectable[inside] <- T
        distance[inside] <- w[inside]
        transect[inside] <- ceiling(i/(n.units/n.transects))
    }
    p.detect <- rep(0, n.groups)
    p.detect[detectable] <- detection.transectmethods(distance[detectable], exposure[detectable], 
       theta)
    detected <- rbinom(n.groups, 1, p.detect)
    detected[!detectable] <- NA
    samp <- list(population=pop, design=des, detected=detected, 
        distance=distance, transect=transect, parents=parents, created=date(), seed=seed)
    class(samp) <- "sample.lt"
    return(samp)
}



is.sample.lt <- function (samp) 
{
 inherits(samp, "sample.lt")
}

summary.sample.lt<-function (samp, digits=5) 
{
    if (!is.sample.lt(samp)) 
        stop("\nThe parameter <samp> must be of type 'sample.lt'.\n")
    cat("\n")
    cat("SAMPLE SUMMARY (LINE TRANSECT METHOD)\n")
    cat("---------------------------------------\n")
 cat("creation date   :", samp$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(samp$parents)) {
   cat("      ",paste("(",samp$parents[[i]]$class,", ",samp$parents[[i]]$name,", ",samp$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(samp$seed)) cat("random number seed used: ",samp$seed,"\n")
 cat("\n")
    w <- samp$design$visual.range
    K <- samp$design$effective.n.transects
    frac.L.in<-samp$design$effective.n.transects/samp$design$n.transects
    L <- sum(abs(samp$design$start.y - samp$design$end.y))*frac.L.in
    a <- 2 * w * L
    A <- samp$population$region$width * samp$population$region$length
    xi <- samp$distance[(samp$detected == 1) & !is.na(samp$detected)]
    zi <- samp$population$groupsize[(samp$detected == 1) & !is.na(samp$detected)]
    cat("Truncation distance:              ", w, "\n")
    cat("Number of lines:                  ", K, "\n")
    cat("Total line length:                ", L, "\n")
    cat("Survey area:                      ", A, "\n")
    cat("Covered area:                     ", a, "\n")
    cat("Percentage of survey area covered:", signif(100*a/A,digits), "%\n")
    cat("Number of groups detected:        ", length(xi), "\n")
    cat("Mean group size:                  ", signif(mean(zi),digits), "\n")
    cat("Mean perpendicular distance:      ", signif(mean(xi),digits), "\n")
}

plot.sample.lt<-function (samp, type="hist", show.sizes=TRUE, show.exps = TRUE, dsf=0.5, whole.population=FALSE, 
    show.paths=TRUE, ...) 
{
    if (!is.sample.lt(samp)) 
        stop("\n*** The parameter <samp> must be of type 'sample.lt'.\n")
    if (!is.numeric(dsf)) 
        stop("\n*** The parameter <dsf> must be numeric.\n")
    if (dsf <= 0) 
        stop("\n*** The parameter <dsf> must be positive.\n")
    if ((show.sizes != T) & (show.sizes != F)) 
        stop("\n*** The parameter <show.sizes> must be TRUE or FALSE.\n")
    if ((show.exps != T) & (show.exps != F)) 
        stop("\n*** The parameter <show.exps> must be TRUE or FALSE.\n")
    if ((whole.population != T) & (whole.population != F)) 
        stop("\n** The parameter <whole.population> must be TRUE or FALSE.\n")
    if ((show.paths != T) & (show.paths != F)) 
        stop("\n*** The parameter <show.paths> must be TRUE or FALSE.\n")
    pop <- samp$population
    des <- samp$design
    par.was <- par(no.readonly = T)
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 if(type=="hist") {
   xi <- samp$distance[(samp$detected == 1) & !is.na(samp$detected)]
   hist(xi, xlab = "Perpendicular Distance", ylab = "Frequency", main = "Perpendicular distance distribution")
 } else {
#    len <- pop$reg$length
#    width <- pop$reg$width
#    margin <- calculate.plot.margin(reg.x = len, reg.y = width, 
#        area.x = par.was$fin[1], area.y = par.was$fin[2])
#    par(mai = c(margin$y, margin$x, margin$y, margin$x))
#    plot(0, 0, type = "n", las = 1, xlab = "", ylab = "", xlim = c(0, 
#        len), ylim = c(0, width), xaxs = "i", yaxs = "i", xpd = T, 
#        ...)
 plot.region(des$region, reset.pars=FALSE)
 plot.strips.lt(des, show.paths=show.paths)
#    plot.design.lt(des, show.paths = show.paths, newplot = TRUE, xpd=TRUE, ...)
    if (whole.population == T) 
#        plot(pop, show.sizes = show.sizes, show.exps = show.exps, 
#            newplot = F, dsf = dsf, ...)
    plot.groups(pop, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="black")
    inside <- ((samp$detected == 1) & !is.na(samp$detected))
    seen <- pop
    seen$groupID <- pop$groupID[inside]
    seen$posx <- pop$posx[inside]
    seen$posy <- pop$posy[inside]
    seen$groupsize <- pop$groupsize[inside]
    seen$types <- pop$types[inside]
    seen$exposure <- pop$exposure[inside]
    plot.groups(seen, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="red")
#    plot(seen, show.sizes = show.sizes, show.exps = show.exps, 
#        newplot = FALSE, dsf = dsf, group.col = "red", details=FALSE, ...)
 }
 par(par.was)
}


obscure.sample.lt<-function(samp)
#----------------------------------------------------------------
# Removes all information about undetected animals from an object
# of class sample.lt. Returns object of same class.
#----------------------------------------------------------------
{
if (!is.sample.lt(samp)) 
  stop("\n*** <samp> is not an object of type 'sample.lt'.\n")
t<-samp
t$population$groupID<-samp$population$groupID[!is.na(samp$detected) & samp$detected==1]
t$population$posx<-samp$population$posx[!is.na(samp$detected) & samp$detected==1]
t$population$posy<-samp$population$posy[!is.na(samp$detected) & samp$detected==1]
t$population$groupsize<-samp$population$groupsize[!is.na(samp$detected) & samp$detected==1]
t$population$types<-samp$population$types[!is.na(samp$detected) & samp$detected==1]
t$population$exposure<-samp$population$exposure[!is.na(samp$detected) & samp$detected==1]
t$distance<-samp$distance[!is.na(samp$detected) & samp$detected==1]
t$transect<-samp$transect[!is.na(samp$detected) & samp$detected==1]
t$detected<-samp$detected[!is.na(samp$detected) & samp$detected==1]
t$created<-date()
t
}



point.est.lt<-function (sampl, plot=FALSE, title=TRUE, conditional=TRUE, model="half.normal") 
{
    if (is.sample.lt(sampl) | is.sample.dp(sampl)) {
        n <- sum(sampl$detected[!is.na(sampl$detected)])
        if (!(n > 0)) 
            stop("Sample size of zero!")
        L <- sum(abs(sampl$design$start.y - sampl$design$end.y))
        L<-L*sampl$design$effective.n.transects/sampl$design$n.transects
        width <- sampl$population$region$width
        length <- sampl$population$region$length
        A <- width * length
        xi <- sampl$distance[sampl$detected > 0 & !is.na(sampl$detected)]
        w <- min(sampl$design$visual.range, (1.01 * max(xi)))
        zi <- sampl$population$groupsize[sampl$detected > 0 & !is.na(sampl$detected)]
        model<-model
        lower<-0
        upper<-w
    }
    else stop("sampl must be of class sample.lt or sample.dp")
    parents<-list(wisp.id(sampl,newname=as.character(substitute(sampl))))
    if (model != "half.normal" & model != "hazard.rate") 
        stop("Invalid detection function model. The following are valid: \n 'half.normal', hazard.rate'")
    if (model == "half.normal" | model=="hazard.rate") { #hazard.rate uses half.normal to get start values
        full.llk <- function(x) {
            N <- exp(x[1]) + n
            sigma2 <- exp(x[2])
            temp1 <- lgamma(N + 1) - lgamma(N - n + 1) - lgamma(n + 1)
            mu <- sqrt(2 * pi * sigma2) * (pnorm(w, 0, sqrt(sigma2)) - 0.5)
            temp2 <- (N - n) * log(1 - (2 * L * mu/A))
            temp3 <- n * log(2 * w * L/A)
            llk <- -(temp1 + temp2 + temp3 - sum(xi^2)/(2 * sigma2) - n * log(w))
            return(llk)
        }
        transform.xtoNtheta <- function(x) {
            N <- exp(x[1]) + n
            sigma2 <- exp(x[2])
            return(c(N, sigma2))
        }
        transform.Nthetatox <- function(est) {
            x <- c(log(est[1] - n), log(est[2]))
            return(x)
        }
        cond.llk <- function(x) {
            sigma2 <- exp(x)
            mu <- sqrt(2 * pi * sigma2) * (pnorm(w, 0, sqrt(sigma2)) - 0.5)
#            mu <- integrate(detfn.lt, lower=0, upper=w, theta=c(scale.par,shape.par), model="half.normal")$value
            llk <- sum(xi^2)/(2 * sigma2) + n * log(mu) + n * log(w)
            llk <- sum(xi^2)/(2*sigma2) + n*log(mu)
            return(llk)
        }
        transform.xtotheta <- function(x) {
            sigma2 <- exp(x)
            return(sigma2)
        }
        transform.thetatox <- function(x) {
            x <- log(x)
            return(x)
        }
        startest <- var(xi) * (mean(xi)/0.8)^2
        startx <- transform.thetatox(startest)
        res <- nlm(cond.llk, abs(startx), typsize = abs(startx))
    }
    if (model == "hazard.rate") {
        full.llk <- function(x) {
            N <- exp(x[1]) + n
            scale.par <- exp(x[2])
            shape.par <- 1 + exp(x[3])
#            nbin <- 50
#            dist <- seq(0, w, length = nbin)
#            mu <- sum(1 - exp(-(dist/scale.par)^(-shape.par))) * w/nbin
            mu <- integrate(detfn.lt, lower=0, upper=w, theta=c(scale.par,shape.par), model="hazard.rate")$value
            temp1 <- lgamma(N + 1) - lgamma(N - n + 1) - lgamma(n+1)
            temp2 <- (N - n) * log(1 - (2 * L * mu/A))
            temp3 <- n * log(2 * w * L/A)
            llk <- -(temp1 + temp2 + temp3 + sum(log(1 - exp(-(xi/scale.par)^(-shape.par)))) - n*log(w))
            return(llk)
        }
        transform.xtoNtheta <- function(x) {
            N <- exp(x[1]) + n
            scale.par <- exp(x[2])
            shape.par <- 1 + exp(x[3])
            return(c(N, scale.par, shape.par))
        }
        transform.Nthetatox <- function(est) {
            x <- c(log(est[1] - n), log(est[2]), log(est[3] - 1))
            return(x)
        }
        cond.llk <- function(x, xi, lower, upper, model) {
            scale.par <- exp(x[1])
            shape.par <- 1 + exp(x[2])
            n<-length(xi)
#             nbin <- 50
#            dist <- seq(0, w, length = nbin)
#            mu <- sum(1 - exp(-(dist/scale.par)^(-shape.par))) * w/nbin
            mu <- integrate(detfn.lt, lower=lower, upper=upper, theta=c(scale.par,shape.par), model=model)$value
#            llk <- -sum(log(1 - exp(-(xi/scale.par)^(-shape.par)))) + n * log(mu) + n * log(w)
            llk <- -sum(log(1 - exp(-(xi/scale.par)^(-shape.par)))) + n*log(mu)
            return(llk)
        }
        transform.xtotheta <- function(x) {
            scale.par <- exp(x[1])
            shape.par <- 1 + exp(x[2])
            return(c(scale.par, shape.par))
        }
        transform.thetatox <- function(x) {
            theta <- c(log(x[1]), log(x[2] - 1))
            return(theta)
        }
        sigma2<-transform.xtotheta(res$estimate)  # get half-normal parameter from fit above
        startest<-hr.start(sigma2=sigma2, w=w)    # use it to get hazard.rate starting parameters
        startx <- transform.thetatox(startest)
        for(ii in 1:2) if(is.nan(startx[ii])) startx[ii]<-3 # (to deal with flat functions)
        res <- nlm(cond.llk, startx, typsize = abs(startx), xi=xi, lower=0, upper=w, model=model)
    }
    xhat <- res$estimate
    log.likelihood <- -res$minimum
    AIC <- -2 * log.likelihood + 2 * length(xhat)
    theta <- transform.xtotheta(xhat)
    mu <- integrate(detfn.lt, lower=0, upper=w, theta=theta, model=model)$value
    Nhat <- n * A/(2 * L * mu)
    if (!conditional) {
        startest <- c(Nhat * n, theta)
        startx <- transform.Nthetatox(startest)
        if (model == "half.normal") 
            res <- nlm(full.llk, abs(startx), typsize = abs(startx))
        if (model == "hazard.rate") 
            res <- nlm(full.llk, startx, typsize = abs(startx))
        xhat <- res$estimate
        log.likelihood <- -res$minimum
        AIC <- -2 * log.likelihood + 2 * length(xhat)
        est <- transform.xtoNtheta(xhat)
        Nhat <- est[1]
        theta <- est[2:length(est)]
        mu <- integrate(detfn.lt, lower=0, upper=w, theta=theta, model=model)$value
    }
    if (model == "half.normal") {
        theta <- matrix(theta, nrow = 1, ncol = 1, dimnames = list(c("sigma2"), ""))
    }
    if (model == "hazard.rate") {
        theta <- matrix(theta, nrow=2, ncol=1, dimnames=list(c("scale.parameter", "shape.parameter"), ""))
    }
    pointest <- list(sample=sampl, model=model, conditional=conditional, Nhat.grp = Nhat, Nhat.ind = Nhat * mean(zi), 
        theta = theta, mu = mu, nL = n/sum(L), Es = mean(zi), 
        log.likelihood = log.likelihood, AIC = AIC, fit.summary = res, parents=parents, created=date())
    class(pointest) <- "point.est.lt"
    if (plot) plot(pointest)
    return(pointest)
}

is.point.est.lt<-function(est)
{
 inherits(est, "point.est.lt")
}




detfn.lt<-function(x, theta, model="half.normal") 
#------------------------------------------------------------------------
# Returns line transect detection function of type <model>, with
# parameters <theta>, evaluated at <x>.
# NOTE: This is different from the function used when generating
#       data (detection.transectmethods()) because that also uses
#       exposure in the scale parameter (exp(theta[1]+theta[2]*exposure))
#------------------------------------------------------------------------
{
 p<-switch(model,
      half.normal=exp(-x^2/(2*theta[1])),
      hazard.rate=1 - exp(-(x/theta[1])^(-theta[2]))
    )
 p
}


hr.start<-function(sigma2,w)
#-----------------------------------------------------------------------
# Finds starting parameters for hazard rate by fixing hazard rate to be
# equal to half-normal at distances w and w/2.
# Argument sigma2 is the scale parameter of the half-normal
#-----------------------------------------------------------------------
{
 g.w<-detfn.lt(w,theta=sigma2, model="half.normal")    # half-normal value at w
 g.5w<-detfn.lt(w/2,theta=sigma2, model="half.normal") # half-normal value at w/2
 G<-log(-log(1-g.5w))
 k<- -log(1-g.w)
 b<-(G-log(k))/log(2)
 a<-w*k^(1/b)
 c(a,b)
}



summary.point.est.lt<-function(est, digits=5) 
{
 if (!is.point.est.lt(est)) stop("\nThe parameter <samp> must be of class 'point.est.lt'.\n")
 cat("\n")
 cat("POINT ESTIMATE SUMMARY (LINE TRANSECT METHOD)\n")
 cat("---------------------------------------------\n")
 cat("creation date   :", est$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(est$parents)) {
   cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
 }
 cat("\n")
 cat("Conditional likelihood? :",est$conditional,"\n")
 cat("\n")
 n <- sum(est$sample$detected[!is.na(est$sampl$detected)])
    w <- est$sample$design$visual.range
    K <- est$sample$design$effective.n.transects
    frac.L.in<-est$sample$design$effective.n.transects/est$sample$design$n.transects
    L <- sum(abs(est$sample$design$start.y - est$sample$design$end.y))*frac.L.in
    a <- 2 * w * L
    A <- est$sample$population$region$width * est$sample$population$region$length
    cat("Truncation distance               :", w, "\n")
    cat("Number of lines                   :", K, "\n")
    cat("Total line length in survey region:", L, "\n")
    cat("Survey area                       :", A, "\n")
    cat("Covered area                      :", a, "\n")
    cat("Percentage of survey area covered :", signif(100*a/A, digits=digits), "%\n")
 cat("\n")
    cat("Number of groups detected         :",n,"\n")
    cat("Mean encounter rate per transect  :",signif(est$nL, digits=digits),"\n")
    cat("\n")
    cat("Detection function Model          :",est$model,"\n")
    if(est$model=="half.normal") {
      cat("    p(detect) = exp(-0.5 * distance^2/(sigma^2)\n")
      cat("    Parameters: \n")
      cat("      sigma = ", signif(sqrt(est$theta[1]), digits=digits), "\n") 
    }
    if(est$model=="hazard.rate") {
      cat("    p(detect) = 1 - exp(-(distance/a)^(-b]))\n")
      cat("  Parameters: \n")
      cat("    a = ", signif(est$theta[1], digits=digits), "b = ",signif(est$theta[2], digits=digits),"\n") 
    }
    cat("Effective strip half-width        :",signif(est$mu, digits=digits),"\n")
    cat("Mean detection probability        :",signif(est$mu/w, digits=digits),"\n")
    cat("Effective percentage area covered :",signif((100*a/A)*est$mu/w, digits=digits),"%\n")
    cat("\n")
    cat("Group abundance                   :",round(est$Nhat.grp),"\n")
    cat("Individual abundance              :",round(est$Nhat.ind),"\n")
    cat("Mean group size                   :",signif(est$Es, digits=digits),"\n")
    cat("\n")
    cat("Log-likelihood                    :",est$log.likelihood,"\n")
    cat("AIC                               :",est$AIC,"\n")
}

plot.point.est.lt<-function (est, breaks=10, title = TRUE) 
{
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 old.par<-par(no.readonly=TRUE)
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 if (title) main <- "Perpendicular distance distribution\nand fitted probability density function"
# extract distances, w, det. fn. model and parameters from 
 xi<-est$sample$distance[!is.na(est$sample$distance) & est$sample$detected==1]
 theta<-est$theta
 model<-est$model
 w<-est$sample$design$visual.range
 mu <- integrate(detfn.lt, lower=0, upper=w, theta=theta, model=model)$value
 x <- seq(0, w, length = 100)
 f<-detfn.lt(x,theta,model=model)/mu
 if(length(breaks)==1) breaks<-seq(0,w,length=breaks)
###       Unused argument found by Mike Merridith, June 2007
### hst<-hist(xi, freq=FALSE, breaks=breaks, plot=FALSE)
 hst <- hist(xi, breaks = breaks, plot = FALSE)
 ymax<-max(f,hst$density)
 hist(xi, xlab = "Perpendicular Distance", ylab="Probability density", freq=FALSE, main=main, 
      ylim=c(0,ymax), breaks=breaks)
 lines(x, f, col="red")
 par(old.par)
}



int.est.lt=function (sampl, ci.type = "boot.nonpar", nboot = 999, vlevels = c(0.025, 
    0.975), conditional = TRUE, model = "half.normal", plot = FALSE, 
    show.all = FALSE, seed = NULL) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.sample.lt(sampl) & !is.sample.dp(sampl)) 
        stop("\n*** <sampl> is not an object of class 'sample.lt' or  'sample.dp'.\n")
    if (!(ci.type %in% c("boot.nonpar"))) 
        stop(paste("\n*** Invalid <ci.type>. Only ", "'boot.nonpar' is implemented.\n"))
    if (!is.numeric(vlevels)) 
        stop("\n*** All <vlevels> values must be numeric.\n")
    if (any(vlevels < 0) | any(vlevels > 1)) 
        stop("\n*** All <vlevels> values must be between 0 and 1.\n")
    if (length(vlevels) != 2) 
        stop("\n*** There must be exactly two <vlevels> values!\n")
    if (!is.numeric(nboot)) 
        stop("\n*** <nboot> must be numeric.\n")
    if (nboot < 2) 
        stop("\n*** <nboot> must be at least 2.\n")
    if ((plot != T) & (plot != F)) 
        stop("\n*** <plot> must be TRUE or FALSE.\n")
    if (model != "half.normal" & model != "hazard.rate") 
        stop("Invalid detection function model. The following are valid: \n 'half.normal', hazard.rate'")
    if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) 
        stop("\nThe parameter <seed> must be a number or a WiSP object.\n")
    if (is.wisp.class(seed)) {
        if (is.element("seed", names(seed))) {
            if (!is.null(seed$seed)) {
                seed <- seed$seed
                set.seed(seed)
            }
        }
    }
    if (is.numeric(seed)) 
        set.seed(seed)
    parents <- list(wisp.id(sampl, newname = as.character(substitute(sampl))))
    samp <- sampl
    if (is.sample.lt(sampl)) 
        samp <- obscure.sample.lt(sampl)
    if (is.sample.dp(sampl)) 
        samp <- obscure.sample.dp(sampl)
    K <- samp$design$n.transects
    nobs <- hist(samp$transect, breaks = seq(0, K, length = (K + 
        1)) + 0.5, plot = F)$counts
    if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
        b.nL <- rep(0, nboot)
        b.Nhat.grp <- rep(0, nboot)
        b.Nhat.ind <- rep(0, nboot)
        if (model == "half.normal") {
            b.theta <- matrix(rep(0, nboot), nrow = nboot, ncol = 1, dimnames = list(replicate = 1:nboot, c("sigma2")))
        }
        else {
            b.theta <- matrix(rep(0, nboot * 2), nrow = nboot, 
                ncol = 2, dimnames = list(replicate = 1:nboot, 
                  c("scale.parameter", "shape.parameter")))
        }
        b.mu <- rep(0, nboot)
        b.Es <- rep(0, nboot)
        b.samp <- samp
    }
    if (ci.type == "boot.nonpar") {
        for (i in 1:nboot) {
            index <- sample(1:K, K, replace = T)
            reps <- hist(index, breaks = (seq(0, K, length = K + 
                1) + 0.5), plot = F)$counts
            n <- sum(reps * nobs)
            b.samp$distance <- rep(NA, n)
            b.samp$population$groupsize <- rep(NA, n)
            b.samp$transect <- rep(NA, n)
            last <- 0
            b.tno <- 1
            n.units.per.transect <- samp$design$n.units/samp$design$n.transects
            for (k in 1:K) {
                transind <- c(((k - 1) * n.units.per.transect + 
                  1):(k * n.units.per.transect))
                if (reps[k] > 0) {
                  keep <- (samp$transect == k & !is.na(samp$transect))
                  first <- last + 1
                  nsit <- length(keep[keep == T])
                  last <- first - 1 + nsit * reps[k]
                  tn <- b.tno
                  for (m in 1:reps[k]) {
                    bt.first <- (tn - 1) * n.units.per.transect + 
                      1
                    bt.last <- tn * n.units.per.transect
                    b.samp$design$pos.x[bt.first:bt.last] <- samp$design$pos.x[transind]
                    b.samp$design$start.y[bt.first:bt.last] <- samp$design$start.y[transind]
                    b.samp$design$end.y[bt.first:bt.last] <- samp$design$end.y[transind]
                    tn <- tn + 1
                  }
                  if (nsit > 0) {
                    b.samp$distance[first:last] <- rep(samp$distance[keep], 
                      reps[k])
                    b.samp$population$groupsize[first:last] <- rep(samp$population$groupsize[keep], 
                      reps[k])
                    b.samp$transect[first:last] <- rep(b.tno:(b.tno + 
                      reps[k] - 1), rep(nobs[k], reps[k]))
                  }
                  first <- last
                }
            }
            b.samp$detected <- rep(1, length(b.samp$distance))
            est <- point.est.lt(b.samp, conditional = conditional, model = model)
            if (show.all) plot(est)
            b.Nhat.grp[i] <- est$Nhat.grp
            b.Nhat.ind[i] <- est$Nhat.ind
            b.theta[i, ] <- est$theta
            b.mu[i] <- est$mu
            L <- sum(abs(b.samp$design$start.y - b.samp$design$end.y))
            b.nL[i] <- length(b.samp$distance)/L
            b.Es[i] <- mean(b.samp$population$groupsize)
        }
        text2 <- paste("\nNon-parametric Bootstrap with", nboot, 
            "replicates")
    }
    if (ci.type == "boot.par" | ci.type == "boot.nonpar") {
        boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, 
            theta = b.theta, mu = b.mu, nL = b.nL, Es = b.Es)
        valid <- (b.Nhat.grp != Inf)
        boot.mean <- list(Nhat.grp = mean(b.Nhat.grp[valid]), 
            Nhat.ind = mean(b.Nhat.ind[valid]), theta = apply(as.matrix(b.theta[valid, 
                ]), 2, mean), mu = mean(b.mu[valid]), nL = mean(b.nL[valid]), 
            Es = mean(b.Es[valid]))
#        boot.mean <- list(Nhat.grp = mean(b.Nhat.grp), Nhat.ind = mean(b.Nhat.ind), 
#            theta = apply(b.theta, 2, mean), mu = mean(b.mu), 
#            nL = mean(b.nL), Es = mean(b.Es))
        se <- list(Nhat.grp = sqrt(var(b.Nhat.grp[valid])), Nhat.ind = sqrt(var(b.Nhat.ind[valid])), 
            theta = sqrt(apply(as.matrix(b.theta[valid, ]), 2, 
                var)), mu = sqrt(var(b.mu[valid])), nL = sqrt(var(b.nL[valid])), 
            Es = sqrt(var(b.Es[valid])))
        cv <- list(Nhat.grp = se$Nhat.grp/boot.mean$Nhat.grp, 
            Nhat.ind = se$Nhat.ind/boot.mean$Nhat.ind, theta = se$theta/boot.mean$theta, 
            mu = se$mu/boot.mean$mu, nL = se$nL/boot.mean$nL, 
            Es = se$Es/boot.mean$Es)
        civec <- rep(NA, length(vlevels))
        n.theta <- length(b.theta[1, ])
        ci <- list(Nhat.grp = civec, Nhat.ind = civec, theta = matrix(rep(civec, 
            n.theta), nrow = n.theta, ncol = length(vlevels), 
            dimnames = list(dimnames(b.theta)[[2]], rep("", 2))), 
            mu = mean(b.mu), nL = mean(b.nL), Es = civec)
        for (i in 1:length(ci)) {
            if (!is.null(dim(boot.dbn[[i]]))) {
                for (j in 1:dim(boot.dbn[[i]])[2]) {
                  sort.est <- sort(boot.dbn[[i]][, j])
                  cin <- round(nboot * vlevels, 0)
                  cin <- ifelse(cin < 1, 1, cin)
                  ci[[i]][j, ] <- sort.est[cin]
                }
            }
            else {
                sort.est <- sort(boot.dbn[[i]])
                cin <- round(nboot * vlevels, 0)
                cin <- ifelse(cin < 1, 1, cin)
                ci[[i]] <- sort.est[cin]
            }
        }
    }
    intest <- list(levels = vlevels, ci = ci, boot.mean = boot.mean, 
        boot.dbn = boot.dbn, se = se, cv = cv, ci.type = ci.type, 
        conditional = conditional, model = model, parents = parents, 
        created = date(), seed = seed)
    class(intest) <- "int.est.lt"
    if (plot) plot(intest, type = "hist")
    return(intest)
}


is.int.est.lt<-function(est)
{
 inherits(est, "int.est.lt")
}




summary.int.est.lt<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "mu", "nL", "theta"), digits=5)
{
 if(!is.int.est.lt(iest)) 
   stop("Argument <iest>. must be of class int.est.lt\n")
 addtext1<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 addtext2<-paste("Detection function model          : ",iest$model,"\n",sep="")
 addtext3<-paste("Conditional likelihood?           : ",iest$conditional,sep="")
 addtext<-paste(addtext1,addtext2,addtext3,sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.lt<-function(iest, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}



point.sim.lt<-function (pop.spec, survey.spec, design.spec, B=99, model.sel=FALSE, 
    plot=FALSE, title=FALSE, conditional=TRUE, model="half.normal", seed=NULL, show=FALSE) 
{
    if (!is.pars.survey.lt(survey.spec) & !is.pars.survey.dp(survey.spec)) {
        stop("\nsurvey.spec must be of class 'pars.survey.lt' or 'pars.survey.dp'.\n")
    }
    if (!is.design.lt(design.spec) & !is.pars.design.lt(design.spec) &
        !is.design.dp(design.spec) & !is.pars.design.dp(design.spec)) {
        stop("design.spec must be of class 'design.lt', 'pars.design.lt', 'design.dp' or 'pars.design.dp'\n")
    }
    if (!is.population(pop.spec) & !is.pars.population(pop.spec)) {
        stop("pop.spec must be of class 'population' or 'pars.population'.\n")
    }
 parents<-list(wisp.id(pop.spec,newname=as.character(substitute(pop.spec))), 
   wisp.id(survey.spec,newname=as.character(substitute(survey.spec))), 
   wisp.id(design.spec,newname=as.character(substitute(design.spec))))
 if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) stop("\nThe parameter <seed> must be a number or a WiSP object.\n")
 if (is.wisp.class(seed)) {
   if (is.element("seed",names(seed))) {
     if(!is.null(seed$seed)) {
       seed<-seed$seed
       set.seed(seed) # use seed stored in passed wisp object
     }
   }
 }
 if(is.numeric(seed)) set.seed(seed)
 random.pop<-FALSE
 random.design<-FALSE
 true<-get.true.state(pop.spec)
 stats<-c("Nhat.grp","Nhat.ind","Es", "mu", "nL") 
 len<-length(stats)
 res <- matrix(0, nrow = B, ncol=len)
 res <- as.data.frame(res)
 names(res)<-stats
 out.est <- NULL
 for (i in 1:B) {
   if (is.population(pop.spec)) mypop <- pop.spec
   if (is.pars.population(pop.spec)) {
     mypop <- generate.population(pop.spec)
     random.pop<-TRUE
   }
   if (is.design.lt(design.spec) | is.design.dp(design.spec)) mydes <- design.spec
   if (is.pars.design.lt(design.spec)) {
     mydes <- generate.design.lt(design.spec)
     random.design<-TRUE
   }
   if (is.pars.design.dp(design.spec)) {
     mydes <- generate.design.dp(design.spec)
     random.design<-TRUE
   }
   survey.spec$population <- mypop
   survey.spec$design <- mydes
   if (is.pars.survey.lt(survey.spec)) mysamp <- generate.sample.lt(survey.spec)
   if (is.pars.survey.dp(survey.spec)) mysamp <- generate.sample.dp(survey.spec)
   if (model.sel == T) {
     out.est.hn <- point.est.lt(mysamp, plot=FALSE, title=title, conditional=conditional, model="half.normal")
     out.est.hr <- point.est.lt(mysamp, plot=FALSE, title=title, conditional=conditional, model="hazard.rate")
     if (out.est.hn$AIC <= out.est.hr$AIC) out.est <- out.est.hn
     else out.est <- out.est.hr
   } else {
     out.est <- point.est.lt(mysamp, plot=FALSE, title=title, conditional=conditional, model=model)
   }
   res[i, stats] <- out.est[stats]
   if(show) plot(out.est)
 }
 true.N.grp <- length(mypop$groupsize)
 sim<-list(est=res, true=true, model.sel=model.sel, conditional=conditional, model=model, 
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
 class(sim) <- "point.sim.lt"
 if(plot) plot(sim, est="Nhat.grp")
 return(sim)
}


is.point.sim.lt<-function(sim)
{
 inherits(sim, "point.sim.lt")
}

#plot.point.sim.lt<-function(sim, est=c("Nhat.grp","Nhat.ind","Es", "mu", "nL"), breaks="Sturges", type="both", ...)
plot.point.sim.lt<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
 if(length(est)>1) breaks="Sturges"
 for(i in 1:length(est)) {
   if(i>1) windows(height=5, width=4)
   sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
 }
}


summary.point.sim.lt<-function(sim, est=c("Nhat.grp","Nhat.ind","Es", "mu", "nL"), digits=5)
{
 if(!is.point.sim.lt(sim)) 
   stop("Argument <sim>. must be of class point.sim.lt\n")
 addtext<-paste("(Model = ",sim$model,";  Conditional = ",sim$conditional, ";  Model selection = ",sim$model.sel,")",sep="")
 summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}



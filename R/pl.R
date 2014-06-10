#Plot sampling functions

setpars.design.pl<-function (reg, n.interval.x = 10, n.interval.y = 10, method = "random", 
    area.covered = 0.05, jitter = "sync") 
{
    if (!is.region(reg)) 
        stop("\n*** <reg> must be of type 'region'\n")
    if (!is.numeric(n.interval.x) | !is.numeric(n.interval.y)) 
        stop("\n*** <n.inteval.x> and <n.interval.y> must be numeric\n")
    if ((n.interval.x != as.integer(n.interval.x)) | (n.interval.x < 
        0) | (n.interval.y != as.integer(n.interval.y)) | (n.interval.y < 
        0)) 
        stop(paste("\n*** Either <n.interval.x> or <n.interval.y> is invalid.", 
            "They should be positive integers.\n"))
    if (!(method %in% c("random", "regular"))) 
        stop("\n*** Unrecognised <method>. Possible is 'random' or 'regular'.\n")
    if (!is.numeric(area.covered)) 
        stop("\n*** <area.covered> must be numeric\n")
    if (area.covered > 1 | area.covered < 0) 
        stop(paste("\n*** <area.covered> should be a percentage value", 
            "between 0 and 1.\n"))
    if (!(jitter %in% c("sync", "unsync"))) 
        stop("\n*** Unrecognised <jitter>. Possible is 'sync' and 'unsync'.\n")
    parents<-list(wisp.id(reg,newname=as.character(substitute(reg))))
    pars.design.pl <- list(region = reg, n.interval.x = n.interval.x, 
        n.interval.y = n.interval.y, method = method, percentage.area.covered = area.covered, 
        jitter = jitter, parents=parents, created=date())
    class(pars.design.pl) <- "pars.design.pl"
    return(pars.design.pl)
}

is.pars.design.pl<-function (despars) 
{
   inherits(despars,"pars.design.pl")
}

summary.pars.design.pl<-function(des, digits=5)
{
# check class:
 if (!is.pars.design.pl(des)) stop("\nThe parameter <des> must be of class 'pars.design.pl'.\n")
 cat("\n")
 cat("PLOT SAMPLING DESIGN PARAMETERS SUMMARY\n")
 cat("---------------------------------------\n")
 cat("creation date   :", des$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(des$parents)) {
   cat("      ",paste("(",des$parents[[i]]$class,", ",des$parents[[i]]$name,", ",des$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(des$seed)) cat("random number seed used: ",des$seed,"\n")
 cat("\n")
 cat("Method              : ", des$method,"\n")
 cat("Coverage probability: ", signif(des$percentage.area.covered,digits),"\n")
 cat("Number of x intervals: ", des$n.interval.x,"\n")
 cat("Number of y intervals: ", des$n.interval.y,"\n")
 cat("Jitter by           : ", des$jitter,"\n\n")

 cat("Region dimensions (length x width):", des$reg$length, "x", des$reg$width, "\n")
}


generate.design.pl<-function (pars, seed=NULL) # pars used to be pars.design.pl
{
# check class:
 if (!is.pars.design.pl(pars)) stop("\nThe parameter <pars> must be of class 'pars.design.pl'.\n")
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
    reg <- pars$region
    nintx <- pars$n.interval.x
    ninty <- pars$n.interval.y
    pacov <- pars$percentage.area.covered
    method <- pars$method
    jitter <- pars$jitter
    reg.length <- reg$length
    reg.width <- reg$width
    aregion <- reg.length * reg.width
    arect <- aregion/(nintx * ninty)
    xcoords <- seq(0, reg.length, reg.length/nintx)
    ycoords <- seq(0, reg.width, reg.width/ninty)
    rectmatrix <- matrix(1, ncol = 4, nrow = nintx * ninty)
    rectmatrix[, 1] <- rep(xcoords[1:nintx], ninty)
    rectmatrix[, 3] <- rep(xcoords[2:(nintx + 1)], ninty)
    rectmatrix[, 2] <- rep(ycoords[1:ninty], rep(nintx, ninty))
    rectmatrix[, 4] <- rep(ycoords[2:(ninty + 1)], rep(nintx, 
        ninty))
    if (method == "random") {
        nrects <- ceiling(pacov * aregion/arect)
        rectnumbers <- sample(1:(nintx * ninty), nrects)
        needed.rectmatrix <- matrix(rectmatrix[rectnumbers, ], 
            ncol = 4)
    }
    if (method == "regular") {
        nrects <- nintx * ninty
        needed.rectmatrix <- rectmatrix
    }
    cell.length <- reg.length/nintx
    cell.width <- reg.width/ninty
    gamm <- sqrt(pacov * aregion/(arect * nrects))
    area.length <- cell.length * gamm
    area.width <- cell.width * gamm
    if (jitter == "sync") {
        xshift <- runif(1) * (cell.length - area.length)
        yshift <- runif(1) * (cell.width - area.width)
    }
    if (jitter == "unsync") {
        xshift <- runif(nrects) * (cell.length - area.length)
        yshift <- runif(nrects) * (cell.width - area.width)
    }
    needed.rectmatrix[, 1] <- needed.rectmatrix[, 1] + xshift
    needed.rectmatrix[, 2] <- needed.rectmatrix[, 2] + yshift
    needed.rectmatrix[, 3] <- needed.rectmatrix[, 1] + area.length
    needed.rectmatrix[, 4] <- needed.rectmatrix[, 2] + area.width
    parents<-list(wisp.id(pars,newname=as.character(substitute(pars))))
    des <- list(region = reg, number.areas = nrects, area.coordinates = needed.rectmatrix, parents=parents, created=date(), seed=seed)
    class(des) <- "design.pl"
    return(des)
}

is.design.pl <- function (des) 
#----------------------------------------------------------------
# description:
#    tests if the given object <des> if of type "design.pl"
#
# author: W. Zucchini
# adapted by: M. Erdelmeier
#----------------------------------------------------------------
{
    # test if <des> is of the type "design.pl"
    inherits(des, "design.pl")
}

summary.design.pl<-function(des, digits=5)
{
# check class:
 if (!is.design.pl(des)) stop("\nThe parameter <des> must be of class 'design.pl'.\n")
 cat("\n")
 cat("PLOT SAMPLING DESIGN SUMMARY\n")
 cat("----------------------------\n")
 cat("creation date   :", des$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(des$parents)) {
   cat("      ",paste("(",des$parents[[i]]$class,", ",des$parents[[i]]$name,", ",des$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(des$seed)) cat("random number seed used: ",des$seed,"\n")
 cat("\n")
 cat("Number of plots : ", des$number.areas,"\n")
 a<-sum((des$area.coordinates[,3]-des$area.coordinates[,1])*(des$area.coordinates[,4]-des$area.coordinates[,2]))
 A<-des$reg$length*des$reg$width
 cat("Coverage        : ",signif(100*a/A, digits),"%\n")
 cat("Region dimensions (length x width):", des$reg$length, "x", des$reg$width, "\n")
}


plot.design.pl <- function (des, newplot = TRUE,...)

   #-----------------------------------------------------
   # description:
   #   The function plot the current survey design.
   #
   # author: W. Zucchini
   # adapted by: M. Erdelmeier
   #-----------------------------------------------------

   #-----------------------------------------------------
   # input/output-variables:
   #-----------------------------------------------------

   # name    | type    | I/O | description
   #---------------------------------------------------------------------
   # des     | design- |  I  | design which shall be plotted
   #         | object  |     |
   # newplot | boolean |  I  | if FALSE: picture is drawn in the
   #         |         |     | last plot

   
   #-----------------------------------------------------
   # used objects
   #-----------------------------------------------------

   # name           | type            | R/W | description
   #---------------------------------------------------------------------


   #-----------------------------------------------------
   # local variables
   #-----------------------------------------------------

   # name    | type       | description
   #-----------------------------------------------------------------
   # par.was | par-object | stores old graphical parameter
   # qp      | matrix     | matrix which includes the coordinates
   #         | of real    | of the survey areas
   # reg     | region-    | current region object
   #         | object     |


   #-------------------------------------------------------
   # programming part
   #-------------------------------------------------------

{
   # test design object
   if (!is.design.pl(des))
      stop("\n*** <des> must be of type 'design.pl'\n")

   # test <newplot>
   if ((newplot!=TRUE) & (newplot!=FALSE))
      stop("\n*** <newplot> must be TRUE or FALSE.\n")

   # get region
   reg <- des$region

   ## save current graphic parameters
   par.was <- par(no.readonly = T)

   # get region dimensions
   len <- reg$length
   width <- reg$width

   # calculate dimensions of plot borders (ratio corresponding to
   # length and width of region, but appropriate to plot area)
   margin <- calculate.plot.margin (reg.x=len, reg.y=width,
             area.x=par.was$fin[1], area.y=par.was$fin[2])

   # set plot margins
   par (mai=c(margin$y, margin$x, margin$y, margin$x))

   # plot border
   par(new = !newplot)
   plot(0, 0, type = "n", las = 1, xlab = "", ylab = "",
         xlim = c(0, len), ylim = c(0, width),
         xaxs = "i", yaxs = "i", xpd = T,...)

   ## plot survey areas
   qp <- des$area.coordinates
   rect(qp[, 1], qp[, 2], qp[, 3], qp[, 4], col = "aquamarine",
        border = par("fg"))

   ## restore changed graphic parameters
   par(new=par.was$new)
}




generate.sample.pl<-function(pop, des, seed=NULL) 
{
 if (!is.population(pop)) stop("\n*** The parameter <pop> must be of type 'population'.\n")
 if (!is.design.pl(des)) stop("\n*** The parameter <des> must be of type 'design.pl'.\n")
 if (!equal(pop$region, des$region)) stop(paste("\n*** The given population and design were defined", 
            "with different regions.\n"))
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
    pos.x <- pop$posx
    pos.y <- pop$posy
    n.groups <- length(pop$groupID)
    nquads <- des$number.areas
    quadpars <- des$area.coordinates
    detected <- rep(0, n.groups)
    quad <- rep(NA, n.groups)
    for (i in 1:nquads) {
        seen <- ((pos.x >= quadpars[i, 1]) & (pos.x <= quadpars[i, 
            3]) & (pos.y >= quadpars[i, 2]) & (pos.y <= quadpars[i, 
            4]))
        detected[seen] <- 1
        quad[seen] <- i
    }
    if (all(detected == 0)) stop("\nThere were zero detections.\n")
    parents<-list(wisp.id(pop,newname=as.character(substitute(pop))),wisp.id(des,newname=as.character(substitute(des))))
    samp<-list(population=pop, design=des, detected=detected, unit=quad, parents=parents, created=date(), seed=seed)
    class(samp) <- "sample.pl"
    return(samp)
}

is.sample.pl <- function (samp) 
#----------------------------------------------------------------
# description:
#    tests if the given object <samp> if of type "sample.pl"
#
# author: M. Erdelmeier
#----------------------------------------------------------------
{
    # test if <samp> is of the type "sample.pl"
    inherits(samp, "sample.pl")
}


summary.sample.pl<-function(samp, digits=5) 
{
 if (!is.sample.pl(samp)) stop("\nThe parameter <samp> must be of class 'sample.pl'.\n")
 cat("\n")
 cat("SAMPLE SUMMARY (PLOT METHOD)\n")
 cat("----------------------------\n")
 cat("creation date   :", samp$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(samp$parents)) {
   cat("      ",paste("(",samp$parents[[i]]$class,", ",samp$parents[[i]]$name,", ",samp$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(samp$seed)) cat("random number seed used: ",samp$seed,"\n")
 cat("\n")
 K <- samp$design$number.areas
 n <- sum(samp$detected)
 co <- samp$design$area.coordinates
 a <- abs(co[1, 1] - co[1, 3]) * abs(co[1, 2] - co[1, 4])
 A <- samp$design$region$length * samp$design$region$width
 covered <- K * a/A
 cat("Number of plots            :", K, "\n")
 cat("Individual plot size       :", a, "\n")
 cat("Number of detected groups  :", n, "\n")
 cat("Mean detections per plot   :", signif(n/K,digits), "\n")
 cat("Survey area size           :", A, "\n")
 cat("Percentage covered         :", signif(covered*100,digits), "%\n")
}


plot.sample.pl<-function (samp, show.sizes=T, show.exps=T, dsf=0.75, whole.population=FALSE) 
{
    if (!is.sample.pl(samp)) 
        stop("\nThe parameter <samp> must be of type 'sample.pl'.\n")
    if (!is.numeric(dsf)) 
        stop("\nThe parameter <dsf> must be numeric.\n")
    if (dsf <= 0) 
        stop("\nThe parameter <dsf> must be positive.\n")
    if ((show.sizes != T) & (show.sizes != F)) 
        stop("\nThe parameter <show.sizes> must be TRUE or FALSE.\n")
    if ((show.exps != T) & (show.exps != F)) 
        stop("\nThe parameter <show.exps> must be TRUE or FALSE.\n")
    if ((whole.population != T) & (whole.population != F)) 
        stop("\nThe parameter <whole.population> must be TRUE or FALSE.\n")
 pars.old<-par(no.readonly=TRUE)
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
    pop <- samp$population
    des <- samp$design
#    len <- pop$reg$length
#    width <- pop$reg$width
#    margin <- calculate.plot.margin(reg.x = len, reg.y = width, 
#        area.x = pars.old$fin[1], area.y = pars.old$fin[2])
#    par(mai = c(margin$y, margin$x, margin$y, margin$x))
#    plot(0, 0, type = "n", las = 1, xlab = "", ylab = "", xlim = c(0, 
#        len), ylim = c(0, width), xaxs = "i", yaxs = "i", xpd = T)
 plot.region(des$region, reset.pars=FALSE)
    quadpars <- des$area.coordinates
    rect(quadpars[, 1], quadpars[,2], quadpars[,3], quadpars[,4], col="aquamarine", border = par("fg"))
    if (whole.population == T) 
#        plot(pop, show.sizes=show.sizes, show.exps=show.exps, newplot=FALSE, dsf=dsf, details=FALSE)
    plot.groups(pop, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="black")
    inside <- (samp$detected == 1)
    seen <- pop
    seen$groupID <- pop$groupID[inside]
    seen$posx <- pop$posx[inside]
    seen$posy <- pop$posy[inside]
    seen$groupsize <- pop$groupsize[inside]
    seen$types <- pop$types[inside]
    seen$exposure <- pop$exposure[inside]
#    plot(seen, show.sizes=show.sizes, show.exps=show.exps,  newplot=FALSE, dsf=dsf, group.col="red", details=FALSE)
    plot.groups(seen, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="red")
 par(pars.old)
}


obscure.sample.pl<-function (samp) 
{
    if (!is.sample.pl(samp)) 
        stop("\n*** <samp> is not an object of type 'sample.pl'.\n")
    t <- samp
    t$population$groupID <- samp$population$groupID[!is.na(samp$detected) & 
        samp$detected == 1]
    t$population$posx <- samp$population$posx[!is.na(samp$detected) & 
        samp$detected == 1]
    t$population$posy <- samp$population$posy[!is.na(samp$detected) & 
        samp$detected == 1]
    t$population$groupsize <- samp$population$groupsize[!is.na(samp$detected) & 
        samp$detected == 1]
    t$population$types <- samp$population$types[!is.na(samp$detected) & 
        samp$detected == 1]
    t$population$exposure <- samp$population$exposure[!is.na(samp$detected) & 
        samp$detected == 1]
    t$unit <- samp$unit[!is.na(samp$detected) & samp$detected == 
        1]
    t$detected <- samp$detected[!is.na(samp$detected) & samp$detected == 
        1]
    t$created<-date()
    t
}
point.est.pl<-function(samp, HT=FALSE)
{
 estimate.pl(samp, ci.type = NULL, HT=HT)
}


estimate.pl<-function(samp, HT=FALSE, vlevels=c(0.025, 0.975), ci.type=NULL, nboot=999, plot=FALSE, seed=NULL, ...) 
#----------------------------------------------------------------------
# Must split this into point.est.pl() and int.est.pl() in due course...
# Must estimate individual abundance and mean group size in bootstrap!
# Removed all rounding of CI, except in printing numbers to plot
#----------------------------------------------------------------------
{
 if (!is.sample.pl(samp)) stop("\n*** <samp> is not an object of type 'sample.pl'.\n")
 if (!is.null(ci.type) && !(ci.type %in% c("normal", "boot.par", "boot.nonpar"))) 
    stop(paste("\n*** Unrecognised <ci.type>. Possible is", "'normal', 'boot.par' and 'boot.nonpar'\n"))
 if (!is.numeric(vlevels)) stop("\n*** All <vlevels> values must be numeric.\n")
 if (any(vlevels < 0) | any(vlevels > 1)) stop("\n*** All <vlevels> values must be between 0 and 1.\n")
 if (!is.numeric(nboot)) stop("\n*** <nboot> must be numeric.\n")
 if (nboot < 1) stop("\n*** <nboot> must be at least 1.\n")
 if ((HT != T) & (HT != F)) stop("\n*** <plot> must be TRUE or FALSE.\n")
 if ((plot != TRUE) & (plot != FALSE)) stop("\n*** <plot> must be TRUE or FALSE.\n")
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
 nb <- NULL
 pop <- samp$population
 des <- samp$design
 reg.length <- des$region$length
 reg.width <- des$region$width
 nquads <- des$number.areas
 max.nquads <- des$max.number.areas
 ra <- reg.length * reg.width
 qp <- des$area.coordinates
 qa <- (qp[, 4] - qp[, 2]) * (qp[, 3] - qp[, 1])
 pqa <- qa/ra
 p <- sum(pqa)
 qn <- numeric(nquads)
 for (i in 1:nquads) {
   qn[i]<-sum((pop$posx >= qp[i,1]) & (pop$posx<=qp[i,3]) & (pop$posy >= qp[i,2]) & (pop$posy <= qp[i,4]))}
   NH <- mean(qn/pqa)
   n <- sum(qn)
   Nmle <- n/p
   Es <- mean(pop$groupsize[samp$detected == 1])
   Nhat.grp.MLE <- Nmle
   Nhat.grp.HT <- NH
   if (is.null(ci.type)) {
     if (HT) {
       pointest<-list(sample=samp, Nhat.grp=Nhat.grp.HT, Nhat.ind=round(Nhat.grp.HT*Es), Es=Es, HT=HT, parents=parents, created=date(), seed=seed)
     } else {
       pointest<-list(sample=samp, Nhat.grp=Nhat.grp.MLE, Nhat.ind=round(Nhat.grp.MLE*Es), Es=Es, HT=HT, parents=parents, created=date(), seed=seed)
     }
   } else {
     if (ci.type != "normal") {    # only want seed when bootstrapping:
       if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) 
         stop("\nThe parameter <seed> must be a number or a WiSP object.\n")
       if (is.wisp.class(seed)) {
         if (is.element("seed",names(seed))) {
           if(!is.null(seed$seed)) {
             seed<-seed$seed
             set.seed(seed) # use seed stored in passed wisp object
           }
         } 
       }
       if(is.numeric(seed)) set.seed(seed)
     }
     if (ci.type == "normal") {
       mean <- n/p
       sd <- sqrt(mean * (1 - p)/p)
       ci <- qnorm(vlevels, mean, sd)
       cilist <- list(Nhat.grp = ci)
       xlo <- floor(max(0, qnorm(0.001, mean, sd)))
       xhi <- ceiling(qnorm(0.999, mean, sd))
       x <- seq(xlo, xhi, length = 100)
       y <- dnorm(x, mean, sd)
       yhi <- max(y * 0.025)
       ylo <- -yhi
     }else if (ci.type == "boot.par") {
       if (HT) nb <- sort(rbinom(nboot, round(NH), p)/p)
       else nb <- sort(rbinom(nboot, round(Nmle), p)/p)
       boot.dbn <- list(Nhat.grp = nb)
       boot.mean <- list(Nhat.grp = mean(nb))
       cin <- nboot * vlevels
       cin <- ifelse(cin < 1, 1, cin)
       ci <- nb[cin]
       cilist <- list(Nhat.grp = ci)
       yhi <- max(nb * 0.025)
       ylo <- -yhi
     }else if (ci.type == "boot.nonpar") {
       nb <- numeric(nboot)
       for (ib in 1:nboot) {
         sind <- sample(1:nquads, nquads, replace = T)
         if (HT) nb[ib] <- mean(qn[sind]/pqa[sind])
         if (!HT) nb[ib] <- sum(qn[sind])/sum(pqa[sind])
       }
       nb <- sort(nb)
       boot.dbn <- list(Nhat.grp = nb)
       boot.mean <- list(Nhat.grp = mean(nb))
       cin <- nboot * vlevels
       cin <- ifelse(cin < 1, 1, cin)
       ci <- nb[cin]
       cilist <- list(Nhat.grp = ci)
       yhi <- max(nb * 0.025)
       ylo <- -yhi
     }
   }
 if (is.null(ci.type)) {
   class(pointest) <- "point.est.pl"
   return(pointest)
 } else {
    if (ci.type == "boot.par" | ci.type == "boot.nonpar") {
      valid.Nhat <- boot.dbn[["Nhat.grp"]][boot.dbn[["Nhat.grp"]] != Inf]
      nvalid <- length(valid.Nhat)
      se<-list(Nhat.grp=sqrt(var(valid.Nhat)))
      cv<-list(Nhat.grp=se$Nhat.grp/mean(valid.Nhat))
      boot.mean$Nhat.grp<-mean(valid.Nhat)
     intest<-list(sample=samp, levels=vlevels, ci=cilist, boot.mean=boot.mean, boot.dbn=boot.dbn, HT=HT, ci.type=ci.type, 
                  se=se, cv=cv, parents=parents, created=date(), seed=seed)
    }
    if(ci.type == "normal") {
      k<-length(vlevels)
      lower<-cilist$Nhat.grp[1]
      upper<-cilist$Nhat.grp[k]
      q.lower<-qnorm(vlevels[1])
      q.upper<-qnorm(vlevels[k])
      se<-list(Nhat.grp=(upper-lower)/(q.upper-q.lower))
      cv<-list(Nhat.grp=se$Nhat.grp/point.est.pl(samp)$Nhat.grp)
      intest<-list(sample=samp, levels=vlevels, ci=cilist, boot.mean=NA, boot.dbn=NA, HT=HT, ci.type=ci.type, 
                   se=se, cv=cv, parents=parents, created=date(), seed=seed)
    }
   class(intest) <- "int.est.pl"
   if(plot) plot(intest)
   return(intest)
 }
}

is.point.est.pl<-function(est)
{
 inherits(est, "point.est.pl")
}



summary.point.est.pl<-function(samp, digits=5) 
{
 if (!is.point.est.pl(samp)) stop("\nThe parameter <samp> must be of class 'point.est.pl'.\n")
 cat("\n")
 cat("POINT ESTIMATE SUMMARY (PLOT METHOD)\n")
 cat("-----------------------------------\n")
 cat("creation date   :", samp$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(samp$parents)) {
   cat("      ",paste("(",samp$parents[[i]]$class,", ",samp$parents[[i]]$name,", ",samp$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(samp$seed)) cat("random number seed used: ",samp$seed,"\n")
 cat("\n")
 if (samp$HT) cat("(Horvitz-Thompson estimator used)\n")
 else cat("(Maximum likelihood estimator used)\n")
 n <- sum(samp$sample$detected)
 co <- samp$sample$design$area.coordinates
 a <- sum(abs(co[,1]-co[,3])*abs(co[,2]-co[,4]))
 A <- samp$sample$design$region$length * samp$sample$design$region$width
 cat("Number of groups detected      :", n, "\n")
 cat("Coverage probability           :", signif(a/A,digits), "\n")
 cat("Estimated number of groups     :", round(samp$Nhat.grp), "\n")
 cat("Estimated number of individuals:", round(samp$Nhat.ind), "\n")
 cat("Estimated mean group size      :", signif(samp$Es,digits), "\n")
}



int.est.pl<-function(samp, HT=FALSE, vlevels=c(0.025, 0.975), ci.type="boot.nonpar", nboot=999, plot=T, seed=NULL, ...) 
{
 estimate.pl(samp, HT=HT, vlevels=vlevels, ci.type=ci.type, nboot=nboot, plot=plot, seed=seed, ...)
}

is.int.est.pl<-function(est)
{
 inherits(est, "int.est.pl")
}

summary.int.est.pl<-function(iest, digits=5) 
{
 if (!is.int.est.pl(iest)) stop("\nThe parameter <iest> must be of class 'int.est.pl'.\n")
 cat("\n")
 cat("INTERVAL ESTIMATE SUMMARY\n")
 cat("-------------------------\n")
 cat("creation date   :", iest$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(iest$parents)) {
   cat("      ",paste("(",iest$parents[[i]]$class,", ",iest$parents[[i]]$name,", ",iest$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(iest$seed)) cat("random number seed used: ",iest$seed,"\n")
 cat("\n")
 se<-iest$se
 cv<-iest$cv
 if (iest$HT) cat("(Horvitz-Thompson estimator used)\n")
 else cat("(Maximum likelihood estimator used)\n")
 if (iest$ci.type=="normal") cat("(Normal approximation confidence interval estimator used)\n")
 if (iest$ci.type=="boot.par") cat("(Parametric bootstrap confidence interval estimator used)\n")
 if (iest$ci.type=="boot.nonpar") cat("(Nonparametric bootstrap confidence interval estimator used)\n")
 cat("\n")
 cat("Confidence limit percentiles                :", paste("(",paste(iest$levels,collapse="; "),")",sep=""), "\n")
 cat("\n")
 if(iest$ci.type == "boot.par" | iest$ci.type == "boot.nonpar") 
   cat("Group abundance bootstrap mean              :", signif(iest$boot.mean$Nhat.grp, digits), "\n")
 cat("Group abundance Standard error              :", signif(iest$se$Nhat.grp, digits),"\n")
 cat("Group abundance Coefficient of variance     :", signif(iest$cv$Nhat.grp, digits),"\n")
 cat("Group abundance confidence limit values     :", paste("(",paste(round(iest$ci$Nhat.grp),collapse="; "),")",sep=""), "\n")
 cat("Individual abundance confidence limit values: (not yet implemented)\n")
 cat("Mean group size confidence limit values     : (not yet implemented)\n")
}


plot.int.est.pl<-function(iest, ...) 
#-------------------------------------------------------------------------
# Produces plots of confidence intervals.
# Updates to plots over WiSP V1 include:
#   revised normal approx plotting
#   automatic scaling of text etc. to fit plot region
# Need to generalize to allow plotting of $Nhat.indiv and $Es Cis.
#-------------------------------------------------------------------------
{
 if (!is.int.est.pl(iest)) stop("\nThe parameter <iest> must be of class 'int.est.pl'.\n")
 if(length(iest$levels) != length(iest$ci$Nhat.grp)) stop("Inconsistent levels and CI vector lengths,\n")
 k<-length(iest$levels)
 pars.old<-par(no.readonly=TRUE)
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 if (iest$ci.type == "normal") {
   lower<-iest$ci$Nhat.grp[1]
   upper<-iest$ci$Nhat.grp[k]
   width<-(upper-lower)
   q.lower<-qnorm(iest$levels[1])
   q.upper<-qnorm(iest$levels[k])
   se<-(upper-lower)/(q.upper-q.lower)
   est<-upper-q.upper*se
   xlim<-c(lower-width*0.2, upper+width*0.2)
   x<-seq(xlim[1],xlim[2],length=500)
   y1<-dnorm(x,mean=lower, sd=se)
   y2<-dnorm(x,mean=upper, sd=se)
   y.est<-dnorm(est,mean=upper, sd=se)
   ymax<-max(y1,y2)
   plot(x,y1,type="l", main="Normal-based confidence interval", xlab="Group abundance estimate", ylab="probability density", col="blue")
   lines(x,y1,col="blue", lwd=1.5)
   lines(x,y2, col="red", lwd=1.5)
   lines(c(lower,lower),c(-ymax*0.05,ymax),col="blue",lty=2, lwd=2)
   lines(c(upper,upper),c(-ymax*0.05,ymax),col="red",lty=2, lwd=2)
   lines(c(est,est),c(-ymax*0.05,ymax),lty=3, lwd=2)
   CI<-round(c(lower,upper))
   vlevels<-c(iest$levels[1], iest$levels[k])
   text(CI, -ymax*0.05, paste(CI), adj=c(0.5,1), col=c("blue","red"), xpd=T)
   text(CI, ymax*0.1, paste(round(100*vlevels,1), "%"), adj=c(0.5,0), col=c("blue","red"), xpd=T)
   text(est, -ymax*0.05, paste(round(est)), adj=c(0.5,1), xpd=T)
   text(est, ymax*0.1, paste("point estimate"), adj=c(0.5,0), xpd=T)
 }
 if (iest$ci.type == "boot.par" || iest$ci.type == "boot.nonpar") {
#   hst<-hist(iest$boot.dbn$Nhat.grp, xlab="Group abundance estimate", main="", ...)
   hst<-hist(iest$boot.dbn$Nhat.grp, xlab="Group abundance estimate", main="", ...)
   if (iest$ci.type == "boot.par")  title(main="Parametric bootstrap confidence interval")
   if (iest$ci.type == "nonboot.par")  title(main="nonparametric bootstrap confidence interval")
   ymax<-max(hst$counts)
   lower<-iest$ci$Nhat.grp[1]
   upper<-iest$ci$Nhat.grp[k]
   est<-iest$boot.mean$Nhat.grp
   lines(c(lower,lower),c(-ymax*0.05,ymax*0.1),col="blue",lty=2, lwd=2)
   lines(c(upper,upper),c(-ymax*0.05,ymax*0.1),col="blue",lty=2, lwd=2)
   lines(c(est,est),c(-ymax*0.05,ymax),lty=3, lwd=2)
   CI<-round(c(lower,upper))
   vlevels<-c(iest$levels[1], iest$levels[k])
   text(CI, -ymax*0.05, paste(CI), adj=c(0.5,1), col="blue", xpd=T)
   text(CI, ymax*0.1, paste(round(100*vlevels,1), "%"), adj=c(0.5,0), col="blue", xpd=T)
   text(est, -ymax*0.05, paste(round(est)), adj=c(0.5,1), xpd=T)
   text(est, ymax*0.1, paste("bootstrap mean"), adj=c(0.5,0), xpd=T)
 }
 par(pars.old)
}


is.point.sim.pl<-function(sim)
{
 inherits(sim, "point.sim.pl")
}

plot.point.sim.pl<-function(sim, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 if(length(est)>1) breaks="Sturges"
 for(i in 1:length(est)) {
   if(i>1) windows(height=5, width=4)
   sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
 }
}




summary.point.sim.pl<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), digits=5)
{
 if(!is.point.sim.pl(sim)) 
   stop("Argument <sim>. must be of class point.sim.pl\n")
 if (sim$HT) addtext<-"(Horvitz-Thompson estimator used)"
 else addtext<-"(Maximum likelihood estimator used)"
 summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}


point.sim.pl<-function (pop.spec, design.spec, B=99, HT=FALSE, seed=NULL, show=FALSE, plot=FALSE) 
{
 if (!is.design.pl(design.spec) & !is.pars.design.pl(design.spec)) {
   stop("design.spec must be of class 'design.pl' or 'pars.design.pl'.\n")
 }
 if (!is.population(pop.spec) & !is.pars.population(pop.spec)) {
   stop("pop.spec must be of class 'population' or 'pars.population'.\n")
 }
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
 stats<-c("Nhat.grp","Nhat.ind","Es")
 len<-length(stats)
 res <- matrix(0, nrow = B, ncol = len)
 res <- as.data.frame(res)
 names(res)<-stats
 out.est <- NULL
 for (i in 1:B) {
   if (is.population(pop.spec)) mypop <- pop.spec
   if (is.pars.population(pop.spec)) {
     mypop <- generate.population(pop.spec)
     random.pop<-TRUE
   }
   if (is.design.pl(design.spec)) mydes <- design.spec
   if (is.pars.design.pl(design.spec)) {
     mydes <- generate.design.pl(design.spec)
     random.design<-TRUE
   }
   mysamp <- generate.sample.pl(mypop, mydes)
   if(show) plot(mysamp)
   out.est <- point.est.pl(mysamp, HT = HT)
   res[i, stats] <- out.est[stats]
 }
 colnames(res) <- c("Nhat.grp", "Nhat.ind", "Es")
 parents<-list(wisp.id(pop.spec,newname=as.character(substitute(pop.spec))), wisp.id(design.spec,newname=as.character(substitute(survey.spec))))
 sim<-list(est=res, true=true, HT=HT, random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
 class(sim) <- "point.sim.pl"
 if(plot) plot(sim, est="Nhat.grp")
 return(sim)
}




# Non-plotting functions

plot.pars.design.pl<-function(x,col="black")
{
 plot.text("There is no useful plot for this class of object",col=col)
}

plot.point.est.pl<-function(x,col="black")
{
 plot.text("There is no useful plot for this class of object",col=col)
}





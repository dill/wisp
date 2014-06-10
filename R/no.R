# Based on David's "New NN functions.doc" but with the papametres passed explicitly
# I've had to rename "n" as "n.points"  for internal consistency

# NN estimation functions for Popas:
# ------------------------------------------
#
# Survey:
#
generate.design.no<-function (reg, n.points=2) 
{
    if (!is.region(reg)) 
            stop("\n*** The parameter <reg> must be of type 'region'.\n")
        if (!is.numeric(n.points)) 
            stop("\n*** The number of points must be numeric.\n")
        if (n.points < 1) 
        stop("\n*** You must sample at least two points (n.points>1).\n")
        
    n <- n.points
    nn.x<-runif(n,min=0,max=reg$length)
    nn.y<-runif(n,min=0,max=reg$width)
    des <- list(region = reg, n.points=n, x=nn.x,y=nn.y)
    class(des) <- "design.no"
    return(des)
}



plot.design.no<-function (des, ...) 
# -------------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------------
{
    if (!is.design.no(des)) 
        stop("\nThe parameter <des> must be of type 'design.no'.\n")
    reg<-des$region
    par.was <- par(no.readonly = T)
    len <- reg$length
    width <- reg$width
    margin <- calculate.plot.margin(reg.x = len, reg.y = width, 
        area.x = par.was$fin[1], area.y = par.was$fin[2])
    par(mai = c(margin$y, margin$x, margin$y, margin$x))
    par(new = F)
    plot(0, 0, type = "n", las = 1, xlab = "", ylab = "", xlim = c(0, 
        len), ylim = c(0, width), xaxs = "i", yaxs = "i", xpd = T, 
        ...)
    points(des$x, des$y, pch = 3)
}





is.design.no<-function (des) 
{
    inherits(des, "design.no")
}




#
generate.sample.no<-function (pop, des ,with.neighbours=FALSE) 
# -------------------------------------------------------------------------------------
# Returns groupIDs of objects nearest design points, and distances to design points.
# If with.neighbours==TRUE, also returns groupIDs of objects nearest these objects (thier
#    nearest neighbours), subject to constraint that line from design point to object
#    and thence to neighbour is >90 degrees. Also returns distances between the 
#    objects and nearest neighbours. If a point has no neighbours satisfying the 
#    constraint, the neighbour groupID is NA and the distance is 0.
# -------------------------------------------------------------------------------------
{
    if (!is.population(pop)) 
          stop("\n*** The parameter <pop> must be of type 'population'.\n")
    if (!is.design.no(des)) 
          stop("\n*** The parameter <des> must be of type 'design.no'.\n")
    if (!equal(pop$region, des$region)) 
          stop(paste("\n*** The given population and design were defined", 
          "with different regions.\n"))
    
    pos.x <- pop$posx
    pos.y <- pop$posy
    N <- length(pos.x)
    detected <- rep(0, N)
    u <- des$n.points
#   find distances from points to nearest objects:
    point.distance <- rep(NA, u)
    point.animal <- rep(NA, u)
    for (i in 1:u) {
        indx<-which(sqrt((des$x[i] - pos.x)^2 + (des$y[i] - 
            pos.y)^2) == min(sqrt((des$x[i] - pos.x)^2 + (des$y[i] - 
            pos.y)^2)))
        point.animal[i] <- pop$groupID[indx]
        point.distance[i] <- sqrt((des$x[i] - pos.x[indx])^2 + (des$y[i] - 
            pos.y[indx])^2)
        detected[indx] <- 1
#        cat("\n  Animal: ", animal, "     Distance: ", distance[point.animal], "\n")
    }

    neighbour.distance <- rep(NA, u)
    neighbour.animal <- rep(NA, u)
    if(with.neighbours) {
#     find distances from nearest objects to nearest neighbours at >90 degrees
#     from point-to-nearest-object line:
      neighbour <- rep(0, N)
      skip<-rep(FALSE,u)
      for (i in 1:u) {
        indx<-which(pop$groupID == point.animal[i])
        c.sq<-(pos.x[indx] - pos.x)^2 + (pos.y[indx] - pos.y)^2
        b.sq<-(pos.x[indx] - des$x[i])^2 + (pos.y[indx] - des$y[i])^2
        a.sq<-(des$x[i] - pos.x)^2 + (des$y[i] - pos.y)^2
        cos.A<-(b.sq+c.sq-a.sq)/(2*sqrt(b.sq*c.sq))
        cos.A[indx]<-1
#       exclude point itself and all points at <90 degrees (i.e. with cos.A>0):
        other<-rep(TRUE,N)
        other[indx]<-FALSE
        other[cos.A>0]<-FALSE
        if(sum(other)<1) {
          skip[i]<-TRUE
          neighbour.distance[i]<-0
          neighbour[indx]<-NA
        } else {
          dist<-sqrt((pos.x[indx] - pos.x)^2 + (pos.y[indx] - pos.y)^2)
          dist[!other]<- -99
          n.indx<-which(dist>0 & dist == min(sqrt((pos.x[indx] - pos.x[other])^2 + (pos.y[indx] - pos.y[other])^2)))[1]
          neighbour.animal[i] <- pop$groupID[n.indx]
          neighbour.distance[i] <- sqrt((pos.x[indx] - pos.x[n.indx])^2 + (pos.y[indx] - pos.y[n.indx])^2)
          neighbour[indx] <- n.indx
        }
      }
    }
    samp <- list(population = pop, design = des, detected = detected, 
        point.animal=point.animal, point.distance = point.distance, neighbour.animal=neighbour.animal, neighbour.distance = neighbour.distance)
    class(samp) <- "sample.no"
    return(samp)
}


is.sample.no<-function (samp) 
{
    inherits(samp, "sample.no")
}


summary.sample.no<-function (samp) 
{
    if (!is.sample.no(samp)) 
        stop("\nThe parameter <samp> must be of type 'sample.no'.\n")
    cat("\n")
    cat("SAMPLE SUMMARY (POINT-TO-NEAREST-OBJECT METHOD)\n")
    cat("-----------------------------------------\n\n")
    cat("Number of points: ", samp$design$n.points, "\n")
    cat("Mean of         point-to-object distances    :",mean(samp$point.distance),"\n")
    cat("Mean of squared point-to-object distances    :",mean(samp$point.distance^2),"\n")
    if(length(!is.na(samp$neighbour.distance))>0) {
      cat("Mean of         object-to-neighbour distances:",mean(samp$neighbour.distance),"\n")
      cat("Mean of squared object-to-neighbour distances:",mean(samp$neighbour.distance^2),"\n")
    }
}


plot.sample.no<-function (samp, show.sizes = FALSE, show.exps = FALSE, dsf = 1, whole.population = FALSE,
    pnlines = TRUE, nnlines = TRUE, ...)
# -------------------------------------------------------------------------------
# Only does point-to-nearest-object at present.
# -------------------------------------------------------------------------------
{
    if (!is.sample.no(samp)) 
        stop("\nThe parameter <samp> must be of type 'sample.no'.\n")
    if (!is.numeric(dsf)) 
        stop("\nThe parameter <dsf> must be numeric.\n")
    if (dsf <= 0) 
        stop("\nThe parameter <dsf> must be positive.\n")
    if ((show.sizes != TRUE) & (show.sizes != FALSE))
        stop("\nThe parameter <show.sizes> must be TRUE or FALSE.\n")
    if ((show.exps != TRUE) & (show.exps != FALSE))
        stop("\nThe parameter <show.exps> must be TRUE or FALSE.\n")
    if ((whole.population != TRUE) & (whole.population != FALSE))
        stop("\nThe parameter <whole.population> must be TRUE or FALSE.\n")
    pop <- samp$population
    des <- samp$design
    par.was <- par(no.readonly = TRUE)
    len <- pop$reg$length
    width <- pop$reg$width
    margin <- calculate.plot.margin(reg.x = len, reg.y = width, 
        area.x = par.was$fin[1], area.y = par.was$fin[2])
    par(mai = c(margin$y, margin$x, margin$y, margin$x))
    par(new = FALSE)
    plot(0, 0, type = "n", las = 1, xlab = "", ylab = "", xlim = c(0, 
        len), ylim = c(0, width), xaxs = "i", yaxs = "i", xpd = TRUE,
        ...)
    if (whole.population == TRUE)
        plot(pop, show.sizes = show.sizes, show.exps = show.exps, 
            newplot = FALSE, dsf = dsf, ...)
    inside <- (samp$detected == 1)
    seen <- pop
    seen$groupID <- pop$groupID[inside]
    seen$posx <- pop$posx[inside]
    seen$posy <- pop$posy[inside]
    seen$groupsize <- pop$groupsize[inside]
    seen$types <- pop$types[inside]
    seen$exposure <- pop$exposure[inside]
    plot(seen, show.sizes = show.sizes, show.exps = show.exps, 
        newplot = FALSE, dsf = dsf, group.col = "red", ...)
    points(des$x, des$y, pch = 3)
    if (pnlines) # point-to-nearest-object lines:
        for (i in 1:des$n.points) {
            lines(c(des$x[i], pop$posx[which(samp$pop$groupID==samp$point.animal[i])]), c(des$y[i], pop$posy[which(samp$pop$groupID==samp$point.animal[i])]))
        }
    if (nnlines) # nearest-neighbour lines:
        for (i in 1:des$n.points) {
            indx  <-which(samp$pop$groupID==samp$point.animal[i])
            n.indx<-which(samp$pop$groupID==samp$neighbour.animal[i])
            lines(c(pop$posx[indx], pop$posx[n.indx]), c(pop$posy[indx], pop$posy[n.indx]),col="red")
        }
}



obscure.sample.no<-function(samp)
#----------------------------------------------------------------
# Removes all information about undetected animals from an object
# of class sample.no. Returns object of same class.
#----------------------------------------------------------------
{
if (!is.sample.no(samp)) 
  stop("\n*** <samp> is not an object of type 'sample.no'.\n")
t<-samp
# mark all deteceted animals
detected<-samp$detected
# need to include neighbour animals if they are there:
if(!is.na(samp$neighbour.animal[1])) detected[is.element(samp$population$groupID,samp$neighbour.animal)]<-1
# then filter out all information about others
t$population$groupID<-samp$population$groupID[detected==1]
t$population$posx<-samp$population$posx[detected==1]
t$population$posy<-samp$population$posy[detected==1]
t$population$groupsize<-samp$population$groupsize[detected==1]
t$population$types<-samp$population$types[detected==1]
t$population$exposure<-samp$population$exposure[detected==1]
t$detected<-samp$detected[detected==1]
t$point.animal<-samp$point.animal
t$point.distance<-samp$point.distance
t$neighbour.animal<-samp$neighbour.animal
t$neighbour.distance<-samp$neighbour.distance
t
}



#=================================================
# estimate_nn
# From David's "New NN functions.doc"
#=================================================
#
point.est.no<-function(samp)
{
 estimate.no(samp, ci.type = NULL)
}



int.est.no<-function(samp, vlevels = c(0.025, 0.975), ci.type = "boot.nonpar",
   nboot = 999, plot = T, ... )
{
 estimate.no(samp, vlevels, ci.type, nboot, plot)
}


estimate.no<-function (samp, vlevels = c(0.025, 0.975), ci.type, 
    nboot = 999, plot = T, ...) 
# -------------------------------------------------------------------------------
# Only does point-to-nearest-object at present.
# -------------------------------------------------------------------------------
{
    if (!is.sample.no(samp)) 
        stop("\n*** <samp> is not an object of type 'sample.no'.\n")
    if (!is.null (ci.type) && !(ci.type %in% c("boot.par", "boot.nonpar"))) 
        stop(paste("\n*** Unrecognised <ci.type>. These are valid: ", 
            "'boot.par' and 'boot.nonpar'\n"))
    if (!is.numeric(vlevels)) 
        stop("\n*** All <vlevels> values must be numeric.\n")
    if (any(vlevels < 0) | any(vlevels > 1)) 
        stop("\n*** All <vlevels> values must be between 0 and 1.\n")
    if (!is.numeric(nboot)) 
        stop("\n*** <nboot> must be numeric.\n")
    if (nboot < 1) 
        stop("\n*** <nboot> must be at least 1.\n")
    if ((plot != T) & (plot != F)) 
        stop("\n*** <plot> must be TRUE or FALSE.\n")
    pop <- samp$population
    des <- samp$design
    reg.length <- des$region$length
    reg.width <- des$region$width
    n.points <- des$n.points
    ra <- reg.length * reg.width
    x1 <- samp$point.distance[!is.na(samp$point.distance)]
    x2 <- samp$neighbour.distance[!is.na(samp$neighbour.distance)]
    if (length(x1) != n.points) {
        cat("\n Debug error: length(x)!=n.points.\n")
        cat(length(x1))
        cat(n.points)
        stop("Execution stopped \n")
    }
    Nmle <- round(n.points * ra/(pi * sum(x1^2)))
    
    if (is.null(ci.type) )
    {
     pointest <- list(Nhat = Nmle)
     class(pointest) <- "point.est.no"
     return(pointest)
    }
    
    if (ci.type == "boot.par") {
        K <- pi * Nmle / ra
        nb <- rep(0, nboot)
        for (i in 1:nboot)
        {
         U <- runif(n.points)
         xboot <- sqrt( -log(U) / K )
         nb[i] <- round(n.points * ra/(pi * sum(xboot^2)))
        }
        
        boot.dbn <- list (Nhat.grp = nb)
        
        Nhat.boot <- mean(nb)
        boot.mean <- list(Nhat.grp = Nhat.boot)
        
        indices <- round(vlevels * nboot)
        indices <- ifelse(indices < 1, 1, indices)
        
        sortedNhats <- sort(nb)
        
        CI.Nhat <- sortedNhats[indices]
        ci <- list(Nhat.grp = CI.Nhat)
                
        # x <- seq(sortedNhats[1], sortedNhats[nboot], length = 100)
        # y <- density(sortedNhats, kernel = "gaussian", n = 100, from = sortedNhats[1], 
        #    to = sortedNhats[nboot])$y
        # pcol <- "blue"
        # text2 <- paste("\nParametric Bootstrap with", nboot, 
        #     "replicates")
    }
    else if (ci.type == "boot.nonpar") {
        nb <- numeric(nboot)
        for (ib in 1:nboot) {
            xb <- sample(x1, n.points, replace = T)
            nb[ib] <- round(n.points * ra/(pi * sum(xb^2)))
        }
        
        boot.dbn <- list (Nhat.grp = nb)
        
        Nhat.boot <- mean(nb)
        
        boot.mean <- list(Nhat.grp = Nhat.boot)
        
        nb <- round(sort(nb), 0)
        cin <- round(nboot * vlevels, 0)
        cin <- ifelse(cin < 1, 1, cin)
        CI.Nhat <- nb[cin]
        ci <- list(Nhat.grp = CI.Nhat)
        
        # x <- seq(nb[1], nb[nboot], length = 100)
        # y <- density(nb, kernel = "gaussian", n = 100, from = nb[1], 
        #     to = nb[nboot])$y
        # pcol <- "red"
        # text2 <- paste("\nNon-parametric Bootstrap with", nboot, 
        #     "replicates")
    }
    
    if (plot) {

        if (ci.type == "boot.par") {
            valid.Nhat <- nb[nb != Inf]
            nvalid <- length(valid.Nhat)
            main <- paste("Parametric Bootstrap Group abundance distribution:\n", 
                "(", nboot, "replicates)")
            y <- hist(valid.Nhat, prob = T, plot = F, ...)
            hist(valid.Nhat, prob = T, xlab = "Estimate", ylab = "Density", 
                main = main, ...)
            yhi <- max(y$intensities * 0.05)
            ylo <- 0
            CI <- round(CI.Nhat)
            segments(CI, ylo, CI, yhi, col = "blue", lwd = 2, 
                xpd = T)
            text(CI, ylo * 1.05, paste(CI), adj = c(0.5, 1), 
                col = "blue", cex = 0.8, xpd = T)
            text(CI, yhi * 1.5, paste(round(100 * vlevels, 1), 
                "%"), adj = c(0.5, 0), col = "blue", cex = 0.8, 
                xpd = T)
            title(main)
        }
        if (ci.type == "boot.nonpar") {
            valid.Nhat <- nb[nb != Inf]
            nvalid <- length(valid.Nhat)
            main <- paste("Nonparametric Bootstrap Group abundance distribution:\n", 
                "(", nboot, "replicates)")
            y <- hist(valid.Nhat, prob = T, plot = F, ...)
            hist(valid.Nhat, prob = T, xlab = "Estimate", ylab = "Density", 
                main = main, ...)
            yhi <- max(y$intensities * 0.05)
            ylo <- 0
            CI <- round(CI.Nhat)
            segments(CI, ylo, CI, yhi, col = "blue", lwd = 2, 
                xpd = T)
            text(CI, ylo * 1.05, paste(CI), adj = c(0.5, 1), 
                col = "blue", cex = 0.8, xpd = T)
            text(CI, yhi * 1.5, paste(round(100 * vlevels, 1), 
                "%"), adj = c(0.5, 0), col = "blue", cex = 0.8, 
                xpd = T)
            title(main)
        }



#        par.was <- par(no.readonly = T)
#        plot(x, y, xlab = "population size", ylab = "likelihood", 
#            type = "n", xaxs = "i", yaxs = "i", ...)
#        lines(x, y, type = "l", col = pcol, lwd = 2)
#        yhi <- max(y * 0.025)
#        ylo <- -yhi
#        segments(ci, ylo, ci, yhi, col = pcol, lwd = 2, xpd = T)
#        text(ci, ylo * 1.05, paste(ci), adj = c(0.5, 1), col = pcol, 
#            cex = 0.5, xpd = T)
#        text(ci, yhi * 1.5, paste(round(100 * vlevels, 1), "%"), 
#            adj = c(0.5, 0), col = pcol, cex = 0.5, xpd = T)
#        text1 <- paste("Likelihood for N")
#        title(paste(text1, text2))
#        par(par.was)
    }
    
    intest <- list(levels = vlevels, ci = ci, boot.mean = boot.mean, boot.dbn = boot.dbn)
    class(intest) <- "int.est.no"
    return(intest)
    
}

is.int.est.no<-function(est)
{
	inherits(est, "int.est.no")
}

is.point.est.no<-function(est)
{
	inherits(est, "point.est.no")
}

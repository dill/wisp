
# dp functions

setpars.design.dp<-function (reg, n.transects = 1, n.units = 1, visual.range, percent.on.effort = 1) 
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
    if ((percent.on.effort < 0) | (percent.on.effort > 1)) 
        stop("\n*** The value 'percent.on.effort' must be inside [0, 1].\n")
    if (2 * visual.range * n.transects >= reg$length) 
        stop("\n*** The visual range of neighboured paths is superposed.\n")
    parents<-list(wisp.id(reg,newname=as.character(substitute(reg))))
    pars.design.dp <- list(region = reg, n.transects = n.transects, n.units = n.units, 
       visual.range = visual.range, percentage.on.effort = percent.on.effort, parents=parents, created=date())
    class(pars.design.dp) <- "pars.design.dp"
    return(pars.design.dp)
}

is.pars.design.dp<-function (despars) 
{
 inherits(despars,"pars.design.dp")
}




summary.pars.design.dp<-function(des, digits=5)
{
# check class:
 if (!is.pars.design.dp(des)) stop("\nThe parameter <des> must be of class 'pars.design.dp'.\n")
 cat("\n")
 cat("DOUBLE PLATFORM DESIGN PARAMETER SUMMARY\n")
 cat("----------------------------------------\n")
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



plot.pars.design.dp<-function(x,col="black")
{
 plot.text("There is no useful plot for this class of object",col=col)
}


generate.design.dp<-function (pars, seed=NULL) 
{
 parents<-list(wisp.id(pars,newname=as.character(substitute(pars))))
 pars.lt<-pars
 class(pars.lt)<-"pars.design.lt"
 des<-generate.design.lt(pars, seed)
 class(des)<-"design.dp"
 des$parents<-parents
 des
}

is.design.dp <- function (des) 
{
 inherits(des, "design.dp")
}

summary.design.dp<-function(des, digits=5)
{
# check class:
 if (!is.design.dp(des)) stop("\nThe parameter <des> must be of class 'design.dp'.\n")
 cat("\n")
 cat("DOUBLE PLATFORM DESIGN SUMMARY\n")
 cat("------------------------------\n")
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

plot.design.dp<-function (des, show.paths = FALSE) 
{
 if (!is.design.dp(des)) stop("\n*** <des> must be of type 'design.dp'\n")
 old.par<-par(no.readonly=TRUE)
 plot.region(des$region, reset.pars=FALSE)
 plot.strips.lt(des, show.paths=show.paths)
 par(old.par)
}




setpars.survey.dp<-function (pop, des, adjust.interactive = FALSE, theta.obs1 = 1, theta.obs2 = 1, 
    theta.exp = 0.1, theta.dist = -1, ...) 
{
    if (!is.population(pop)) 
        stop("\nThe parameter <pop> must be of type 'population'.\n")
    if (!is.design.dp(des)) 
        stop("\nThe parameter <des> must be of type 'design.dp'.\n")
    if (!equal(pop$region, des$region)) 
        stop(paste("\n*** The given population and design were defined", 
            "with different regions.\n"))
    if (!is.numeric(theta.obs1) | !is.numeric(theta.obs2) | !is.numeric(theta.exp) | 
        !is.numeric(theta.dist)) 
        stop("\nAll theta values must be numeric.\n")
    if (theta.obs1 < 0) 
        stop("\n<theta.obs1> cannot be negative.\n")
    if (theta.obs2 < 0) 
        stop("\n<theta.obs2> cannot be negative.\n")
    if (theta.exp < 0) 
        stop("\n<theta.exp> cannot be negative.\n")
    if (theta.dist > 0) 
        stop("\n<theta.dist> cannot be positive.\n")
 parents<-list(wisp.id(pop,newname=as.character(substitute(pop))), 
   wisp.id(des,newname=as.character(substitute(des))))
    if (adjust.interactive) {
        range <- des$visual.range
        stepsize <- 0.1
        exit <- F
        refresh <- T
        which.legend <- 1
        mode <- "bandwidth"
        n <- 100
        min.exposure <- rep(pop$minexposure, n)
        max.exposure <- rep(pop$maxexposure, n)
        x <- seq(-range * 1.2, range * 1.2, length = n)
        distance <- abs(x)
        cat("\n")
        cat("********************************\n")
        cat("***    press 'h' for help    ***\n")
        cat("********************************\n\n")
        while (exit == F) {
            if (refresh == T) {
                p1.min <- prob.detect.dp(theta.obs1, theta.exp, 
                  theta.dist, min.exposure, distance)
                p1.max <- prob.detect.dp(theta.obs1, theta.exp, 
                  theta.dist, max.exposure, distance)
                p2.min <- prob.detect.dp(theta.obs2, theta.exp, 
                  theta.dist, min.exposure, distance)
                p2.max <- prob.detect.dp(theta.obs2, theta.exp, 
                  theta.dist, max.exposure, distance)
                p1.min[distance > range] <- 0
                p1.max[distance > range] <- 0
                p2.min[distance > range] <- 0
                p2.max[distance > range] <- 0
                plot(1, 1, xlim = c(min(x), max(x)), ylim = c(0, 
                  1), type = "n", xlab = "distance", ylab = "detection probability", 
                  ...)
                lines(x, p1.min, col = "blue")
                lines(x, p1.max, col = "blue")
                lines(x, p2.min, col = "red")
                lines(x, p2.max, col = "red")
                segments(-range, -0.1, -range, 0.1, lwd = 2)
                segments(range, -0.1, range, 0.1, lwd = 2)
                if (which.legend == 1) 
                  legend(min(x), 1, c("observer 1", "observer 2"), 
                    pch = c(-1, -1), lty = c(1, 1), lwd = c(2, 
                      2), col = c("blue", "red"), bg = "gray90")
                if (which.legend == 2) {
                  text <- c(paste("theta(obs 1) =", theta.obs1), 
                    paste("theta(obs 2) =", theta.obs2), paste("theta(expos) =", 
                      theta.exp), paste("theta(dist) =", theta.dist))
                  legend(min(x), 1, text, bg = "gray90")
                }
            }
            refresh <- F
            valid <- F
            prompt <- paste("<", mode, "> mode, stepsize <", 
                stepsize, "> : ", sep = "")
            cat(prompt)
            cmd <- readline()
            if (cmd == "q") {
                exit <- T
                valid <- T
            }
            if (cmd == "h") {
                cat("\n\n------------------- HELP --------------------\n")
                cat("\n")
                cat("The image shows the detection probabilities of both\n")
                cat("observer by assuming minimum and maximum exposure \n")
                cat("values.\n")
                cat("These functions can be adjusted by various parameter\n")
                cat("(see below). But you have to keep in mind that the\n")
                cat("effects of these parameters may overlie each other\n")
                cat("in some cases.\n")
                cat("\n")
                cat("Commands:\n")
                cat("b : switch to changing of <b>andwidth:\n")
                cat("    Changes the 'distance' between the lower\n")
                cat("    and upper detection function of both observer.\n")
                cat("f : switch to changing of <f>all off:\n")
                cat("    Changes the 'shape' of all detection functions.\n")
                cat("    It can be adjusted how it fast falls off from its\n")
                cat("    maximum value.\n")
                cat("1 : switch to changing of height of observer <1>:\n")
                cat("    The 'height' of both detection functions of\n")
                cat("    observer 1 can be changed.\n")
                cat("2 : switch to changing of height of observer <2>:\n")
                cat("    The 'height' of both detection functions of\n")
                cat("    observer 2 can be changed.\n")
                cat("+ : increase value\n")
                cat("- : decrease value\n")
                cat("s : change <s>tepsize of + and -\n")
                cat("l : toggle <l>egend display\n")
                cat("q : <q>uit\n")
                cat("h : show help\n")
                cat("---------------------------------------------\n\n")
                valid <- T
            }
            if (cmd == "1") {
                mode <- "height 1"
                valid <- T
            }
            if (cmd == "2") {
                mode <- "height 2"
                valid <- T
            }
            if (cmd == "b") {
                mode <- "bandwidth"
                valid <- T
            }
            if (cmd == "f") {
                mode <- "fall off"
                valid <- T
            }
            if (cmd == "l") {
                if (which.legend < 2) {
                  which.legend <- which.legend + 1
                }
                else {
                  which.legend <- 0
                }
                refresh <- T
                valid <- T
            }
            if ((cmd == "+") | (cmd == "-")) {
                dbandwidth <- 0
                dfalloff <- 0
                dheight1 <- 0
                dheight2 <- 0
                if (cmd == "+") {
                  if (mode == "bandwidth") 
                    dbandwidth <- 1
                  if (mode == "fall off") 
                    dfalloff <- 1
                  if (mode == "height 1") 
                    dheight1 <- 1
                  if (mode == "height 2") 
                    dheight2 <- 1
                  refresh <- T
                }
                if (cmd == "-") {
                  if (mode == "bandwidth") 
                    dbandwidth <- -1
                  if (mode == "fall off") 
                    dfalloff <- -1
                  if (mode == "height 1") 
                    dheight1 <- -1
                  if (mode == "height 2") 
                    dheight2 <- -1
                  refresh <- T
                }
                t.obs1 <- theta.obs1 + stepsize * dheight1
                t.obs2 <- theta.obs2 + stepsize * dheight2
                t.exp <- theta.exp + stepsize * dbandwidth
                t.dist <- theta.dist + stepsize * dfalloff
                if (t.obs1 < 0) {
                  cat("*** Boundary reached\n")
                  theta.obs1 <- 0
                }
                else theta.obs1 <- t.obs1
                if (t.obs2 < 0) {
                  cat("*** Boundary reached\n")
                  theta.obs2 <- 0
                }
                else theta.obs2 <- t.obs2
                if (t.exp < 0) {
                  cat("*** Boundary reached\n")
                  theta.exp <- 0
                }
                else theta.exp <- t.exp
                if (t.dist > 0) {
                  cat("*** Boundary reached\n")
                  theta.dist <- 0
                }
                else theta.dist <- t.dist
                valid <- T
            }
            if (cmd == "s") {
                cat("\nNew stepsize value : ")
                input <- readline()
                new.stepsize <- as.numeric(input)
                if (is.na(new.stepsize) | !is.numeric(new.stepsize)) {
                  cat("\n\n*** Invalid stepsize value\n")
                }
                else {
                  if (new.stepsize < 0) {
                    cat("\n\n*** Invalid stepsize value\n")
                  }
                  else {
                    stepsize <- new.stepsize
                  }
                }
                cat("\n")
                valid <- T
            }
            if (valid == F) 
                cat("*** Unrecognized command\n")
        }
        cat("\nSelected parameter values stored for later use ...\n\n")
    }
    pars <- list(population = pop, design = des, theta.obs1 = theta.obs1, 
        theta.obs2 = theta.obs2, theta.exp = theta.exp, theta.dist = theta.dist, parents=parents, created=date())
    class(pars) <- "pars.survey.dp"
    return(pars)
}

is.pars.survey.dp<-function (survpars) 
{
 inherits(survpars,"pars.survey.dp")
}


summary.pars.survey.dp<-function(pars, digits=5, plot=FALSE) 
{
    if (!is.pars.survey.dp(pars)) 
        stop("\nThe parameter <pars> must be of type 'pars.survey.dp'.\n")
    cat("\n")
    cat("SURVEY PARS SUMMARY (DOUBLE PLATFORM METHOD)\n")
    cat("--------------------------------------------\n")
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
    cat("------------------ \n")
    cat(" p(detect) = exp(linear.predictor)/(1 + exp(linear.predictor))\n")
    cat(" where: \n")
    cat(" linear.predictor = theta.obs + theta.exp*exposure + theta.dist*distance\n")
    cat(" and for observer 1, theta.obs = ", pars$theta.obs1,"\n")
    cat("     for observer 2, theta.obs = ", pars$theta.obs2,"\n")
    cat("                     theta.exp = ", pars$theta.exp,"\n")
    cat("                    theta.dist = ", pars$theta.dist,"\n")
    mu1.minexp<-integrate(prob.detect.dp, lower=0, upper=w, exposure=pars$population$minexposure, 
                 theta.obs=pars$theta.obs1, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist)$value
    mu2.minexp<-integrate(prob.detect.dp, lower=0, upper=w, exposure=pars$population$minexposure, 
                 theta.obs=pars$theta.obs2, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist)$value
    if (pars$population$minexposure != pars$population$maxexposure) {
    mu1.maxexp<-integrate(prob.detect.dp, lower=0, upper=w, exposure=pars$population$maxexposure, 
                 theta.obs=pars$theta.obs1, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist)$value
    mu2.maxexp<-integrate(prob.detect.dp, lower=0, upper=w, exposure=pars$population$maxexposure, 
                 theta.obs=pars$theta.obs2, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist)$value
    N<-length(pars$population$exposure)
    mu1<-rep(0,N)
    mu2<-rep(0,N)
    for(i in 1:N) {
     mu1[i]<-integrate(prob.detect.dp, lower=0, upper=w, exposure=pars$population$exposure[i], 
                 theta.obs=pars$theta.obs1, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist)$value
     mu2[i]<-integrate(prob.detect.dp, lower=0, upper=w, exposure=pars$population$exposure[i], 
                 theta.obs=pars$theta.obs2, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist)$value
    }
    mu1<-mean(mu1)
    mu2<-mean(mu2)
      cat("\n")
      cat("Effective strip with at minimum exposure (obs1):", signif(mu1.minexp,digits), "\n")
      cat("Effective strip with at maximum exposure (obs1):", signif(mu1.maxexp,digits), "\n")
      cat("              Mean effective strip width (obs1):", signif(mu1,digits), "\n")
      cat("Effective strip with at minimum exposure (obs2):", signif(mu2.minexp,digits), "\n")
      cat("Effective strip with at maximum exposure (obs2):", signif(mu2.maxexp,digits), "\n")
      cat("              Mean effective strip width (obs2):", signif(mu2,digits), "\n")
    }else {
      cat("                   Effective strip width (obs1):", signif(mu1.minexp,digits), "\n")
      cat("                   Effective strip width (obs2):", signif(mu2.minexp,digits), "\n")
    }
}



plot.pars.survey.dp<-function(pars)
{
 pars.old<-par(no.readonly=TRUE)
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 w<-pars$design$visual.range
 x <- seq(0, w, length = 100)
 minexp <- rep(pars$population$minexposure, 100)
 maxexp <- rep(pars$population$maxexposure, 100)
 f1max <- prob.detect.dp(theta.obs=pars$theta.obs1, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist, 
                         exposure=maxexp, dist=x)
 f2max <- prob.detect.dp(theta.obs=pars$theta.obs2, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist, 
                         exposure=maxexp, dist=x)
 plot(x, f1max, type = "l", ylim = c(0, 1), xlab = "Distance", ylab = "Detection probability", lwd=3*cex, col="blue")
 lines(x, f2max, lwd=3*cex, col="red")
 if (pars$population$minexposure != pars$population$maxexposure) {
   f1min <- prob.detect.dp(theta.obs=pars$theta.obs1, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist, 
                           exposure=minexp, dist=x)
   f2min <- prob.detect.dp(theta.obs=pars$theta.obs2, theta.exp=pars$theta.exp, theta.dist=pars$theta.dist, 
                           exposure=minexp, dist=x)
   lines(x, f1min, lty = 2, lwd=3*cex, col="blue")
   lines(x, f2min, lty = 2, lwd=3*cex, col="red")
   title("Detection functions for min and max exposure")
 }
 else {
   title("Detection function")
 }
 labels<-paste("Observer",as.character(1:2))
 legend(w,1,labels,lty=1,col=c("blue","red"),xjust=1,yjust=1) 
 par(pars.old)
}




prob.detect.dp<-function (theta.obs, theta.exp, theta.dist, exposure, distance) 
{
#    if (length(distance) != length(exposure)) 
#        stop(paste("\nThe number of given distance and exposure values", 
#            "is not equal"))
    l <- theta.obs + theta.exp * exposure + theta.dist * distance
    p.detect <- exp(l)/(1 + exp(l))
    return(p.detect)
}





generate.sample.dp<-function (pars, seed=NULL) 
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
    theta.obs1 <- pars$theta.obs1
    theta.obs2 <- pars$theta.obs2
    theta.exp <- pars$theta.exp
    theta.dist <- pars$theta.dist
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
    p.obs1 <- rep(0, n.groups)
    p.obs2 <- rep(0, n.groups)
    p.obs1[detectable] <- prob.detect.dp(theta.obs1, theta.exp, theta.dist, exposure[detectable], distance[detectable])
    p.obs2[detectable] <- prob.detect.dp(theta.obs2, theta.exp, theta.dist, exposure[detectable], distance[detectable])
    det.obs1 <- rbinom(n.groups, 1, p.obs1)
    det.obs1[!detectable] <- NA
    det.obs2 <- rbinom(n.groups, 1, p.obs2)
    det.obs2[!detectable] <- NA
    detected<-1*(det.obs1 | det.obs2)
    samp <- list(population = pop, design = des, detected=detected, detected.observer1 = det.obs1, 
        detected.observer2 = det.obs2, distance = distance, transect = transect, parents=parents, created=date(), seed=seed)
    class(samp) <- "sample.dp"
    return(samp)
}

is.sample.dp <- function (samp) 
{
 inherits(samp, "sample.dp")
}


summary.sample.dp<-function (samp, digits=5) 
{
    if (!is.sample.dp(samp)) 
        stop("\nThe parameter <samp> must be of type 'sample.dp'.\n")
    cat("\n")
    cat("SAMPLE SUMMARY (DOUBLE PLATFORM METHOD)\n")
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
    ob1<-(samp$detected.observer1 == 1) & !is.na(samp$detected.observer1)
    ob2<-(samp$detected.observer2 == 1) & !is.na(samp$detected.observer2)
    dup<-(samp$detected.observer1 == 1) & !is.na(samp$detected.observer1) & 
         (samp$detected.observer2 == 1) & !is.na(samp$detected.observer2)
    x1i <- samp$distance[ob1]
    z1i <- samp$population$groupsize[ob1]
    x2i <- samp$distance[ob2]
    z2i <- samp$population$groupsize[ob2]
    xdi <- samp$distance[dup]
    zdi <- samp$population$groupsize[dup]
    cat("Truncation distance              :", w, "\n")
    cat("Number of lines                  :", K, "\n")
    cat("Total line length                :", L, "\n")
    cat("Survey area                      :", A, "\n")
    cat("Covered area                     :", a, "\n")
    cat("Percentage of survey area covered:", signif(100*a/A,digits), "%\n")
    cat("\n")
    cat("Number groups detected by observer 1   :", sum(samp$detected.observer1[ob1]), "\n")
    cat("Number groups detected by observer 2   :", sum(samp$detected.observer2[ob2]), "\n")
    cat("Number detected by both observers      :", sum(samp$detected.observer1[dup]), "\n")
    cat("\n")
    cat("        Mean detected group size (obs1):", signif(mean(z1i),digits), "\n")
    cat("        Mean detected group size (obs2):", signif(mean(z2i),digits), "\n")
    cat("        Mean detected group size (both):", signif(mean(zdi),digits), "\n")
    cat("    Mean detected perp. distance (obs1):", signif(mean(x1i),digits), "\n")
    cat("    Mean detected perp. distance (obs2):", signif(mean(x2i),digits), "\n")
    cat("    Mean detected perp. distance (both):", signif(mean(xdi),digits), "\n")
}


plot.sample.dp<-function (samp, show.sizes = TRUE, show.exps = TRUE, dsf = 0.35, whole.population = FALSE, 
    show.paths = FALSE, show.legend = TRUE, type="hist", ...) 
{
    if (!is.sample.dp(samp)) 
        stop("\n*** The parameter <samp> must be of type 'sample.dp'.\n")
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
    if ((show.legend != T) & (show.legend != F)) 
        stop("\n*** The parameter <show.legend> must be TRUE or FALSE.\n")
    pop <- samp$population
    des <- samp$design
    par.was <- par(no.readonly = T)
    group.col <- c("blue", "red", "yellow")
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 chist.ind<-matrix(FALSE, nrow=length(samp$detected.observer1), ncol=3)
 seenby<-chist.ind
 whosaw<-c("obs 1","obs 2","both")
 seenby[,1]<-(samp$detected.observer1 == 1 & !is.na(samp$detected.observer1))
 seenby[,2]<-(samp$detected.observer2 == 1 & !is.na(samp$detected.observer2))
 seenby[,3]<-(samp$detected.observer1 == 1 & !is.na(samp$detected.observer1) &
          samp$detected.observer2 == 1 & !is.na(samp$detected.observer2))
 chist.ind[,1]<-(seenby[,1] & !seenby[,2])
 chist.ind[,2]<-(seenby[,2] & !seenby[,1])
 chist.ind[,3]<-(seenby[,1] & seenby[,2])
 if(type=="hist") {
   par(mfrow=c(2,2))
   max.x<-max(samp$distance[seenby[,1]], samp$distance[seenby[,2]])
   n<-max(length(samp$distance[chist.ind[,1]]),length(samp$distance[chist.ind[,2]]),length(samp$distance[chist.ind[,3]]))
   breaks<-seq(0,max.x,length=1+floor(max(11,(1+log2(n)))))
   h<-matrix(0,nrow=3, ncol=length(breaks)-1)
   for(i in 1:3) {
     xi <- samp$distance[seenby[,i]]
     h[i,]<-hist(xi, breaks=breaks, xlab = "Perpendicular Distance", ylab="Frequency", 
          main=paste("Distribution of detections by ",whosaw[i],sep=""), col=group.col[i])$counts
   }
   p<-h[3,]/h[2,]
   plot(rep(breaks,rep(2,length(breaks))),c(0,rep(p,rep(2,length(p))),0), type="l", lwd=2, ylim=c(0,1),
        xlab="Perpendicular Distance", ylab="Proportion", main="Duplicate proportions", col=group.col[1])
   p<-h[3,]/h[1,]
   lines(rep(breaks,rep(2,length(breaks))),c(0,rep(p,rep(2,length(p))),0), lwd=2, lty=2, col=group.col[2])
 } else {
   par(mfrow=c(1,1))
   plot.region(des$region, reset.pars=FALSE)
   plot.strips.lt(des, show.paths=show.paths)
    if (whole.population == T) 
    plot.groups(pop, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="black")
    for (i in 1:3) {
        inside <- chist.ind[,i]
        if (any(inside == TRUE)) {
            seen <- pop
            seen$groupID <- pop$groupID[inside]
            seen$posx <- pop$posx[inside]
            seen$posy <- pop$posy[inside]
            seen$groupsize <- pop$groupsize[inside]
            seen$types <- pop$types[inside]
            seen$exposure <- pop$exposure[inside]
    plot.groups(seen, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col=group.col[i])
        }
    }
    if (show.legend)
    	  wid <- samp$population$region$width 
    	  len <- samp$population$region$length
        legend(0.05 * len, 0.95 * wid, c("obs 1 only", "obs 2 only", 
            "both"), pch = c(19, 19, 19), col = group.col, bg = "gray90", cex=0.75*cex)
 }
 par(par.was)
}






obscure.sample.dp<-function (samp) 
{
    if (!is.sample.dp(samp)) 
        stop("\n*** <samp> is not an object of type 'sample.dp'.\n")
    t <- samp
    obs.sum <- t$detected.observer1 + t$detected.observer2
    t$population$groupID <- samp$population$groupID[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$population$posx <- samp$population$posx[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$population$posy <- samp$population$posy[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$population$groupsize <- samp$population$groupsize[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$population$types <- samp$population$types[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$population$exposure <- samp$population$exposure[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$distance <- samp$distance[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$detected <- samp$detected[!is.na(samp$detected.observer1) & obs.sum != 0]
    t$detected.observer1 <- samp$detected.observer1[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$detected.observer2 <- samp$detected.observer2[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$transect <- samp$transect[!is.na(samp$detected.observer1) & 
        obs.sum != 0]
    t$created<-date()
    t
}


point.est.dp<-function (samp, model = "~distance", plot=FALSE) 
{
    if (!is.sample.dp(samp)) stop("Argument 'samp' must be of class sample.dp")
    parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
    n <- sum(samp$detected[!is.na(samp$detected)])
    if (!(n > 0))  stop("Sample size of zero!")
    L <- sum(abs(samp$design$start.y - samp$design$end.y))
    L<-L*samp$design$effective.n.transects/samp$design$n.transects
    w <- samp$design$visual.range
    A <- samp$design$region$length * samp$design$region$width

    scale.num <- (2 * w * L)/A
    obs.sum <- (samp$detected.observer1 + samp$detected.observer2)
    t1 <- samp$detected.observer1[!is.na(samp$detected.observer1) & obs.sum != 0]
    t2 <- samp$detected.observer2[!is.na(samp$detected.observer2) & obs.sum != 0]
    obs.mat <- matrix(0, nrow = length(t1), ncol = 2)
    obs.mat[, 1] <- t1
    obs.mat[, 2] <- t2
#    distinct.obs <- sum(obs.mat) - sum(ifelse((obs.mat[, 1] + 
#        obs.mat[, 2]) == 2, 1, 0))
    obs.samp <- data.converter.dp(obscure.sample.dp(samp))
    est <- dp.model.fit(obs.samp, left = 0, condmodel = model, 
        showit = FALSE, TI = FALSE, estimate = TRUE, refit = TRUE, abnd.scale = scale.num, 
        d.obs = n, t.length = L, v.width = w)
    est$p.cover<-scale.num
    est$sample<-samp; est$model<-model; est$parents<-parents; est$created<-date()
    class(est)<-"point.est.dp"
    if(plot) plot(est)
    est
}


# From here to next line like that below, are functions used by point.est.dp
#**************************************************************************
    is.linear.logistic <- function(xmat, g0model, zdim, width) {
        xmat$distance <- rep(width/2, dim(xmat)[1])
        beta <- rep(1, zdim)
        logit1 <- mean(beta %*% t(setcov(xmat, g0model)$cov))
        xmat$distance <- rep(width, dim(xmat)[1])
        logit2 <- mean(beta %*% t(setcov(xmat, g0model)$cov))
        xmat$distance <- rep(0, dim(xmat)[1])
        logit0 <- mean(beta %*% t(setcov(xmat, g0model)$cov))
        if ((logit2 - logit0)/(logit1 - logit0) <= 2.00001 & 
            (logit2 - logit0)/(logit1 - logit0) >= 1.99999) 
            integral.numeric <- F
        else integral.numeric <- T
    }
    pdot.dsr.integrate.logistic <- function(width, beta, x, integral.numeric, 
        BT, models) {
        integratelogistic <- function(x, models, beta, width) {
            if (!is.null(version$language) & version$language == 
                "R") 
                integrate(logisticbyx, lower = 0, upper = width, 
                  subdivisions = 10, rel.tol = 0.01, abs.tol = 0.01, 
                  x = x, models = models, beta = beta)$value
            else integrate(logisticbyx, lower = 0, upper = width, 
                subdivisions = 10, rel.tol = 0.01, abs.tol = 0.01, 
                x = x, models = models, beta = beta)$integral
        }
        integratelogisticdup <- function(x1, x2, models, beta, 
            width) {
            if (!is.null(version$language) & version$language == 
                "R") 
                integrate(logisticdupbyx, lower = 0, upper = width, 
                  subdivisions = 10, rel.tol = 0.01, abs.tol = 0.01, 
                  x1 = x1, x2 = x2, models = models, beta = beta)$value
            else integrate(logisticdupbyx, lower = 0, upper = width, 
                subdivisions = 10, rel.tol = 0.01, abs.tol = 0.01, 
                x1 = x1, x2 = x2, models = models, beta = beta)$integral
        }
        integratelogistic.analytic <- function(x, models, beta, 
            width) {
            b <- logit(logisticbyz(x, 0, models, beta))
            p <- b - logit(logisticbyz(x, 1, models, beta))
            b <- exp(-b)
            int <- width * b/(1 + b)
            int[p != 0] <- width - log(1 + b[p != 0] * exp(p[p != 
                0] * width))/p[p != 0] + log(1 + b[p != 0])/p[p != 
                0]
            return(int)
        }
        if (integral.numeric) {
            if (is.logistic.constant(x[x$platform == 1, ], models$g0model, 
                width)) {
                int1 <- rep(integratelogistic(x = (x[x$platform == 
                  1, ])[1, ], models, beta, width), dim(x[x$platform == 
                  1, ])[1])
            }
            else {
                int1 <- NULL
                for (i in 1:(dim(x[x$platform == 1, ])[1])) int1 <- c(int1, 
                  integratelogistic(x = (x[x$platform == 1, ])[i, 
                    ], models, beta, width))
            }
            if (!BT) {
                if (is.logistic.constant(x[x$platform == 2, ], 
                  models$g0model, width)) {
                  int2 <- rep(integratelogistic(x = (x[x$platform == 
                    2, ])[1, ], models, beta, width), dim(x[x$platform == 
                    2, ])[1])
                }
                else {
                  int2 <- NULL
                  for (i in 1:(dim(x[x$platform == 2, ])[1])) int2 <- c(int2, 
                    integratelogistic(x = (x[x$platform == 2, 
                      ])[i, ], models, beta, width))
                }
            }
            else int2 <- NULL
        }
        else {
            int1 <- integratelogistic.analytic(x[x$platform == 
                1, ], models = models, beta = beta, width = width)
            if (!BT) 
                int2 <- integratelogistic.analytic(x[x$platform == 
                  2, ], models = models, beta = beta, width = width)
            else int2 <- NULL
        }
        if (!BT) {
            if (is.logistic.constant(x[x$platform == 1, ], models$g0model, 
                width) & is.logistic.constant(x[x$platform == 
                2, ], models$g0model, width)) {
                int3 <- rep(integratelogisticdup(x1 = (x[x$platform == 
                  1, ])[1, ], x2 = (x[x$platform == 2, ])[1, 
                  ], models, beta, width), dim(x[x$platform == 
                  2, ])[1])
            }
            else {
                int3 <- NULL
                for (i in 1:(dim(x[x$platform == 1, ])[1])) int3 <- c(int3, 
                  integratelogisticdup(x1 = (x[x$platform == 
                    1, ])[i, ], x2 = (x[x$platform == 2, ])[i, 
                    ], models, beta, width))
            }
            pdot <- int1 + int2 - int3
        }
        else {
            int3 <- NULL
            pdot <- int1
        }
        return(list(pdot = pdot, int1 = int1, int2 = int2, int3 = int3))
    }
    logit <- function(p) {
        log(p/(1 - p))
    }
    logisticbyz <- function(x, distance, models, beta) {
        x$distance <- rep(distance, length(x$distance))
        zlist <- setcov(x, models$g0model)
        g0(beta, zlist$cov)
    }
    is.logistic.constant <- function(xmat, g0model, width) {
        xmat$distance <- rep(width, dim(xmat)[1])
        zlist <- setcov(xmat, g0model)
        beta <- rep(1, zlist$dim)
        logit1 <- beta %*% t(zlist$cov)
        if (all(logit1[1] == logit1)) 
            return(TRUE)
        else return(FALSE)
    }
    logisticdupbyz <- function(xmat, g0model, width) {
        xmat$distance <- rep(width, dim(xmat)[1])
        zlist <- setcov(xmat, g0model)
        beta <- rep(1, zlist$dim)
        logit1 <- beta %*% t(zlist$cov)
        if (all(logit1[1] == logit1)) 
            return(TRUE)
        else return(FALSE)
    }
    logisticdupbyx <- function(distance, x1, x2, models, beta) {
        xlist <- as.list(x1)
        xlist$distance <- distance
        xmat <- expand.grid(xlist)
        gx1 <- g0(beta, setcov(xmat, models$g0model)$cov)
        xlist <- as.list(x2)
        xlist$distance <- distance
        xmat <- expand.grid(xlist)
        gx1 * g0(beta, setcov(xmat, models$g0model)$cov)
    }
    invlogit <- function(x) {
        exp(x)/(1 + exp(x))
    }
    setcov <- function(dmat, model) {
        if (model == "~.") {
            n <- 0
            x <- NULL
        }
        else if (model == "~1") {
            n <- 1
            x <- as.matrix(rep(1, dim(dmat)[1]))
        }
        else {
            if (length(grep(",", as.character(model))) > 0) {
                var1 <- all.vars(as.formula(model))
                for (i in 2:length(var1)) {
                  var1[i] <- paste("+", var1[i])
                }
                var1[1] <- paste("~", var1[1])
                var2 <- NULL
                for (i in 1:length(var1)) {
                  if (i == 1) {
                    var2 <- var1[1]
                  }
                  else {
                    var2 <- paste(var2, var1[i])
                  }
                }
                print(var2)
                model <- var2
            }
            x <- model.matrix(eval(parse(text = model)), data = dmat)
            if (!is.matrix(x)) 
                x <- as.matrix(x)
            n <- dim(x)[2]
        }
        return(list(dim = n, cov = x))
    }
    g0 <- function(beta, z) {
        if (is.matrix(z)) 
            exp(z %*% beta)/(1 + exp(z %*% beta))
        else exp(as.matrix(z) %*% beta)/(1 + exp(as.matrix(z) %*% 
            beta))
    }
    fit.lt <- function(dmat, left = 0, width = NULL, shapeindex = c(0), 
        ftype = "hn", scalemodel = "~1", showit = F, initialvalues = NULL, 
        lowerbounds = NULL, upperbounds = NULL, doeachint = F, 
        estimate = T, refit = T) {
        if (!is.null(lowerbounds)) 
            setlower <- TRUE
        else setlower <- FALSE
        if (!is.null(upperbounds)) 
            setupper <- TRUE
        else setupper <- FALSE
        if (!is.null(version$language) & version$language == 
            "R") 
            library(modreg)
        save.options <- options()
        options(contrasts = c("contr.treatment", "contr.poly"))
        if (!is.null(dmat$seen)) 
            dmat <- dmat[dmat$seen == 1, ]
        else dmat$seen <- rep(1, dim(dmat)[1])
        if (is.null(width)) {
            width <- max(dmat$distance)
            xmat <- dmat
        }
        else xmat <- dmat[dmat$distance <= width, ]
        if (left > 0) {
            xmat <- xmat[xmat$distance >= left, ]
            xmat$distance <- xmat$distance - left
            width <- width - left
        }
        wlist <- setcov(xmat, scalemodel)
        if (ftype == "hn") {
            detfct <- hndetfct
            shapeindex <- c(0)
        }
        else {
            detfct <- hzdetfct
            if (max(shapeindex) == 0) 
                shapeindex <- c(1)
            if (min(shapeindex) < 1 | max(shapeindex) > 1) {
                cat("invalid shape index")
                stop
            }
        }
        intercept.only <- F
        if (scalemodel == "~platform" | scalemodel == "~1") 
            intercept.only <- T
        if (ftype == "hn" & !intercept.only) 
            cgftab <- tablecgf(eps = 1e-05, gridint = 0.05, theta1 = c(1), 
                detfct = detfct)
        else cgftab <- NULL
        bounded <- TRUE
        while (bounded) {
            if (is.null(initialvalues)) 
                initialvalues <- setinitialvalues(xmat, T, F, 
                  ftype, shapeindex, NULL, scalemodel, NULL, 
                  dimensions = c(wlist$dim, 0, 0), width = width)
            if (is.null(lowerbounds)) 
                lowerbounds <- apply(matrix(c(initialvalues - 
                  0.5 * abs(initialvalues), initialvalues - 1), 
                  byrow = F, ncol = 2), 1, min)
            if (is.null(upperbounds)) 
                upperbounds <- apply(matrix(c(initialvalues + 
                  0.5 * abs(initialvalues), initialvalues + 1), 
                  byrow = F, ncol = 2), 1, max)
            if (!is.null(version$language) & version$language == 
                "R") {
                itconverged <- F
                while (!itconverged) {
                  lt <- optim(initialvalues, flt, method = "L-BFGS-B", 
                    lower = lowerbounds, upper = upperbounds, 
                    detfct = detfct, ftype = ftype, shapeindex = shapeindex, 
                    TCI = F, x = xmat, w = wlist$cov, wdim = wlist$dim, 
                    width = width, showit = showit, intercept.only = intercept.only, 
                    cgftab = cgftab, doeachint = doeachint)
                  if (lt$converge == 0) {
                    itconverged <- TRUE
                    lt$aux <- list(detfct = detfct, ftype = ftype, 
                      shapeindex = shapeindex, TCI = F, x = xmat, 
                      w = wlist$cov, wdim = wlist$dim, width = width, 
                      showit = showit, intercept.only = intercept.only, 
                      cgftab = cgftab, doeachint = doeachint)
                  }
                  else {
                    cat("\nRefitting ...\n")
                    initialvalues <- lt$par
                  }
                }
            }
            else {
                lt <- nlminb(initialvalues, flt, lower = lowerbounds, 
                  upper = upperbounds, max.iter = 1000, max.fcal = 1000, 
                  detfct = detfct, ftype = ftype, shapeindex = shapeindex, 
                  TCI = F, x = xmat, w = wlist$cov, wdim = wlist$dim, 
                  width = width, showit = showit, intercept.only = intercept.only, 
                  cgftab = cgftab, doeachint = doeachint)
            }
            bounded <- FALSE
            if (any(abs(lt$par - lowerbounds) < 1e-06)) {
                if (!setlower) 
                  bounded <- TRUE
                cat("\nOne or more parameters was at a lower bound\n")
                cat("Parameters:   ", lt$par, "\n")
                cat("Lower bounds: ", lowerbounds, "\n")
            }
            if (any(abs(lt$par - upperbounds) < 1e-06)) {
                if (!setupper) 
                  bounded <- TRUE
                cat("\nOne or more parameters was at an upper bound\n")
                cat("Parameters:   ", lt$par, "\n")
                cat("Upper bounds: ", upperbounds, "\n")
            }
            if (!refit) 
                bounded <- FALSE
            if (bounded) {
                initialvalues <- lt$par
                if (!setlower) 
                  lowerbounds <- NULL
                if (!setupper) 
                  upperbounds <- NULL
                cat("\nRefitting ...\n")
            }
        }
        lt$model <- list(scalemodel = scalemodel)
        if (lt$message == "FALSE CONVERGENCE") 
            cat("\nModel fitting did not converge.  Try different initial values or different model\n")
        else if (estimate) 
            lt <- summary.lt(lt)
        options(save.options)
        return(lt)
    }
    hndetfct <- function(distance, theta, w, std = FALSE) {
        if (std) 
            exp(-(distance/sqrt(2))^2)
        else exp(-((distance/(sqrt(2) * scalevalue(theta, w)))^2))
    }
    setinitialvalues <- function(dmat, BT, TCI, ftype, shapeindex, 
        g0model, scalemodel, fullscalemodel, dimensions, width) {
        initialvalues <- NULL
        npar <- max(shapeindex)
        if (ftype == "haz") 
            initialvalues <- c(initialvalues, rep(2, npar))
        if (ftype != "uni") {
            if (BT) {
                initialvalues <- c(initialvalues, lm(eval(parse(text = paste("log(distance+width/1000)", 
                  scalemodel))), data = dmat[dmat$seen == 1, 
                  ])$coeff)
                if (TCI) {
                  tmat <- dmat[dmat$platform == 1, ]
                  tmat$seen <- NULL
                  scalepar <- lm(eval(parse(text = paste("log(distance+width/1000)", 
                    fullscalemodel))), data = tmat)$coeff
                  initialvalues <- c(initialvalues, fit.lt(tmat, 
                    width = width, initial = scalepar, scalemodel = fullscalemodel, 
                    estimate = F)$par)
                }
            }
            else {
                scalepar <- lm(eval(parse(text = paste("log(distance+width/1000)", 
                  scalemodel))), data = dmat[dmat$seen == 1, 
                  ])$coeff
                initialvalues <- c(initialvalues, fit.lt(dmat, 
                  width = width, scalemodel = scalemodel, initialvalues = scalepar, 
                  estimate = F)$par)
                if (TCI) {
                  tmat <- dmat[dmat$platform == 1, ]
                  tmat$seen <- NULL
                  scalepar <- lm(eval(parse(text = paste("log(distance+width/1000)", 
                    fullscalemodel))), data = tmat)$coeff
                  initialvalues <- c(initialvalues, fit.lt(tmat, 
                    width = width, initial = scalepar, scalemodel = fullscalemodel, 
                    estimate = F)$par)
                }
            }
        }
        if (dimensions[3] != 0) {
            if (!is.null(version$language) & version$language == 
                "R") 
                initialvalues <- c(initialvalues, glm(eval(parse(text = paste("seen", 
                  g0model))), data = dmat, family = binomial)$coeff)
            else initialvalues <- c(initialvalues, glm(eval(parse(text = paste("seen", 
                g0model))), data = dmat, family = binomial, link = logit)$coeff)
        }
        return(initialvalues)
    }
    flt <- function(fpar, ftype, shapeindex, TCI, x, w, wdim, 
        detfct, width, showit, intercept.only, cgftab, doeachint) {
        if (showit) 
            cat("par = ", fpar, "\n")
        theta1 <- c(getpar(fpar, 1, 1, ftype, shapeindex, c(0), 
            wdim), getpar(fpar, 2, 1, ftype, shapeindex, c(0), 
            wdim))
        p1 <- detfct(x$distance, theta1, w)
        p1[p1 < 1e-05] <- 1e-05
        int1 <- integratedetfct(detfct, cgftab, ftype, width, 
            w1 = w, w2 = NULL, theta1, c(0), intercept.only, 
            doeachint)
        int1[is.infinite(int1)] <- width
        lnl <- -sum(log(p1/int1))
        if (showit) 
            cat("lt lnl = ", lnl, "\n")
        return(lnl)
    }
    getpar <- function(fpar, ptype, position, ftype, shapeindex, 
        zdim, wdim, allwdim) {
        if (ptype == 1) {
            if (ftype == "haz") 
                return(fpar[shapeindex[position]])
            else return(NULL)
        }
        else if (ptype == 2) {
            npar <- max(shapeindex)
            par <- fpar
            if (position <= 2) {
                if (wdim != 0) 
                  return(par[(npar + 1):(npar + wdim)])
            }
            else {
                if (allwdim != 0) 
                  return(par[(npar + 1 + wdim):(npar + wdim + 
                    allwdim)])
            }
        }
        else {
            if (zdim != 0) 
                npar <- max(shapeindex) + wdim + allwdim
            par <- fpar
            return(par[(npar + 1):(npar + zdim)])
        }
    }
    scalevalue <- function(theta, w) {
        if (is.matrix(w)) 
            exp(w %*% theta)
        else exp(as.matrix(w) %*% theta)
    }
    integratedetfct <- function(detfct, cgftab, ftype, width, 
        w1, w2, theta1, theta2, intercept.only, doeachint, fct = NULL) {
        predict.smooth.spline <- function(object, x, deriv = 0, 
            ...) {
            if (missing(x)) {
                if (deriv == 0) 
                  return(object[c("x", "y")])
                else x <- object$x
            }
            fit <- object$fit
            if (is.null(fit)) 
                stop("not a valid smooth.spline object")
            else predict(fit, x, deriv, ...)
        }
        if (!is.matrix(w1)) 
            wmat1 <- as.matrix(w1)
        else wmat1 <- w1
        if (!is.matrix(w2) & !is.null(w2)) 
            wmat2 <- as.matrix(w2)
        else wmat2 <- w2
        if (intercept.only) {
            if (is.null(fct)) 
                if (!is.null(version$language) & version$language == 
                  "R") 
                  return(integrate(detfct, lower = 0, upper = width, 
                    theta = theta1, w = t(wmat1[1, ]))$value)
                else return(integrate(detfct, lower = 0, upper = width, 
                  theta = theta1, w = t(wmat1[1, ]))$integral)
            else if (!is.null(version$language) & version$language == 
                "R") 
                return(integrate(fct, lower = 0, upper = width, 
                  theta1 = theta1, theta2 = theta2, detfct = detfct, 
                  w1 = t(wmat1[1, ]), w2 = t(wmat2[1, ]))$value)
            else return(integrate(fct, lower = 0, upper = width, 
                theta1 = theta1, theta2 = theta2, detfct = detfct, 
                w1 = t(wmat1[1, ]), w2 = t(wmat2[1, ]))$integral)
        }
        else {
            if (ftype == "haz") 
                theta <- theta1[2:length(theta1)]
            else theta <- theta1
            if (doeachint) {
                if (is.null(fct)) 
                  return(apply(width/scalevalue(theta, wmat1), 
                    1, gstdint, theta = theta, gridint = 0, xmin = 0, 
                    detfct = detfct) * scalevalue(theta, wmat1))
                else return(apply(cbind(wmat1, wmat2), 1, fct, 
                  theta1 = theta1, theta2 = theta2, lower = 0, 
                  upper = width, detfct = detfct, wdim = dim(wmat1)[2]))
            }
            else {
                if (ftype == "haz") 
                  cgftab <- tablecgf(eps = 0.01, gridint = 0.05, 
                    theta1 = theta1, detfct = detfct)
                return(predict.smooth.spline(cgftab, as.vector(width/scalevalue(theta, 
                  wmat1)))$y * scalevalue(theta, wmat1))
            }
        }
    }
    io.glm <- function(datavec, fitformula, eps = 1e-08, iterlimit = 100, 
        GAM = F, gamplot = T) {
        done <- F
        i <- 1
        plotobj <- NULL
        fitformula <- as.formula(fitformula)
        while (i <= iterlimit & !done) {
            if (GAM) {
                ioglm <- gam(formula = fitformula, family = binomial, 
                  data = datavec)
            }
            else {
                ioglm <- glm(formula = fitformula, family = binomial, 
                  data = datavec)
            }
            coeff <- ioglm$coeff
            fittedp <- ioglm$fitted.values
            if (i == 1) {
                oldmodel <- ioglm
                oldcoeff <- coeff
                oldp <- fittedp
            }
            else {
                reldiff <- max(abs(invlogit(coeff) - invlogit(oldcoeff))/invlogit(oldcoeff))
                if (is.na(reldiff)) {
                  print("Can't calculate regression coefficients - model has not converged")
                  print(" - last fit used for estimation")
                  ioglm <- oldmodel
                  done <- T
                }
                if (reldiff < eps & !done) {
                  done <- T
                }
                else {
                  oldmodel <- ioglm
                  oldcoeff <- coeff
                  oldp <- fittedp
                }
            }
            if (!done) {
                oldoff <- datavec$offsetvalue
                if (GAM) 
                  off <- -log(invlogit(ioglm$linear.predictor - 
                    datavec$offsetvalue))
                else off <- -log(invlogit(ioglm$linear.predictors - 
                  datavec$offsetvalue))
                datavec$offsetvalue[datavec$plat == "1"] <- off[datavec$plat == 
                  "2"]
                datavec$offsetvalue[datavec$plat == "2"] <- off[datavec$plat == 
                  "1"]
            }
            i <- i + 1
        }
        if (GAM & gamplot) {
            assign("fitformula", fitformula)
            assign("fitformula", NULL)
        }
        if (!done) {
            datavec$offsetvalue <- oldoff
            warning("Iteration limit exceeded - last fit used for estimation")
        }
        list(glm = ioglm, offsetvalue = datavec$offsetvalue, 
            plotobj = plotobj)
    }
    summary.lt <- function(ltmodel) {
        save.options <- options()
        options(contrasts = c("contr.treatment", "contr.poly"))
        fpar <- ltmodel$par
        theta1 <- c(getpar(fpar, 1, 1, ltmodel$aux$ftype, ltmodel$aux$shapeindex, 
            ltmodel$aux$zdim, ltmodel$aux$wdim), getpar(fpar, 
            2, 1, ltmodel$aux$ftype, ltmodel$aux$shapeindex, 
            ltmodel$aux$zdim, ltmodel$aux$wdim))
        width <- ltmodel$aux$width
        detfct <- ltmodel$aux$detfct
        x <- ltmodel$aux$x
        w <- ltmodel$aux$w
        wdim <- ltmodel$aux$wdim
        ftype <- ltmodel$aux$ftype
        intercept.only <- ltmodel$aux$intercept.only
        cgftab <- ltmodel$aux$cgftab
        int1 <- integratedetfct(detfct, cgftab, ftype, width, 
            w, NULL, theta1, theta1, intercept.only, F)
        pdot <- int1/width
        cat("\nPrimary detection function parameters", "\n")
        if (ftype == "hn") 
            cat("Theta: ", theta1, "\n")
        if (ftype == "haz") 
            cat("Theta: ", theta1[2:length(theta1)], "\n")
        if (ftype == "hn") 
            cat("Avg. Scale: ", mean(scalevalue(theta1, w)), 
                "\n")
        else if (ftype == "uni") 
            cat("Avg. Scale: ", width, "\n")
        else {
            cat("Shape parameter: ", theta1[1], "\n")
            cat("Avg.Scale: ", mean(scalevalue(theta1[2:length(theta1)], 
                w)), "\n")
        }
        n <- length(x$distance)
        cat("AICc = ", 2 * ltmodel$value + 2 * length(fpar) * 
            n/(n - length(fpar) - 1), "\n")
        cat("\nAbundance estimation", "\n")
        cat("Number of observations : ", length(x$distance), 
            "\n")
        cat("Mean detection probability : ", mean(pdot), "\n")
        if (intercept.only) 
            Nhat <- length(x$distance)/pdot
        else Nhat <- sum(1/pdot)
        cat("Estimated N : ", Nhat, "\n")
        cat("Estimated D : ", Nhat/(2 * width), "\n")
        ltmodel$pdot.list <- list(int1 = int1, pdot = pdot)
        ltmodel$Nhat <- Nhat
        options(save.options)
        return(ltmodel)
    }
data.converter.dp <- function(samp) {
        n<-length(samp$population$groupID)
        rows <- 2 * n
        oddrow <- seq(1, rows, 2)
        plat <- rep(c(1, 2), n)
        dpobsdat<-data.frame(
          ID=rep(samp$population$groupID, rep(2,n)),
          distance=rep(samp$distance, rep(2,n)),
          size=rep(samp$population$groupsize, rep(2,n)),
          exposure=rep(samp$population$exposure, rep(2,n)),
          types=rep(samp$population$types, rep(2,n)),
          platform=plat,
          seen=rep(0,rows)
        )
        dpobsdat$seen[oddrow]<-samp$detected.observer1
        dpobsdat$seen[oddrow+1]<-samp$detected.observer2
        dpobsdat$seen[is.na(dpobsdat$seen)]<-0
        dpobsdat
    }
dp.model.fit <- function(dmat, left = 0, width = NULL, ftype = "hn", 
        condmodel, uncondmodel = "~1", showit = F, initialvalues = NULL, 
        TI = T, lowerbounds = NULL, upperbounds = NULL, estimate = T, 
        refit = T, abnd.scale, d.obs, t.length, v.width, plotp=TRUE) {
        GAM <- F
        if (length(grep(",", as.character(condmodel))) > 0) {
            cat("Function has good reason to believe this is a GAM")
            GAM <- T
        }
        cat("\n")
        save.options <- options()
        options(contrasts = c("contr.treatment", "contr.poly"))
        if (length(dmat$seen[dmat$platform == 1]) != length(dmat$seen[dmat$platform == 
            2])) 
            stop("Number of records for primary platform not equal to number for secondary platform")
        timesseen <- dmat$seen[dmat$platform == 1] + dmat$seen[dmat$platform == 
            2]
        dmat$timesseen <- rep(0, dim(dmat)[1])
        dmat$timesseen[dmat$platform == 1] <- timesseen
        dmat$timesseen[dmat$platform == 2] <- timesseen
        if (is.null(width)) {
            xmat <- dmat[dmat$timesseen > 0, ]
            width <- max(xmat$distance)
        }
        else xmat <- dmat[dmat$distance <= width & dmat$timesseen > 
            0, ]
        if (left > 0) {
            xmat <- xmat[xmat$distance >= left, ]
            xmat$distance <- xmat$distance - left
            width <- width - left
        }
        b <- dim(xmat)[2]
        for (i in 1:b) {
            if (is.factor(xmat[, i]) == T) {
                xmat[, i] <- factor(xmat[, i])
            }
        }
        if (dim(xmat)[1] == 0) 
            stop("no data to analyze")
        xmat$offsetvalue <- rep(0, dim(xmat)[1])
        detfct.cond <- io.glm(xmat, as.formula(paste("seen", 
            condmodel, "+offset(offsetvalue)")), GAM = GAM)
        cond.det.obsprob <- invlogit(detfct.cond$glm$linear.predictor - 
            detfct.cond$offsetvalue)
        p1 <- cond.det.obsprob[xmat$platform == 1]
        p2 <- cond.det.obsprob[xmat$platform == 2]
        cond.det.eventprob <- p1^xmat$seen[xmat$platform == 1] * 
            (1 - p1)^(1 - xmat$seen[xmat$platform == 1]) * p2^xmat$seen[xmat$platform == 
            2] * (1 - p2)^(1 - xmat$seen[xmat$platform == 2])
        cond.det.totalprob <- p1 + p2 - p1 * p2
        lnlU3 <<- sum(log(cond.det.eventprob)) - sum(log(cond.det.totalprob))
        newdat <- xmat
        newdat$distance <- rep(0, length(newdat$distance))
        if (GAM) {
            pred.glmAtZero <- predict.gam(detfct.cond$glm, newdat)
        }
        else {
            pred.glmAtZero <- predict.glm(detfct.cond$glm, newdat)
        }
        if(plotp) {
          plotres<-50
          plotdat1 <- xmat
          plotdat2 <- xmat
          plotdat1$platform <- rep(1, length(plotdat1$platform))
          plotdat2$platform <- rep(2, length(plotdat2$platform))
          plotx<-seq(0,v.width,length=plotres)
          p1.x<-rep(0,plotres)
          p2.x<-rep(0,plotres)
          p.x<-rep(0,plotres)
          for(i in 1:plotres) {
            plotdat1$distance <- rep(plotx[i], length(plotdat1$distance))
            plotdat2$distance <- rep(plotx[i], length(plotdat2$distance))
            if (GAM) {
                temp1<-predict.gam(detfct.cond$glm, plotdat1, type="response")
                temp2<-predict.gam(detfct.cond$glm, plotdat2, type="response")
                p1.x[i] <- mean(temp1)
                p2.x[i] <- mean(temp2)
                p.x[i]<-mean(temp1+temp2-temp1*temp2)
            }
            else {
                temp1 <- predict.glm(detfct.cond$glm, plotdat1, type="response")
                temp2 <- predict.glm(detfct.cond$glm, plotdat2, type="response")
                p1.x[i] <- mean(temp1)
                p2.x[i] <- mean(temp2)
                p.x[i]<-mean(temp1+temp2-temp1*temp2)
            }
          }
        }
        cond.det.obsprob <- invlogit(pred.glmAtZero)
        modelio <<- detfct.cond$glm
        p1 <- cond.det.obsprob[xmat$platform == 1]
        p2 <- cond.det.obsprob[xmat$platform == 2]
        g0 <- p1 + p2 - p1 * p2
        mod.sum <- summary.glm(detfct.cond$glm)
        detfct.cond.npar <- length(detfct.cond$glm$coeff)
        AIC3 <- -2 * lnlU3 + 2 * detfct.cond.npar
        if (TI) {
            dat <- xmat[xmat$platform == 1, ]
            dat$seen <- rep(1, length(dat$distance))
            if (ftype == "hn") 
                detfct.pooled <- fit.lt(dat, scalemodel = uncondmodel, 
                  ftype = "hn", left = 0, width = width, initialvalues = initialvalues, 
                  lowerbounds = lowerbounds, upperbounds = upperbounds, 
                  doeachint = F, estimate = F, refit = T)
            else detfct.pooled <- fit.lt(dat, scalemodel = uncondmodel, 
                ftype = "haz", left = 0, width = width, initialvalues = initialvalues, 
                lowerbounds = lowerbounds, upperbounds = upperbounds, 
                doeachint = F, estimate = F, refit = T, shapeindex = c(1))
            lnlU2 <- -detfct.pooled$value
            AIC2 <- -2 * lnlU2 + 2 * length(detfct.pooled$par)
            prob.det <- detfct.pooled$pdot.list$pdot * g0
        }
        else {
            detfct.pooled <- NULL
            if (GAM) {
                integral.numeric <- T
            }
            else integral.numeric <- is.linear.logistic(xmat, 
                condmodel, detfct.cond.npar, width)
            models <- list(g0model = condmodel, scalemodel = NULL, 
                fullscalemodel = NULL)
            pdot.list <- pdot.dsr.integrate.logistic(width, detfct.cond$glm$coeff, 
                xmat, integral.numeric, F, models)
            prob.det <- pdot.list$pdot/width
            lnlU2 <- sum(log(cond.det.totalprob)) - sum(log(prob.det * 
                width))
            AIC2 <- -2 * lnlU2
        }
        options(save.options)
        g0.out <- matrix(c(mean(p1), mean(p2), mean(g0)), nrow = 3, 
            ncol = 1, dimnames = list(c("observer 1", "observer 2", 
                "combined  "), ""))
        Es = sum(xmat$size[xmat$platform == 1]/prob.det)/sum(1/prob.det)
        Nhat.unscaled <- c(sum(1/prob.det), sum(xmat$size[xmat$platform == 
            1]/prob.det))
        Nhat.scaled <- Nhat.unscaled/abnd.scale
        model.summary.display <- function(coeffs) {
            list(coefficients = coeffs)
        }
#        model.out <- list(Nhat.grp = Nhat.scaled[1], Nhat.ind = Nhat.scaled[2], 
#            Es = Es, prob.det = d.obs/Nhat.scaled[1], mu = v.width * 
#                d.obs/Nhat.scaled[1], nL = d.obs/t.length, average.g0 = g0.out, 
#            log.Likelihood = lnlU3 + lnlU2, AIC = AIC2 + AIC3, 
#            model.summary = model.summary.display(mod.sum$coefficients), 
#            model.call = mod.sum$call)
        model.out <- list(Nhat.grp = Nhat.scaled[1], Nhat.ind = Nhat.scaled[2], 
            Es = Es, phat = d.obs/Nhat.unscaled[1], mu = v.width * 
                d.obs/Nhat.unscaled[1], nL = d.obs/t.length, average.g0 = g0.out, w=v.width,
            log.likelihood = lnlU3 + lnlU2, AIC = AIC2 + AIC3, 
            model.summary = model.summary.display(mod.sum$coefficients), plotx=plotx, plotp1=p1.x, plotp2=p2.x, plotp=p.x)

    }

#**************************************************************************

is.point.est.dp<-function (est) 
{
    inherits(est, "point.est.dp")
}
    
summary.point.est.dp<-function(est, digits=5) 
{
 if (!is.point.est.dp(est)) stop("\nThe parameter <samp> must be of class 'point.est.dp'.\n")
 cat("\n")
 cat("POINT ESTIMATE SUMMARY (DOUBLE PLATFORM METHOD)\n")
 cat("----------------------------------------------\n")
 cat("creation date   :", est$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(est$parents)) {
   cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
 }
 cat("\n")
 n <- sum(est$sample$detected[!is.na(est$sampl$detected)])
    w <- est$sample$design$visual.range
    K <- est$sample$design$effective.n.transects
    frac.L.in<-est$sample$design$effective.n.transects/est$sample$design$n.transects
    L <- sum(abs(est$sample$design$start.y - est$sample$design$end.y))*frac.L.in
    a <- 2 * w * L
    A <- est$sample$population$region$width * est$sample$population$region$length
    ob1<-(est$samp$detected.observer1 == 1) & !is.na(est$samp$detected.observer1)
    ob2<-(est$samp$detected.observer2 == 1) & !is.na(est$samp$detected.observer2)
    dup<-(est$samp$detected.observer1 == 1) & !is.na(est$samp$detected.observer1) & 
         (est$samp$detected.observer2 == 1) & !is.na(est$samp$detected.observer2)

    cat("Truncation distance                 :", w, "\n")
    cat("Number of lines                     :", K, "\n")
    cat("Total line length in survey region  :", L, "\n")
    cat("Survey area                         :", A, "\n")
    cat("Covered area                        :", a, "\n")
    cat("Percentage of survey area covered   :", signif(100*a/A, digits=digits), "%\n")
 cat("\n")
    cat("Number of different groups detected:",n,"\n")
    cat("Mean encounter rate per transect    :",signif(est$nL, digits=digits),"\n")
    cat("\n")
    cat("Number groups detected by observer 1:", sum(est$samp$detected.observer1[ob1]), "\n")
    cat("Number groups detected by observer 2:", sum(est$samp$detected.observer2[ob2]), "\n")
    cat("Number detected by both observers   :", sum(est$samp$detected.observer1[dup]), "\n")
    cat("\n")
    cat("Detection function Model            : Logistic: ",est$model,"\n")
    cat("  Paramteter estimation summary:\n")
    print(est$model.summary)
    cat("Estimated probability of detection on the transect line:\n")
    for(i in 1:3) 
      cat("                        ",rownames(est$average.g0)[i],": ",est$average.g0[[i]],"\n")
    cat("Effective strip half-width          :",signif(est$mu, digits=digits),"\n")
    cat("Mean detection probability          :",signif(est$mu/w, digits=digits),"\n")
    cat("Effective percentage area covered   :",signif((100*a/A)*est$mu/w, digits=digits),"%\n")
    cat("\n")
    cat("Estimated group abundance           :",round(est$Nhat.grp),"\n")
    cat("Estimated individual abundance      :",round(est$Nhat.ind),"\n")
    cat("Mean group size                     :",signif(est$Es, digits=digits),"\n")
    cat("\n")
    cat("Log-likelihood                      :",est$log.likelihood,"\n")
    cat("AIC                                 :",est$AIC,"\n")
}

plot.point.est.dp<-function (est, breaks=10, title = TRUE) 
#----------------------------------------------------------------------------------------------
# NOTE: At present only plots duplicate proportions and fits for each platform.
# Should add combined fit, but can only generate FI estimate at present and that leads to 
# a very bad combined fit in some cases. When PI estimation is implemented, should add
# code for combined fit by un-commenting code below.
#----------------------------------------------------------------------------------------------
{
 w<-est$sample$design$visual.range
 group.col <- c("blue", "red", "yellow")
 old.par<-par(no.readonly=TRUE)
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 chist.ind<-matrix(FALSE, nrow=length(est$sample$detected.observer1), ncol=3)
 seenby<-chist.ind
 whosaw<-c("obs 1","obs 2","both")
 seenby[,1]<-(est$sample$detected.observer1 == 1 & !is.na(est$sample$detected.observer1))
 seenby[,2]<-(est$sample$detected.observer2 == 1 & !is.na(est$sample$detected.observer2))
 seenby[,3]<-(est$sample$detected.observer1 == 1 & !is.na(est$sample$detected.observer1) &
          est$sample$detected.observer2 == 1 & !is.na(est$sample$detected.observer2))
 chist.ind[,1]<-(seenby[,1] & !seenby[,2])
 chist.ind[,2]<-(seenby[,2] & !seenby[,1])
 chist.ind[,3]<-(seenby[,1] & seenby[,2])
#layout(matrix(c(1,1,2,2,0,3,3,0), 2, 4, byrow = TRUE))
 par(mfrow=c(2,1))
 max.x<-max(est$sample$distance[seenby[,1]], est$sample$distance[seenby[,2]])
 n<-max(length(est$sample$distance[chist.ind[,1]]), 
        length(est$sample$distance[chist.ind[,2]]),
        length(est$sample$distance[chist.ind[,3]]))
 breaks<-seq(0,max.x,length=1+floor(max(11,(1+log2(n)))))
 h<-matrix(0,nrow=3, ncol=length(breaks)-1)
 for(i in 1:3) {
   xi <- est$sample$distance[seenby[,i]]
   h[i,]<-hist(xi, breaks=breaks, plot=FALSE)$counts
 }
 xlim=c(0,min(max(breaks),max(est$plotx)))
 p<-h[3,]/h[2,]
 plot(rep(breaks,rep(2,length(breaks))),c(0,rep(p,rep(2,length(p))),0), type="l", xlim=xlim, ylim=c(0,1),
      xlab="Perpendicular distance", ylab="Detection probability", main="Observer 1 dup. ppn and det. fn.", col=group.col[1])
 lines(est$plotx[est$plotx<=xlim[2]*1.05], est$plotp1[est$plotx<=xlim[2]*1.05], col=group.col[1])
 p<-h[3,]/h[1,]
 plot(rep(breaks,rep(2,length(breaks))),c(0,rep(p,rep(2,length(p))),0), type="l", xlim=xlim, ylim=c(0,1),
      xlab="Perpendicular distance", ylab="Detection probability", main="Observer 2 dup. ppn and det. fn.", col=group.col[2])
 lines(est$plotx[est$plotx<=xlim[2]*1.05], est$plotp2[est$plotx<=xlim[2]*1.05], col=group.col[2])
# p.dot<-hist(est$sample$distance[chist.ind[,1] | chist.ind[,3]], breaks=breaks, plot=FALSE)$counts
# c<-sum(est$plotp)*length(p.dot)/(sum(p.dot)*length(est$plotp))
# plot(rep(breaks,rep(2,length(breaks))),c(0,rep(p.dot*c,rep(2,length(p.dot))),0), type="l", 
#      ylim=c(0,max(1,max(p.dot*c))), xlab="Perpendicular distance", ylab="Detection probability", 
#      main="Combined observers' distance distribution and fitted function")
# lines(est$plotx[est$plotx<=xlim[2]*1.05], est$plotp[est$plotx<=xlim[2]*1.05])
 par(old.par)
}






int.est.dp<-function (sampl, ci.type = "boot.nonpar", nboot = 999, vlevels = c(0.025, 0.975), 
                      model = "~distance", plot = FALSE, show.all = FALSE, seed=NULL) 
{
    if (!is.sample.dp(sampl)) 
        stop("\n*** <sampl> is not an object of type 'sample.dp'.\n")
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
 parents<-list(wisp.id(sampl,newname=as.character(substitute(sampl))))
    samp <- sampl
    if (is.sample.dp(sampl)) 
        samp <- obscure.sample.dp(sampl)
    K <- samp$design$n.transects
    nobs <- hist(samp$transect, breaks = seq(0, K, length = (K + 
        1)) + 0.5, plot = F)$counts
    if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
        b.nL <- rep(0, nboot)
        b.Nhat.grp <- rep(0, nboot)
        b.Nhat.ind <- rep(0, nboot)
        b.average.g0 <- matrix(rep(0, nboot * 3), nrow = nboot, 
            ncol = 3, dimnames = list(replicate = 1:nboot, c("observer 1", "observer 2", 
                "combined  ")))
        b.mu <- rep(0, nboot)
        b.Es <- rep(0, nboot)
        b.prob.det <- rep(0, nboot)
        b.log.likelihood <- rep(0, nboot)
        b.AIC <- rep(0, nboot)
        b.samp <- samp
    }
    if (ci.type == "boot.nonpar") {
        for (i in 1:nboot) {
            index <- sample(1:K, K, replace = T)
            reps <- hist(index, breaks = (seq(0, K, length = K + 1) + 0.5), plot = F)$counts
            n <- sum(reps * nobs)
            b.samp$distance <- rep(NA, n)
            b.samp$population$groupsize <- rep(NA, n)
            b.samp$population$groupID <- rep(NA, n)
            b.samp$transect <- rep(NA, n)
            b.samp$detected.observer1 <- rep(NA, n)
            b.samp$detected.observer2 <- rep(NA, n)
            b.samp$detected <- rep(NA, n)
            b.samp$population$groupsize <- rep(NA, n)
            b.samp$population$posx <- rep(NA, n)
            b.samp$population$posy <- rep(NA, n)
            b.samp$population$types <- rep(NA, n)
            b.samp$population$exposure <- rep(NA, n)
            last <- 0
            b.tno <- 1
            n.units.per.transect <- samp$design$n.units/samp$design$n.transects
            for (k in 1:K) {
print(k)
                transind <- c(((k - 1) * n.units.per.transect +1):(k * n.units.per.transect))
                if (reps[k] > 0) {
                   keep <- (samp$transect == k)
                  first <- last + 1
                  nsit <- length(keep[keep == T])
                  last <- first - 1 + nsit * reps[k]
                  tn <- b.tno
                  for (m in 1:reps[k]) {
                    bt.first <- (tn - 1) * n.units.per.transect + 1
                    bt.last <- tn * n.units.per.transect
                    b.samp$design$pos.x[bt.first:bt.last] <- samp$design$pos.x[transind]
                    b.samp$design$start.y[bt.first:bt.last] <- samp$design$start.y[transind]
                    b.samp$design$end.y[bt.first:bt.last] <- samp$design$end.y[transind]
                    tn <- tn + 1
                  }
                  if (nsit > 0) {
                    b.samp$distance[first:last] <- rep(samp$distance[keep], reps[k])
                    b.samp$population$groupsize[first:last] <- rep(samp$population$groupsize[keep], reps[k])
                    b.samp$transect[first:last] <- rep(b.tno:(b.tno + reps[k] - 1), rep(nobs[k], reps[k]))
                    b.samp$detected.observer1[first:last] <- rep(samp$detected.observer1[keep], reps[k])
                    b.samp$detected.observer2[first:last] <- rep(samp$detected.observer2[keep], reps[k])
                    b.samp$detected[first:last] <- rep(samp$detected[keep], reps[k])
                    b.samp$population$groupID[first:last] <- rep(samp$population$groupID[keep], reps[k])
                    b.samp$population$posx[first:last] <- rep(samp$population$posx[keep], reps[k])
                    b.samp$population$posy[first:last] <- rep(samp$population$posy[keep], reps[k])
                    b.samp$population$types[first:last] <- rep(samp$population$types[keep], reps[k])
                    b.samp$population$exposure[first:last] <- rep(samp$population$exposure[keep], reps[k])
                  }
                  first <- last
                }
            }
            est <- point.est.dp(b.samp, model = model, )
            if (show.all) plot(est)
            b.Nhat.grp[i] <- est$Nhat.grp
            b.Nhat.ind[i] <- est$Nhat.ind
            b.Es[i] <- est$Es
            b.prob.det[i] <- est$phat
            b.mu[i] <- est$mu
            b.nL[i] <- est$nL
            b.average.g0[i, ] <- est$average.g0
            b.log.likelihood[i] <- est$log.likelihood
            b.AIC[i] <- est$AIC
        }
    }
    if (ci.type == "boot.par" | ci.type == "boot.nonpar") {
        boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, 
            Es = b.Es, phat = b.prob.det, mu = b.mu, nL = b.nL, 
            average.g0 = b.average.g0)
        valid<-(b.Nhat.grp != Inf)
        boot.mean <- list(Nhat.grp = mean(b.Nhat.grp[valid]), Nhat.ind = mean(b.Nhat.ind[valid]), 
            Es = mean(b.Es[valid]), phat = mean(b.prob.det), mu = mean(b.mu), 
            nL = mean(b.nL[valid]), average.g0 = apply(as.matrix(b.average.g0[valid,]), 2, mean))
        civec <- rep(NA, length(vlevels))
        se <- list(Nhat.grp = sqrt(var(b.Nhat.grp[valid])), Nhat.ind = sqrt(var(b.Nhat.ind[valid])), 
            Es = sqrt(var(b.Es[valid])), phat = sqrt(var(b.prob.det[valid])), mu=sqrt(var(b.mu[valid])), 
            nL = sqrt(var(b.nL[valid])), average.g0=sqrt(apply(as.matrix(b.average.g0[valid,]), 2, var)))
        cv<-list(Nhat.grp=se$Nhat.grp/boot.mean$Nhat.grp , Nhat.ind=se$Nhat.ind/boot.mean$Nhat.ind, 
            Es=se$Es/boot.mean$Es, phat = se$phat/boot.mean$phat, mu=se$mu/boot.mean$mu, 
            nL=se$nL/boot.mean$nL, se$average.g0/boot.mean$average.g0)
        n.average.g0 <- length(b.average.g0[1, ])
        ci <- list(Nhat.grp = civec, Nhat.ind = civec, Es = civec, 
            phat = civec, mu = civec, nL = civec, average.g0 = matrix(rep(civec, 
                n.average.g0), nrow = n.average.g0, ncol = length(vlevels), 
                dimnames = list(dimnames(b.average.g0)[[2]], 
                  rep("", 2))))
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
    intest <- list(levels=vlevels, ci=ci, boot.mean=boot.mean, boot.dbn=boot.dbn, se=se, cv=cv, ci.type=ci.type, 
                 model=model, parents=parents, created=date(), seed=seed)
    class(intest) <- "int.est.dp"
    if(plot) plot(intest)
    return(intest)
}



is.int.est.dp<-function (est) 
{
    inherits(est, "int.est.dp")
}
    

summary.int.est.dp<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "mu", "nL", "phat", "average.g0"), digits=5)
{
 if(!is.int.est.dp(iest)) 
   stop("Argument <iest>. must be of class int.est.dp\n")
 addtext1<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 addtext2<-paste("Detection model is logistic       : ",iest$model,"\n",sep="")
 addtext<-paste(addtext1,addtext2,sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.dp<-function(iest, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}




point.sim.dp<-function (pop.spec, survey.spec, design.spec, B = 99, model = "~distance", seed=NULL, show=FALSE, plot=FALSE) 
{
    if (!is.pars.survey.dp(survey.spec)) {
        stop("\nsurvey.spec must be of class 'pars.survey.dp'.\n")
    }
    if (!is.design.dp(design.spec) & !is.pars.design.dp(design.spec)) {
        stop("pop.spec must be of class 'design.dp' or 'pars.design.dp'.\n")
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
        if (is.population(pop.spec)) {
            mypop <- pop.spec
        }
        if (is.pars.population(pop.spec)) {
            mypop <- generate.population(pop.spec)
            random.pop<-TRUE
        }
        if (is.design.dp(design.spec)) {
            mydes <- design.spec
        }
        if (is.pars.design.dp(design.spec)) {
            mydes <- generate.design.dp(design.spec)
           random.design<-TRUE
        }
        survey.spec$population <- mypop
        survey.spec$design <- mydes
        mysamp <- generate.sample.dp(survey.spec)
        out.est <- point.est.dp(mysamp, model = model)
        res[i, stats] <- out.est[stats]
        if(show) plot(out.est)
    }
 true.N.grp <- length(mypop$groupsize)
 sim<-list(est=res, true=true, model=model, random.pop=random.pop, random.design=random.design, 
           parents=parents, created=date(), seed=seed)
 class(sim) <- "point.sim.dp"
 if(plot) plot(sim, est="Nhat.grp")
 return(sim)
}


is.point.sim.dp<-function(sim)
{
 inherits(sim, "point.sim.dp")
}

#plot.point.sim.dp<-function(sim, est=c("Nhat.grp","Nhat.ind","Es", "mu", "nL"), breaks="Sturges", type="both", ...)
plot.point.sim.dp<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
 if(length(est)>1) breaks="Sturges"
 for(i in 1:length(est)) {
   if(i>1) windows(height=5, width=4)
   sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
 }
}


summary.point.sim.dp<-function(sim, est=c("Nhat.grp","Nhat.ind","Es", "mu", "nL"), digits=5)
{
 if(!is.point.sim.dp(sim)) 
   stop("Argument <sim>. must be of class point.sim.dp\n")
 addtext<-paste("Model = ",sim$model, sep="")
 summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}





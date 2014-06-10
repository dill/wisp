############################################################
#
# POINT TRANSECT method functions
#
############################################################




generate.design.pt=function (pars.design.pt, seed = NULL) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
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
    parents <- list(wisp.id(pars.design.pt, newname = as.character(substitute(pars.design.pt))))
    pars <- pars.design.pt
    reg <- pars$region
    n.transects <- pars$n.transects
    n.units <- pars$n.units
    range <- pars$visual.range
    units.per.path <- n.units/n.transects
    length <- reg$length
    width <- reg$width
    dx <- length/n.transects
    paths.x <- (0:(n.transects - 1)) * dx + runif(1, 0, dx)
    units.x <- rep(paths.x, rep(units.per.path, n.transects))
    dy <- width/units.per.path
    displacement <- (0:(units.per.path - 1)) * dy + runif(1, 
        0, dy)
    units.y <- matrix(0, nrow = n.transects, ncol = units.per.path)
    for (i in 1:n.transects) {
        if ((i/2) != as.integer(i/2)) 
            units.y[i, ] <- displacement
        if ((i/2) == as.integer(i/2)) 
            units.y[i, ] <- width - displacement
    }
    units.y <- as.vector(t(units.y))
    des <- list(region = reg, n.transects = n.transects, n.units = n.units, 
        units.x = units.x, units.y = units.y, visual.range = range,parents = parents, created = date(), seed = seed)
    class(des) <- "design.pt"
    return(des)
}


#===========================================================
# is.design.pt
#===========================================================

is.design.pt <- function (des) 

   #----------------------------------------------------------------
   # description:
   #    tests if the given object <des> if of type "design.pt"
   #
   # author: M. Erdelmeier
   #----------------------------------------------------------------

{
    # test if <des> is of the type "design.pt"
    inherits(des, "design.pt")
}


#===========================================================
# plot.design.pt
#===========================================================

plot.design.pt <- function (des, newplot = T,...)
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
   if (!is.design.pt(des))
      stop("\n*** <des> must be of type 'design.pt'\n")
   if ((newplot!=T) & (newplot!=F))
      stop("\n*** <newplot> must be TRUE or FALSE.\n")
   # get design data
   reg <- des$region
   pos.x <- des$units.x
   pos.y <- des$units.y
   range <- rep(des$visual.range, length(pos.x))
   #get region data
   len <- reg$length
   wid <- reg$width
   # save current graphic parameters
   par.was <- par(no.readonly = T)
   # calculate dimensions of plot borders (ratio corresponding to
   # length and width of region, but appropriate to plot area)
   margin <- calculate.plot.margin (reg.x=len, reg.y=wid,
             area.x=par.was$fin[1], area.y=par.was$fin[2])
   # set plot margins
   par (mai=c(margin$y, margin$x, margin$y, margin$x))
   # show new plot?
   par(new = !newplot)
   # plot region 
   plot(0, 0, type = "n", las = 1, xlab = "", ylab = "",
      xlim = c(0, len), ylim = c(0, wid), xaxs = "i", yaxs = "i", xpd = T,...)
   # plot survey areas 
   symbols(pos.x, pos.y, circles=range, xlab = "", ylab = "", inches=FALSE, bg = "aquamarine", fg = "black")   #  , border = par("fg"))
###		Plot centre of each point transect (suggested by LJT June 2007)   
   points(pos.x, pos.y, pch=3, cex=0.5)
   # restore changed graphical parameters
   par(new=par.was$new)
}


#===========================================================
# setpars.design.pt
#===========================================================

setpars.design.pt=function (reg, n.transects = 1, n.units = 1, visual.range) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.region(reg)) 
        stop("\n*** The parameter <reg> is not of type 'region'.\n")
    if (!is.numeric(n.transects)) 
        stop("\n*** The number of paths must be numeric.\n")
    if (n.transects != as.integer(n.transects)) 
        stop("\n*** The number of paths must be of type integer.\n")
    if (n.transects < 1) 
        stop("\n*** The number of paths must be at least 1.\n")
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
    if (2 * visual.range * n.transects >= reg$length) 
        stop(paste("\n*** The visual range is bigger than half", 
            "of the path distance.\n"))
    if (2 * ((n.units/n.transects) - 1) * visual.range >= reg$width) 
        stop(paste("\n*** The visual range of neighboured survey", 
            "units is superposed.\n"))
    parents <- list(wisp.id(reg, newname = as.character(substitute(reg))))
    pars.design.pt <- list(region = reg, n.transects = n.transects, 
        n.units = n.units, visual.range = visual.range,parents = parents, created = date())
    class(pars.design.pt) <- "pars.design.pt"
    return(pars.design.pt)
}

is.pars.design.pt<-function (despars) 
{
   inherits(despars,"pars.design.pt")
}

plot.pars.design.pt=function (x, col = "black") 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    plot.text("There is no useful plot for this class of object", 
        col = col)
}


summary.pars.design.pt=function (des, digits = 5) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.pars.design.pt(des)) 
        stop("\nThe parameter <des> must be of class 'pars.design.pt'.\n")
    cat("\n")
    cat("POINT TRANSECT DESIGN PARAMETER SUMMARY\n")
    cat("---------------------------------------\n")
    cat("creation date   :", des$created, "\n")
    cat("parent object(s) (class, name, creation date):\n")
    for (i in 1:length(des$parents)) {
        cat("      ", paste("(", des$parents[[i]]$class, ", ", 
            des$parents[[i]]$name, ", ", des$parents[[i]]$created, 
            ")", sep = ""), "\n")
    }
    if (is.numeric(des$seed)) 
        cat("random number seed used: ", des$seed, "\n")
    cat("\n")
    w <- des$visual.range
    K <- des$n.transects
    L <- des$n.units
    a <- pi * w^2 * L
    A <- des$reg$length * des$reg$width
    cat("Number of transects              :", K, "\n")
    cat("Number of points                 :", L, "\n")
    cat("Point radius                     :", w, "\n")
    cat("Covered area                     :", a, "\n")
    cat("Coverage probability             :", signif(a/A, digits), "\n")
    cat("\n")
    cat("Region dimensions (length x width):", des$reg$length, "x", des$reg$width, "\n")
}


plot.pars.survey.pt=function (pars) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    pars.old <- par(no.readonly = TRUE)
    cex <- 0.9 * par()$din[2]/5
    par(cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex, xpd = TRUE)
    w <- pars$design$visual.range
    x <- seq(0, w, length = 100)
    minexp <- rep(pars$population$minexposure, 100)
    maxexp <- rep(pars$population$maxexposure, 100)
    fmax <- detection.transectmethods(x, maxexp, theta = pars$theta)
    plot(x, fmax, type = "l", ylim = c(0, 1), xlab = "Distance", 
        ylab = "Detection probability", lwd = 3 * cex)
    if (pars$population$minexposure != pars$population$maxexposure) {
        fmin <- detection.transectmethods(x, minexp, theta = pars$theta)
        lines(x, fmin, lty = 2, lwd = 3 * cex)
        title("Detection functions for min and max exposure")
    }
    else {
        title("Detection function")
    }
    par(pars.old)
}


summary.design.pt=function (des, digits = 5) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.design.pt(des)) 
        stop("\nThe parameter <des> must be of class 'design.pt'.\n")
    cat("\n")
    cat("POINT TRANSECT DESIGN SUMMARY\n")
    cat("------------------------------\n")
    cat("creation date   :", des$created, "\n")
    cat("parent object(s) (class, name, creation date):\n")
    for (i in 1:length(des$parents)) {
        cat("      ", paste("(", des$parents[[i]]$class, ", ", 
            des$parents[[i]]$name, ", ", des$parents[[i]]$created, 
            ")", sep = ""), "\n")
    }
    if (is.numeric(des$seed)) cat("random number seed used: ", des$seed, "\n")
    cat("\n")
    K <- des$n.transects
    L <- des$n.units
    w <- des$visual.range
    a <- pi * w^2 * L
    A <- des$reg$length * des$reg$width
    cat("Number of transects              :", K, "\n")
    cat("Number of points                 :", L, "\n")
    cat("Point radius                     :", w, "\n")
    cat("Covered area                     :", a, "\n")
    cat("Coverage probability             :", signif(a/A, digits), "\n")
    cat("\n")
    cat("Region dimensions (length x width):", des$reg$length, "x", des$reg$width, "\n")
}



generate.sample.pt=function (pars, seed = NULL) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
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
    if (is.numeric(seed)) set.seed(seed)
    parents <- list(wisp.id(pars, newname = as.character(substitute(pars))))
    pop <- pars$population
    des <- pars$design
    theta <- pars$theta
    pos.x <- pop$posx
    pos.y <- pop$posy
    exposure <- pop$exposure
    n.groups <- length(pos.x)
    unit.x <- des$units.x
    unit.y <- des$units.y
    range <- des$visual.range
    n.units <- des$n.units
    n.transects <- des$n.transects
    detectable <- rep(FALSE, n.groups)
    distance <- rep(NA, n.groups)
    transect <- rep(NA, n.groups)
    for (i in 1:n.units) {
        w <- sqrt((unit.x[i] - pos.x)^2 + (unit.y[i] - pos.y)^2)
        inside <- (w <= range)
        detectable[inside] <- TRUE
        distance[inside] <- w[inside]
        transect[inside] <- ceiling(i/(n.units/n.transects))
    }
    p.detect <- rep(0, n.groups)
    p.detect[detectable] <- detection.transectmethods(distance[detectable], 
        exposure[detectable], theta)
    detected <- rbinom(n.groups, 1, p.detect)
    detected[!detectable] <- NA
    samp <- list(population = pop, design = des, detected = detected, 
        distance = distance, transect = transect, parents = parents, 
        created = date(), seed = seed)
    class(samp) <- "sample.pt"
    return(samp)
}




#===========================================================
# is.sample.pt
#===========================================================

is.sample.pt <- function (samp) 

   #----------------------------------------------------------------
   # description:
   #    tests if the given object <samp> if of type "sample.pt"
   #
   # author: M. Erdelmeier
   #----------------------------------------------------------------

{
    # test if <samp> is of the type "sample.pt"
    inherits(samp, "sample.pt")
}


#===========================================================
# plot.sample.pt
#===========================================================

plot.sample.pt = function (samp, show.sizes = TRUE, show.exps = TRUE, dsf = 1,
    whole.population = FALSE, ...) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.sample.pt(samp)) 
        stop("\n*** The parameter <samp> must be of type 'sample.pt'.\n")
    if (!is.numeric(dsf)) 
        stop("\n*** The parameter <dsf> must be numeric.\n")
    if (dsf <= 0) 
        stop("\n*** The parameter <dsf> must be positive.\n")
    if ((show.sizes != TRUE) & (show.sizes != FALSE)) 
        stop("\n*** The parameter <show.sizes> must be TRUE or FALSE.\n")
    if ((show.exps != TRUE) & (show.exps != FALSE)) 
        stop("\n*** The parameter <show.exps> must be TRUE or FALSE.\n")
    if ((whole.population != TRUE) & (whole.population != FALSE)) 
        stop("\n** The parameter <whole.population> must be TRUE or FALSE.\n")
    pop <- samp$population
    des <- samp$design
    par.was <- par(no.readonly = TRUE)
    len <- pop$reg$length
    width <- pop$reg$width
    margin <- calculate.plot.margin(reg.x = len, reg.y = width, 
        area.x = par.was$fin[1], area.y = par.was$fin[2])
    par(mai = c(margin$y, margin$x, margin$y, margin$x))
    plot(0, 0, type = "n", las = 1, xlab = "", ylab = "", xlim = c(0, 
        len), ylim = c(0, width), xaxs = "i", yaxs = "i", xpd = TRUE, 
        ...)
   plot(des, newplot = FALSE, ...)
    if (whole.population == TRUE) 
        plot.groups(pop, show.sizes = show.sizes, show.exps = show.exps, 
             dsf = dsf, ...)
    inside <- ((samp$detected == 1) & !is.na(samp$detected))
    seen <- pop
    seen$groupID <- pop$groupID[inside]
    seen$posx <- pop$posx[inside]
    seen$posy <- pop$posy[inside]
    seen$groupsize <- pop$groupsize[inside]
    seen$types <- pop$types[inside]
    seen$exposure <- pop$exposure[inside]
    plot.groups(seen, show.sizes = show.sizes, show.exps = show.exps, dsf = dsf, group.col = "red", ...)
}


#===========================================================
# setpars.survey.pt
#===========================================================

setpars.survey.pt=function (pop, des, disthalf.min, disthalf.max, model = "half.normal") 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (model != "half.normal") 
        stop("Argument <> must be 'half.normal' (other methods not yet implemented)")
    if (!is.population(pop)) 
        stop("\n*** The parameter <pop> must be of type 'population'.\n")
    if (!is.design.pt(des)) 
        stop("\n*** The parameter <des> must be of type 'design.pt'.\n")
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
    if (model == "half.normal") {
        if (min.exposure == max.exposure) 
            theta1 <- 0
        if (min.exposure != max.exposure) 
            theta1 <- (log(disthalf.min) - log(disthalf.max))/(min.exposure - 
                max.exposure)
        theta0 <- log(disthalf.min/sqrt(-2 * log(0.5))) - theta1 * 
            min.exposure
        if (is.na(theta0) | is.na(theta1)) 
            stop("\nParameters lead to invalid data.\n")
        if ((theta0 == Inf) | (theta1 == Inf)) 
            stop("\nParameters lead to invalid data.\n")
        theta <- c(theta0, theta1)
    }
    parents <- list(wisp.id(pop, newname = as.character(substitute(pop))), 
        wisp.id(des, newname = as.character(substitute(des))))
    pars.survey.pt <- list(population = pop, design = des, theta = theta, 
        model = model, parents = parents, created = date())
    class(pars.survey.pt) <- "pars.survey.pt"
    return(pars.survey.pt)
}



obscure.sample.pt<-function(samp)
#----------------------------------------------------------------
# Removes all information about undetected animals from an object
# of class sample.pt. Returns object of same class.
#----------------------------------------------------------------
{
if (!is.sample.pt(samp)) 
  stop("\n*** <samp> is not an object of type 'sample.pt'.\n")
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
t
}




is.pars.survey.pt<-function (pars) 
{
    inherits(pars, "pars.survey.pt")
}


summary.pars.survey.pt=function (pars) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.pars.survey.pt(pars)) 
        stop("\nThe parameter <pars> must be of type 'pars.survey.pt'.\n")
    cat("\n")
    cat("SURVEY PARS SUMMARY (POINT TRANSECT METHOD)\n")
    cat("-------------------------------------------\n\n")
    r <- pars$design$visual.range
    K <- pars$design$n.transects
    L <- pars$design$n.units
    a <- pi * r^2 * L
    A <- pars$population$region$width * pars$population$region$length
    cat("Truncation distance:              ", r, "\n")
    cat("Number of transects:              ", K, "\n")
    cat("Number of points:                 ", L, "\n")
    cat("Survey area:                      ", A, "\n")
    cat("Covered area:                     ", a, "\n")
    cat("Detection function: \n")
    cat("------------------ \n")
    cat("  Half-normal model: \n")
    cat("    p(detect) = exp(-0.5 * distance^2/(exp(theta0 + theta1 * exposure))^2)\n")
    cat("  Parameters: \n")
    cat("    theta0 = ", pars$theta[1], "; theta1 = ", pars$theta[2],"\n")
    old.par <- par(no.readonly = TRUE)
    minexp <- rep(pars$population$minexposure, 100)
    maxexp <- rep(pars$population$maxexposure, 100)
    x <- seq(0, r, length = 100)
    fmax <- detection.transectmethods(x, maxexp, pars$theta)
    plot(x, fmax, type = "l", ylim = c(0, 1), xlab = "Distance",ylab = "Detection probability")
    if (pars$population$minexposure != pars$population$maxexposure) {
        fmin <- detection.transectmethods(x, minexp, pars$theta)
        lines(x, fmin, lty = 2)
        title("Detection functions for min and max exposure")
    }
    else {
        title("Detection function")
    }
    par(old.par)
}



summary.sample.pt<-function (samp) 
{
    if (!is.sample.pt(samp)) 
        stop("\nThe parameter <samp> must be of type 'sample.pt'.\n")
    cat("\n")
    cat("SAMPLE SUMMARY (POINT TRANSECT METHOD)\n")
    cat("---------------------------------------\n\n")
    r <- samp$design$visual.range
    K <- samp$design$n.transects
    L <- samp$design$n.units
    a <- pi * r^2 * L
    A <- samp$population$region$width * samp$population$region$length
    xi <- samp$distance[(samp$detected == 1) & !is.na(samp$detected)]
    zi <- samp$population$groupsize[(samp$detected == 1) & !is.na(samp$detected)]
    cat("Truncation distance:              ", r, "\n")
    cat("Number of transects:              ", K, "\n")
    cat("Number of points:                 ", L, "\n")
    cat("Survey area:                      ", A, "\n")
    cat("Covered area:                     ", a, "\n")
    cat("Number of groups detected:        ", length(xi), "\n")
    cat("Mean group size:                  ", mean(zi), "\n")
    cat("Mean radial distance:             ", mean(xi), "\n")
    hist(xi, xlab = "Radial Distance", ylab = "Frequency", 
        main = "Radial distance distribution")
}



point.est.pt=function (sampl, plot = FALSE, title=TRUE, conditional = TRUE, model = "half.normal") 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (is.sample.pt(sampl)) {
        n <- sum(sampl$detected[!is.na(sampl$detected)])
        if (!(n > 0)) 
            stop("Sample size of zero!")
        width <- sampl$population$region$width
        length <- sampl$population$region$length
        A <- width * length
        xi <- sampl$distance[sampl$detected > 0 & !is.na(sampl$detected)]
        w <- min(sampl$design$visual.range, (1.01 * max(xi)))
        u <- sampl$design$n.units
        a <- u * pi * w^2
        pc <- a/A
        zi <- sampl$population$groupsize[sampl$detected > 0 & 
            !is.na(sampl$detected)]
        lower <- 0
        upper <- w
    }
    else stop("sampl must be of class sample.pt")
    parents <- list(wisp.id(sampl, newname = as.character(substitute(sampl))))
    if (model != "half.normal") 
        stop("Invalid detection function model. The following are valid: \n 'half.normal'")
    if (model == "half.normal") {
        full.llk <- function(x) {
            N <- exp(x[1]) + n
            sigma2 <- exp(x[2])
            v <- 2 * pi * sigma2 * (1 - exp(-w^2/(2 * sigma2)))
            log.bincoeff <- -lgamma(N + 1) + lgamma(n + 1) + 
                lgamma(N - n + 1)
            llk <- log.bincoeff - (N - n) * log(1 - u * v/A) - 
                n * log(pc * 2/w^2) + sum(xi^2)/(2 * sigma2) - 
                sum(log(xi))
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
            v.over.2pi <- sigma2 * (1 - exp(-w^2/(2 * sigma2)))
            llk <- n * log(v.over.2pi) + sum(xi^2)/(2 * sigma2) - 
                sum(log(xi))
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
        startest <- sum(xi^2)/(2 * length(xi))
        startx <- transform.thetatox(startest)
        res <- nlm(cond.llk, abs(startx), typsize = abs(startx))
    }
    if (conditional) {
        xhat <- res$estimate
        log.likelihood <- -res$minimum
        AIC <- -2 * log.likelihood + 2 * length(xhat)
        theta <- transform.xtotheta(xhat)
        vhat <- 2 * pi * theta * (1 - exp(-w^2/(2 * theta)))
        EPhat <- vhat * u/A
        Nhat <- n/EPhat
    }
    else {
        xhat <- res$estimate
        theta <- transform.xtotheta(xhat)
        startsigma <- theta
        vhat <- 2 * pi * theta * (1 - exp(-w^2/(2 * theta)))
        startN <- n * A/(vhat * u)
        startNtheta <- c(startN, startsigma)
        startx <- transform.Nthetatox(startNtheta)
        if (model == "half.normal") {
            res <- nlm(full.llk, abs(startx), typsize = abs(startx))
        }
        xhat <- res$estimate
        log.likelihood <- -res$minimum
        AIC <- -2 * log.likelihood + 2 * length(xhat)
        est <- transform.xtoNtheta(xhat)
        Nhat <- est[1]
        theta <- est[2]
        vhat <- 2 * pi * theta * (1 - exp(-w^2/(2 * theta)))
    }
#    if (plot) {
#        plotpthn(xi, theta, w, title = T, fit = TRUE)
#    }
    if (model == "half.normal") {
        theta <- matrix(theta, nrow = 1, ncol = 1, dimnames = list(c("sigma2"), 
            ""))
    }
    pointest <- list(sample = sampl, model = model, conditional = conditional,
        Nhat.grp = Nhat, Nhat.ind = Nhat * mean(zi), theta = theta, 
        esa = vhat, nbar = n/u, Es = mean(zi), log.likelihood = log.likelihood, 
        AIC = AIC, fit.summary = res, parents = parents, created = date())
    class(pointest) <- "point.est.pt"
    if (plot) 
        plot(pointest,title=title)
    pointest
}


int.est.pt=function (sampl, ci.type = "boot.nonpar", nboot = 999, vlevels = c(0.025, 
    0.975), conditional = TRUE, model = "half.normal", plot = FALSE, 
    show.all = FALSE, seed = NULL) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.sample.pt(sampl)) 
        stop("\n*** <sampl> is not an object of type 'sample.pt'.\n")
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
    if (model != "half.normal") 
        stop("Invalid detection function model. The following are valid: \n 'half.normal'")
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
    if (is.sample.pt(sampl)) 
        samp <- obscure.sample.pt(sampl)
    K <- samp$design$n.transects
    nobs <- hist(samp$transect, breaks = seq(0, K, length = (K + 
        1)) + 0.5, plot = F)$counts
    if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
        b.nbar <- rep(0, nboot)
        b.Nhat.grp <- rep(0, nboot)
        b.Nhat.ind <- rep(0, nboot)
        if (model == "half.normal") {
            b.theta <- matrix(rep(0, nboot), nrow = nboot, ncol = 1, 
                dimnames = list(replicate = 1:nboot, c("sigma2")))
        }
        else {
            b.theta <- matrix(rep(0, nboot * 2), nrow = nboot, 
                ncol = 2, dimnames = list(replicate = 1:nboot, 
                  c("scale.parameter", "shape.parameter")))
        }
        b.esa <- rep(0, nboot)
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
            est <- point.est.pt(b.samp, conditional = conditional, model = model)
            if (show.all) plot(est)
#            if (show.all) 
#                est <- point.est.pt(b.samp, conditional = conditional, model = model)
#            else est <- point.est.pt(b.samp, conditional = conditional, model = model, plot = F)
            b.Nhat.grp[i] <- est$Nhat.grp
            b.Nhat.ind[i] <- est$Nhat.ind
            b.theta[i, ] <- est$theta
            b.esa[i] <- est$esa
            u <- b.samp$design$n.units
            b.nbar[i] <- length(b.samp$distance)/u
            b.Es[i] <- mean(b.samp$population$groupsize)
        }
        text2 <- paste("\nNon-parametric Bootstrap with", nboot, 
            "replicates")
    }
    if (ci.type == "boot.par" | ci.type == "boot.nonpar") {
        boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, 
            theta = b.theta, esa = b.esa, nbar = b.nbar, Es = b.Es)
        valid <- (b.Nhat.grp != Inf)
        boot.mean <- list(Nhat.grp = mean(b.Nhat.grp[valid]), 
            Nhat.ind = mean(b.Nhat.ind[valid]), theta = apply(as.matrix(b.theta[valid,]),2, mean), 
            esa = mean(b.esa[valid]), nbar = mean(b.nbar[valid]), 
            Es = mean(b.Es[valid]))
        se <- list(Nhat.grp = sqrt(var(b.Nhat.grp[valid])), Nhat.ind = sqrt(var(b.Nhat.ind[valid])), 
            theta = sqrt(apply(as.matrix(b.theta[valid, ]), 2, var)), 
            esa = sqrt(var(b.esa[valid])), nbar = sqrt(var(b.nbar[valid])), 
            Es = sqrt(var(b.Es[valid])))
        cv <- list(Nhat.grp = se$Nhat.grp/boot.mean$Nhat.grp, 
            Nhat.ind = se$Nhat.ind/boot.mean$Nhat.ind, theta = se$theta/boot.mean$theta, 
            esa = se$esa/boot.mean$esa, nbar = se$nbar/boot.mean$nbar, 
            Es = se$Es/boot.mean$Es)
        civec <- rep(NA, length(vlevels))
        n.theta <- length(b.theta[1, ])
        ci <- list(Nhat.grp = civec, Nhat.ind = civec, theta = matrix(rep(civec, 
            n.theta), nrow = n.theta, ncol = length(vlevels), 
            dimnames = list(dimnames(b.theta)[[2]], rep("", 2))), 
            esa = mean(b.esa), nbar = mean(b.nbar), Es = civec)
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
    class(intest) <- "int.est.pt"
    if (plot) plot(intest, type = "hist")
    return(intest)
}


is.int.est.pt<-function(est)
{
	inherits(est, "int.est.pt")
}

is.point.est.pt<-function(est)
{
	inherits(est, "point.est.pt")
}


#plot.point.est.pt=function (est, breaks = 10, title = TRUE) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
#{
#    old.par <- par(no.readonly = TRUE)
#    cex <- 0.9 * par()$din[2]/5
#    par(cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex, xpd = TRUE)
#    if (title) 
#        main <- "Radial distance distribution\nand fitted probability density function"
#    xi <- est$sample$distance[!is.na(est$sample$distance) & est$sample$detected == 1]
#    theta <- est$theta
#    model <- est$model
#    w <- est$sample$design$visual.range
#    mu <- integrate(detfn.lt, lower = 0, upper = w, theta = theta, model = model)$value
#    x <- seq(0, w, length = 100)
#    f <- detfn.lt(x, theta, model = model)/mu*x
#    if (length(breaks) == 1) 
#        breaks <- seq(0, w, length = breaks)
#    hst <- hist(xi, breaks = breaks, plot = FALSE)
#    ymax <- max(f, hst$density)
#    hist(xi, xlab = "Perpendicular Distance", ylab = "Probability density", 
#        freq=FALSE, main = main, ylim = c(0, ymax), breaks = breaks)
#    lines(x, f, col = "red")
#    par(old.par)
#}
#
plot.point.est.pt <- function (est, breaks = 10, which="both", title = TRUE) 
# This version will plot either the detection function or the probability density
#   function, or both (the default). 'which' can be (an abbreviation of) "detection"
#   or "probability"; any other value results in both being plotted.
# 'title' is retained for compatability, but is ignored.
#
# Suggested alternative by Mike Meredith, mmeredith@wcs.org, 1 June 2007
{
    to.plot <- pmatch(tolower(which), c("detection", "probability"),
      nomatch=0)  # if no match, do both
    old.par <- par(no.readonly = TRUE)
    cex <- 0.8 * par()$din[2]/5
    par(cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex, 
        mar=c(5,5,2,2)+0.1, xpd = TRUE)
#    if (title) 
#        main <- "Radial distance distribution\nand fitted probability density function"
    xi <- est$sample$distance[!is.na(est$sample$distance) & est$sample$detected == 
        1]
    theta <- est$theta
    model <- est$model
    w <- est$sample$design$visual.range
    mu <- integrate(detfn.lt, lower = 0, upper = w, theta = theta, 
        model = model)$value
    x <- seq(0, w, length = 100)
    g <- detfn.lt(x, theta, model = model)
    f <- g * x/mu
    if (length(breaks) == 1) 
        breaks <- seq(0, w, length = breaks)
    hst <- hist(xi, breaks = breaks, plot = FALSE)
    if(to.plot == 0)
       par(mfrow = c(2,1))
    if(to.plot==0 || to.plot==1) {   # Plot detection function:
       bars <- mu * hst$density / hst$mids
       ylim <- range(0, g, bars)
       ylab <- expression(paste("Detection function  ", hat(g), "(r)", sep=""))
       plot(hst$mids, bars, type='n', xlim=range(breaks), ylim=ylim, bty='n',
          xlab = "Perpendicular Distance", ylab = ylab)
       rect(breaks[-length(breaks)],0,breaks[-1],bars)
       lines(x, g, col = "red")
    }
    if(to.plot==0 || to.plot==2) {    # Plot prob density:
       ylim <- range(0, f, hst$density)
       ylab <- expression(paste("Probability density  ", hat(f), "(r)", sep=""))
       hist(xi, xlab = "Perpendicular Distance", ylab = ylab, 
           freq = FALSE, main = "", ylim = ylim, breaks = breaks)
       lines(x, f, col = "red")
    }
    par(old.par)
}

summary.point.est.pt=function (est, digits = 5) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.point.est.pt(est)) 
        stop("\nThe parameter <samp> must be of class 'point.est.pt'.\n")
    cat("\n")
    cat("POINT ESTIMATE SUMMARY (POINT TRANSECT METHOD)\n")
    cat("----------------------------------------------\n")
    cat("creation date   :", est$created, "\n")
    cat("parent object(s) (class, name, creation date):\n")
    for (i in 1:length(est$parents)) {
        cat("      ", paste("(", est$parents[[i]]$class, ", ", 
            est$parents[[i]]$name, ", ", est$parents[[i]]$created, 
            ")", sep = ""), "\n")
    }
    cat("\n")
    cat("Conditional likelihood? :", est$conditional, "\n")
    cat("\n")
    n <- sum(est$sample$detected[!is.na(est$sampl$detected)])
    w <- est$sample$design$visual.range
    K <- est$sample$design$n.transects
    L <- est$sample$design$n.units
    a <- pi * w^2 * L
    A <- est$sample$population$region$width * est$sample$population$region$length
    cat("Truncation distance               :", w, "\n")
    cat("Number of lines                   :", K, "\n")
    cat("Number of points                  :", L, "\n")
    cat("Survey area                       :", A, "\n")
    cat("Covered area                      :", a, "\n")
    cat("\n")
    cat("Number of groups detected         :", n, "\n")
    cat("Mean encounter rate per point     :", signif(est$nbar, 
        digits = digits), "\n")
    cat("\n")
    cat("Detection function Model          :", est$model, "\n")
    if (est$model == "half.normal") {
        cat("    p(detect) = exp(-0.5 * distance^2/(sigma^2)\n")
        cat("    Parameters: \n")
        cat("      sigma = ", signif(sqrt(est$theta[1]), digits = digits), 
            "\n")
    }
    if (est$model == "hazard.rate") {
        cat("    p(detect) = 1 - exp(-(distance/a)^(-b]))\n")
        cat("  Parameters: \n")
        cat("    a = ", signif(est$theta[1], digits = digits), 
            "b = ", signif(est$theta[2], digits = digits), "\n")
    }
    cat("Effective per-plot survey area    :", signif(est$esa, 
        digits = digits), "\n")
    cat("Mean detection probability        :", signif(est$esa/(pi*w^2), 
        digits = digits), "\n")
    cat("Effective percentage area covered :", signif((100 * 
        a/A) * est$esa/(pi*w^2), digits = digits), "%\n")
    cat("\n")
    cat("Group abundance                   :", round(est$Nhat.grp), 
        "\n")
    cat("Individual abundance              :", round(est$Nhat.ind), 
        "\n")
    cat("Mean group size                   :", signif(est$Es, 
        digits = digits), "\n")
    cat("\n")
    cat("Log-likelihood                    :", est$log.likelihood, 
        "\n")
    cat("AIC                               :", est$AIC, "\n")
}


plot.int.est.pt=function (iest, est = "Nhat.grp", breaks = "Sturges", type = "both", ...) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    plot.int.est(iest, est = est, breaks = breaks, type = type, ...)
}


summary.int.est.pt=function (iest, est=c("Nhat.grp","Nhat.ind","Es","esa","nbar","theta"),digits=5) 
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.int.est.pt(iest)) 
        stop("Argument <iest>. must be of class int.est.pt\n")
    addtext1 <- paste("Interval estimation method        : ",iest$ci.type, "\n", sep = "")
    addtext2 <- paste("Detection function model          : ",iest$model, "\n", sep = "")
    addtext3 <- paste("Conditional likelihood?           : ",iest$conditional, sep = "")
    addtext <- paste(addtext1, addtext2, addtext3, sep = "")
    summary.int.est(iest, est = est, add.text = addtext, digits = digits)
}



point.sim.pt<-function (pop.spec, survey.spec, design.spec, B=99, 
    plot=FALSE, title=FALSE, conditional=TRUE, model="half.normal", seed=NULL, show=FALSE) 
#------------------------------------------------------------------------------------------------------------
#  Function: 'point.sim.pt'
#  Usage:     Simulates drawing samples from populations and using these to estimate abundance using the
#             point transect method 
#  Arguments:  pop.spec - object either of class 'population' or 'population.pars'.  If of class                  
#                         'pars.population' a population will be randomly generated using these parameters for 
#                          each replicate.  If of class 'population' the population will not be randomised.
#           survey.spec - object either of class 'sample.pt' or 'pars.survey.pt'.  If of class 
#                         'pars.survey.pt' a sample object will be randomly generated using these parameters for
#                         each replicate.  If of class 'sample.pt' the sample object will not be randomised. 
#           design.spec - object either of class 'design.pt' or 'pars.design.pt'.  If of class 'pars.design.pt' the
#                         survey will be randomly generated using these parameters for each replicate.  If of
#                         class 'design.pt' the survey design will not be randomised in this function
#                     B - Number of replicates required in the simulation
#              plot = F - argument used by point.est.pt
#       conditional = F - argument used by point.est.pt
# model = "half.normal" - argument used by point.est.pt
#                   ... - optional arguments to be used for producing the histogram of abundance estimates
#------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
    if (!is.pars.survey.pt(survey.spec)) {
        stop("\nsurvey.spec must be of class 'pars.survey.pt'.\n")
    }
    if (!is.design.pt(design.spec) & !is.pars.design.pt(design.spec)) {
        stop("design.spec must be of class 'design.pt' or 'pars.design.pt'\n")
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
 stats<-c("Nhat.grp","Nhat.ind","Es", "esa", "nbar") 
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
   if (is.design.pt(design.spec)) mydes <- design.spec
   if (is.pars.design.pt(design.spec)) {
     mydes <- generate.design.pt(design.spec)
     random.design<-TRUE
   }
   if (is.pars.design.dp(design.spec)) {
     mydes <- generate.design.dp(design.spec)
     random.design<-TRUE
   }
   survey.spec$population <- mypop
   survey.spec$design <- mydes
   if (is.pars.survey.pt(survey.spec)) mysamp <- generate.sample.pt(survey.spec)
   out.est <- point.est.pt(mysamp, plot=FALSE, title=title, conditional=conditional, model=model)
   res[i, stats] <- out.est[stats]
   if(show) plot(out.est)
 }
 true.N.grp <- length(mypop$groupsize)
 sim<-list(est=res, true=true, conditional=conditional, model=model, 
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
 class(sim) <- "point.sim.pt"
 if(plot) plot(sim, est="Nhat.grp")
 return(sim)
}



is.point.sim.pt<-function(sim)
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
 inherits(sim, "point.sim.pt")
}


plot.point.sim.pt<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
 if(length(est)>1) breaks="Sturges"
 for(i in 1:length(est)) {
   if(i>1) windows(height=5, width=4)
   sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
 }
}


summary.point.sim.pt<-function(sim, est=c("Nhat.grp","Nhat.ind","Es", "esa", "nbar"), digits=5)
#-------------------------------------------------------------------------------------
# Updated 4/1/07 dlb
#-------------------------------------------------------------------------------------
{
 if(!is.point.sim.pt(sim)) 
   stop("Argument <sim>. must be of class point.sim.pt\n")
 addtext<-paste("(Model = ",sim$model,";  Conditional = ",sim$conditional, ";  Model selection = ",sim$model.sel,")",sep="")
 summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}



setpars.population<-function (density.pop, number.groups = 100, size.method = "user", 
    size.values = 1, size.prob = 1, size.min = 1, size.max = 1, 
    size.mean = 1, exposure.method = "user", exposure.min = 1, 
    exposure.max = 1, exposure.values = 1, exposure.prob = 1, 
    exposure.mean = 0.5 * (exposure.min + exposure.max), exposure.shape = 1, 
    type.values = NA, type.prob = 1, adjust.interactive = FALSE) 
# deleted reg from argument list
{
    if (!is.density.population(density.pop)) 
        stop(paste("\n*** The parameter <density.pop> must be of type", 
            "'density.population'.\n"))
    if (!is.numeric(number.groups)) 
        stop("\n*** The number of groups must be numeric.\n")
    if (number.groups != as.integer(number.groups)) 
        stop("\n*** The number of groups must be an integer value.\n")
    if (number.groups < 1) 
        stop("\n*** There must be at least 1 animal group inside the region.\n")
    if ((size.method != "user") & (size.method != "poisson")) 
        stop(paste("\n*** The parameter <size.method> must be either", 
            "'poisson' or 'user'.\n"))
    if (size.method == "poisson") {
        size.values <- NA
        size.prob <- NA
        if (!is.numeric(size.min) | !is.numeric(size.max)) 
            stop(paste("\n*** The parameter <size.min> and <size.max> must", 
                "be numeric.\n"))
        if ((size.min != as.integer(size.min)) | (size.max != 
            as.integer(size.max))) 
            stop(paste("\n*** The parameter <size.min> and <size.max> must", 
                "be integer values.\n"))
        if (size.min < 1) 
            stop("\n*** The parameter <size.min> must be at least 1.\n")
        if (size.min > size.max) 
            stop(paste("\n*** The parameter <size.min> cannot be greater", 
                "than <size.max>.\n"))
        if (!is.numeric(size.mean)) 
            stop("\n*** The parameter <size.mean> must be numeric.\n")
        if (size.mean <= 0) 
            stop("\n*** The parameter <size.mean> must be greater than 0.\n")
    }
    if (size.method == "user") {
        size.min <- 1
        size.max <- 1
        size.mean <- 1
        if (length(size.values) != length(size.prob)) 
            stop(paste("\n*** There given number of size values does not", 
                "correspond to the given number of probabilities.\n"))
        if (!is.numeric(size.values)) 
            stop("\n*** The size values must be numeric.\n")
        if (any(size.values != as.integer(size.values))) 
            stop("\n*** The sizes must be of type integer.\n")
        if (any(size.values < 1)) 
            stop("\n*** All sizes must be at least one.\n")
        if (!is.numeric(size.prob)) 
            stop("\n*** The size probabilities must be numeric.\n")
        if (any(size.prob < 0)) 
            stop("\n*** The size probabilities cannot be negative.\n")
        if (all(size.prob == 0)) 
            stop(paste("\n*** At least one size probability must be", 
                "greater then 0.\n"))
    }
    if ((exposure.method != "user") & (exposure.method != "beta")) 
        stop(paste("\n*** The parameter <exposure.method> must be either", 
            "'beta' or 'user'.\n"))
    if (!is.numeric(exposure.min) | !is.numeric(exposure.max)) 
        stop("\n*** The exposure bounds must be numeric.\n")
    if ((exposure.min < 0) | (exposure.max < 0)) 
        stop("\n*** The exposure boundaries must be greater than 0.\n")
    if (exposure.min > exposure.max) 
        stop("\n*** The lower exposure bound is greater than the upper bound.\n")
    if (exposure.method == "beta") {
        exposure.values <- NA
        exposure.prob <- NA
        if (!is.numeric(exposure.mean)) 
            stop("\n*** The parameter <exposure.mean> must be numeric.\n")
        if ((exposure.mean <= exposure.min) | (exposure.mean >= 
            exposure.max)) 
            stop(paste("\n*** The parameter <exposure.mean> must be inside", 
                "the exposure boundaries.\n"))
        if (!is.numeric(exposure.shape)) 
            stop("\n*** The parameter <exposure.shape> must be numeric.\n")
        if (exposure.shape <= 0) 
            stop("\n*** The parameter <exposure.shape> must be greater than 0.\n")
    }
    if (exposure.method == "user") {
        exposure.mean <- NA
        exposure.shape <- NA
        if (length(exposure.values) != length(exposure.prob)) 
            stop(paste("\n*** There given number of exposure values does not", 
                "correspond to the given number of probabilities.\n"))
        if (!is.numeric(exposure.values)) 
            stop("\n*** The exposure values must be numeric.\n")
        if (any(exposure.values < exposure.min) | any(exposure.values > 
            exposure.max)) 
            stop("\n*** Some exposure values are out of bound.\n")
        if (!is.numeric(exposure.prob)) 
            stop("\n*** The exposure probabilities must be numeric.\n")
        if (any(exposure.prob < 0)) 
            stop("\n*** The exposure probabilities cannot be negative.\n")
        if (all(exposure.prob == 0)) 
            stop(paste("\n*** At least one exposure probability must be", 
                "greater then 0.\n"))
    }
    if (length(type.values) != length(type.prob)) 
        stop(paste("\n*** There number of given type values does not", 
            "correspond to the number of probabilities.\n"))
    if (!is.numeric(type.prob)) 
        stop("\n*** The type probabilities must be numeric.\n")
    if (any(type.prob < 0)) 
        stop("\n*** The type probabilities cannot be negative.\n")
    if (all(type.prob == 0)) 
        stop(paste("\n*** At least one type probability must be", 
            "greater then 0.\n"))
    reg<-density.pop$region
    if (exposure.method == "beta") {
### Confusion here between 'mu' and 'exposure.mean'; this section modifies
#   'mu', but the value in the list returned is the unmodified 'exposure.mean'.
#   Clearer if we stick with 'exposure.mean' and reserve 'mu' for the scaled
#   [0,1] value used to calculate alpha and beta. (MM 27 May 2007)
#        mu <- exposure.mean
        shape <- exposure.shape
        if (adjust.interactive == TRUE) {
#            pars.exposure <- adjust.exposure(mu, shape, exposure.min, 
            pars.exposure <- adjust.exposure(exposure.mean, shape, exposure.min, 
                exposure.max)
#            mu <- pars.exposure$mu
            exposure.mean <- pars.exposure$mu
            shape <- pars.exposure$shape
        }
#        mu <- (mu - exposure.min)/(exposure.max - exposure.min)
        mu <- (exposure.mean - exposure.min)/(exposure.max - exposure.min)
        alpha <- mu/shape
        beta <- (1 - mu)/shape
        if (is.na(alpha) | is.na(beta)) 
            stop(paste("\n*** Exposure: The selected parameters lead to invalid", 
                "data.\n"))
    }
    else {
        alpha <- NA
        beta <- NA
    }
    if (size.method == "poisson") {
        if (adjust.interactive == TRUE) {
            size.mean <- adjust.size(size.mean, size.min, size.max)
            if (is.na(size.mean)) 
                stop(paste("\n*** Exposure: The selected parameters", 
                  "lead to invalid data.\n"))
        }
    }
    parents<-list(wisp.id(density.pop,newname=as.character(substitute(density.pop))))
    pars <- list(density=density.pop, number.groups = number.groups, 
        size.method = size.method, size.values = size.values, 
        size.prob = size.prob, size.min = size.min, size.max = size.max, 
        size.lambda = size.mean, exposure.method = exposure.method, 
        exposure.min = exposure.min, exposure.max = exposure.max, exposure.mean=exposure.mean, 
        exposure.values = exposure.values, exposure.prob = exposure.prob, 
        exposure.alpha = alpha, exposure.beta = beta, type.values = type.values, 
        type.prob = type.prob, parents=parents, created=date()) # removed region = reg, from list
    class(pars) <- "pars.population"
    return(pars)
}


#adjust.size<-function (lambda, size.min, size.max, ...) 
#{
#    stepsize <- 1
#    exit <- FALSE
#    refresh <- TRUE
#    which.legend <- 1
#    y.max <- 0.5
#    x <- size.min:size.max
#    cat("\n")
#    cat("********************************\n")
#    cat("***    press 'h' for help    ***\n")
#    cat("********************************\n\n")
#    while (exit == FALSE) {
#        if (refresh == TRUE) {
#            p <- dpois(x, lambda)/sum(dpois(x,lambda))
#            p.max <- max(p)
#            if (!is.na(p.max)) {
#                if (p.max < 0.1) 
#                  y.max <- 0.1
#                if ((p.max > 0.1) & (p.max < 0.2)) 
#                  y.max <- 0.2
#                if ((p.max > 0.2) & (p.max < 0.5)) 
#                  y.max <- 0.5
#                if (p.max > 0.5) 
#                  y.max <- 1
#            }
#            plot(1, 1, xlim = c(min(x), max(x)), ylim = c(0, 
#                y.max), type = "n", xlab = "size", ylab = "probability", 
#                ...)
#            abline(h = seq(0.05, y.max, by = 0.05), col = "grey")
#            lines(x, p, col = "red", type = "h")
#            lines(x, p, col = "blue")
#            if (which.legend == 1) {
#                legend(min(x), y.max, paste("mean =", lambda), 
#                  bg = "gray90")
#            }
#        }
#        refresh <- FALSE
#        valid <- FALSE
#        prompt <- paste("stepsize <", stepsize, "> : ", sep = "")
#        cat(prompt)
#        cmd <- readline()
#        if (cmd == "q") {
#            exit <- TRUE
#            valid <- TRUE
#        }
#        if (cmd == "h") {
#            cat("\n\n------------------- HELP --------------------\n")
#            cat("\n")
#            cat("The image shows the distribution of the\n")
#            cat("size values of a population.\n")
#            cat("\n")
#            cat("Commands:\n")
#            cat("+ : increase mean value\n")
#            cat("- : decrease mean value\n")
#            cat("s : change <s>tepsize of + and -\n")
#            cat("l : toggle <l>egend display\n")
#            cat("q : <q>uit\n")
#            cat("h : show help\n")
#            cat("---------------------------------------------\n\n")
#            valid <- TRUE
#        }
#        if (cmd == "l") {
#            if (which.legend < 1) {
#                which.legend <- which.legend + 1
#            }
#            else {
#                which.legend <- 0
#            }
#            refresh <- TRUE
#            valid <- TRUE
#        }
#        if ((cmd == "+") | (cmd == "-")) {
#            if (cmd == "+") {
#                dlambda <- 1
#                refresh <- TRUE
#            }
#            if (cmd == "-") {
#                dlambda <- -1
#                refresh <- TRUE
#            }
#            lambda.new <- lambda + stepsize * dlambda
#            if (lambda.new <= 0) {
#                cat("*** Boundary reached\n")
#            }
#            else lambda <- lambda.new
#            valid <- T
#        }
#        if (cmd == "s") {
#            cat("\nNew stepsize value : ")
#            input <- readline()
#            new.stepsize <- as.numeric(input)
#            if (is.na(new.stepsize) | !is.numeric(new.stepsize)) {
#                cat("\n\n*** Invalid stepsize value\n")
#            }
#            else {
#                if (new.stepsize < 0) {
#                  cat("\n\n*** Invalid stepsize value\n")
#                }
#                else {
#                  stepsize <- new.stepsize
#                }
#            }
#            cat("\n")
#            valid <- TRUE
#        }
#        if (valid == FALSE) 
#            cat("*** Unrecognized command\n")
#    }
#    cat("\nSelected parameter value stored for later use ...\n\n")
#    return(lambda)
#}
#
adjust.size <- function(lambda, xmin, xmax)
#	Modified to use Tcl/tk slider
#		Suggested by Mike Merridith, WCS, June 2007 by Rexstad
{

 ## Odd variables needed for plotting:
   mean.resol <- 1   #   (xmax - xmin)/100

 ## Set up tcl/tk environment:
    require(tcltk)
    xlambda <- tclVar(lambda)
#    sh <- tclVar(0)     # 1 = show histogram

 ## Function to draw the figure
    poisson.refresh <- function(...) {
        lambda <<- as.numeric(tclvalue(xlambda))
        hist(rpois(2000,lambda), prob=TRUE, main='Adjust size', xlab='Group size', xlim=c(xmin,xmax))
    }

 ## Draw initial graphic:
    poisson.refresh()
    bringToTop()

 ## Do the tcl/tk window:
    m <- tktoplevel()
    tkwm.title(m, "Adjust Size")
    tkwm.geometry(m, "+0+0")

 # Slider for mean:
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Lambda", width = "10"), side = "right")
    sc <- tkscale(fr, command = poisson.refresh, from = xmin+mean.resol,
        to = xmax-mean.resol, orient = "horiz", resolution = mean.resol,
        showvalue = T, variable = xlambda)
    tkpack(sc, side = "left")

 # Button
    tkpack(tkbutton(m, text = "DONE", command = function() tkdestroy(m)))

 ## Wait until "DONE"
   tkwait.window(m)

 ## Return the result:
   return(lambda)
}

is.pars.population<-function (poppars) 
{
     inherits(poppars,"pars.population")
}



summary.pars.population<-function(pars, digits=5, plot=FALSE) 
{
# check class:
 if (!is.pars.population(pars)) stop("\nThe parameter <pars> must be of type 'pars.population'.\n")
# unpack stuff:
 N <- pars$number.groups
 reg<-pars$density$reg
# Calculate E[s]:
 if(pars$size.method == "poisson") {
   lambda<-pars$size.lambda
   size<-seq(pars$size.min,pars$size.max,length=(pars$size.max-pars$size.min+1))
   Es<-sum(dpois(size,lambda)*size)/sum(dpois(size,lambda))
 }else {
   Es<-sum(pars$size.prob*pars$size.values)
 }
 if(pars$exposure.method=="beta") {
   exposure.mean<-pars$exposure.mean
 }else {
   exposure.mean<-sum(pars$exposure.prob*pars$exposure.values)
 }
# printing:
 cat("\n")
 cat("POPULATION PARAMETER SUMMARY\n")
 cat("----------------------------\n")
 cat("creation date   :", pars$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(pars$parents)) {
   cat("      ",paste("(",pars$parents[[i]]$class,", ",pars$parents[[i]]$name,", ",pars$parents[[i]]$created,")",sep=""),"\n")
 }
 cat("\n")
 cat("Region (length x width):", reg$length, "x", reg$width, "\n\n")
 cat("Number of groups     :", N, "\n")
 cat("Number of individuals:",round(N*Es),"\n")
 cat("\n")
 cat("Group size distribution method :",pars$size.method, "\n")
 if(pars$size.method=="user") {
   cat("Group sizes                    :",pars$size.values, "\n")
   cat("Group size probabilities       :",pars$size.prob, "\n")
 }
 cat("Group size minimum             :",pars$size.min, "\n")
 cat("Group size maximum             :",pars$size.max, "\n")
 cat("Group size mean                :",signif(Es,digits), "\n")
 cat("\n")
 cat("Exposure method      :",pars$exposure.method, "\n")
 cat("Exposure minimum     :",pars$exposure.min, "\n")
 cat("Exposure maximum     :",pars$exposure.max, "\n")
 if(pars$exposure.method=="user") {
   cat("Exposure values      :",pars$exposure.values, "\n")
   cat("Exposure probabilites:",pars$exposure.prob, "\n")
 }
 cat("Exposure mean        :",signif(exposure.mean,digits), "\n")
 cat("\n")
 if(!is.na(pars$type.values[1])) {
   cat("Type probabilities :",pars$type.prob, "\n")
   cat("Types              :",pars$type.values, "\n")
 }
    
# plotting:
 if(plot) plot(pars)
}


plot.pars.population<-function(pars) 
#----------------------------------------------------------------------------
# Should tidy this up some variables are surplus
#----------------------------------------------------------------------------
{
# check class:
 if (!is.pars.population(pars)) stop("\nThe parameter <pars> must be of type 'pars.population'.\n")
# unpack stuff:
 N <- pars$number.groups
 density <- pars$density
 exposure.method <- pars$exposure.method
 exposure.min <- pars$exposure.min
 exposure.max <- pars$exposure.max
 exposure.val <- pars$exposure.values
 exposure.prob <- pars$exposure.prob
 exposure.alpha <- pars$exposure.alpha
 exposure.beta <- pars$exposure.beta
 size.method <- pars$size.method
 size.val <- pars$size.values
 size.prob <- pars$size.prob
 size.min <- pars$size.min
 size.max <- pars$size.max
 size.lambda <- pars$size.lambda
 vtypes <- pars$type.values
 ptypes <- pars$type.prob
 reg <- pars$density$region
 reg.length <- reg$length
 reg.width <- reg$width

# plotting:
 old.par <- par(no.readonly=TRUE)
 one.exposure <- (exposure.max == exposure.min)
# set scaling factor for labels, axes and text 
 cex<-0.75*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE, mgp=c(3, 1, 0)*cex)
 par(mfrow = c(2, 2))

 plot(density,method="image", reset.par=FALSE)
 title("Density surface")

 if(exposure.method == "beta") {
   x <- seq(0.0001, 0.9999, length=100)
   p <- dbeta(x, exposure.alpha, exposure.beta)
   p.max <- max(p)
   x<-x*(exposure.max-exposure.min)+exposure.min  # (shift and scale onto exposure range)
   plot(x,p, xlim=c(min(x), max(x)), ylim = c(0, p.max), type="l", col="blue",  xlab="exposure", ylab="probability density", main="Exposure distribution")
 }
if (exposure.method == "user") {
   x <- exposure.val
   p <- exposure.prob
   p.max <- max(p)
   plot(x, p, xlim=c(min(x), max(x)), ylim=c(0,p.max), type="l", col="blue", xlab="exposure", ylab="probability", main="Exposure distribution")
   lines(x, p, col = "red", type = "h")
 }

 if (size.method == "poisson") {
   x <- size.min:size.max
   p <- dpois(x, size.lambda)/sum(dpois(x, size.lambda))
   p.max <- max(p)
   plot(x, p, xlim=c(size.min, size.max), ylim=c(0, p.max), type="l", col="blue", xlab="size", ylab="probability", main="Group size distribution")
   lines(x, p, col = "red", type = "h")
 }
 if (size.method == "user") {
   x <- size.val
   p <- size.prob
   p.max <- max(p)
   plot(x, p, xlim=c(min(x), max(x)), ylim=c(0, p.max), type="l", col="blue", xlab="size", ylab="probability", main="Group size distribution")
   lines(x, p, col = "red", type = "h")
 }
 if(!is.na(pars$type.values[1])) {
    typename <- sort(unique(pars$type.values))
    n.types <- length(typename)
    typenumber <- 1:n.types
    type.n <- rep(NA, length(pars$type.values))
    for (i in 1:n.types) type.n[pars$type.values == typename[i]] <- i
    ht<-hist(type.n, breaks = (c(0:n.types)+0.5), plot=FALSE)$counts
    barplot(ht, names.arg=typename,xlab="Type",main="Type distribution",col="grey")
 } else plot.text("All animal types are identical")
 par(old.par)
}



generate.population<-function (pars.population, seed=NULL) 
{
    if (!is.pars.population(pars.population)) 
        stop("\nThe parameter <pars.population> must be of class 'pars.population'.\n")
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
    pars <- pars.population
    N <- pars$number.groups
    density.mat <- pars$density$matrix
    expos.method <- pars$exposure.method
    expos.min <- pars$exposure.min
    expos.max <- pars$exposure.max
    expos.val <- pars$exposure.values
    expos.prob <- pars$exposure.prob
    expos.alpha <- pars$exposure.alpha
    expos.beta <- pars$exposure.beta
    size.method <- pars$size.method
    size.val <- pars$size.values
    size.prob <- pars$size.prob
    size.min <- pars$size.min
    size.max <- pars$size.max
    size.lambda <- pars$size.lambda
    vtypes <- pars$type.values
    ptypes <- pars$type.prob
    reg <- pars$density$reg # changed "region" to "density$reg"
    reg.length <- reg$length
    reg.width <- reg$width
    nx <- dim(density.mat)[1]
    ny <- dim(density.mat)[2]
    groupID <- 1:N
    n.cells <- nx * ny
#    n.animals <- tabulate(sample(x = 1:n.cells, size = N, replace = TRUE, 
#        prob = as.vector(density.mat)))
### 'sample' gives an incomprehensible warning the first time it is called.
#   Shut it up!!  (MM 3 June 07)
    tmp <- suppressWarnings(sample(x = 1:n.cells, size = N, replace = TRUE, 
        prob = as.vector(density.mat)))
    n.animals <- tabulate(tmp)
    if (length(n.animals) < n.cells) 
        n.animals <- c(n.animals, rep(0, n.cells - length(n.animals)))
    groups.mat <- matrix(n.animals, nrow = nx)
    dx <- reg.length/nx
    dy <- reg.width/ny
    pos.x <- rep(rep(seq(0, reg.length - dx, length = nx), ny), 
        as.vector(groups.mat)) + runif(N, 0, dx)
    pos.y <- rep(rep(seq(0, reg.width - dy, length = ny), rep(nx, 
        ny)), as.vector(groups.mat)) + runif(N, 0, dy)
    if (expos.method == "user") {
        if (length(expos.val) == 1) 
            expos <- rep(expos.val, N)
        else expos <- sample(expos.val, N, replace = TRUE, prob = expos.prob)
    }
    else {
        expos <- rbeta(N, expos.alpha, expos.beta)
        expos <- expos.min + (expos.max - expos.min) * expos
        if (any(expos < expos.min) | any(expos > expos.max)) 
            cat("\n*** warning: invalid exposure values occured.\n")
    }
    if (size.method == "user") {
        if (length(size.val) == 1) 
            sizes <- rep(size.val, N)
        else sizes <- sample(size.val, N, replace = TRUE, prob = size.prob)
    }
    else {
        pmin <- ppois(size.min - 1, size.lambda)
        pmax <- ppois(size.max, size.lambda)
        r <- runif(N, pmin, pmax)
        while (any(r == (size.min - 1))) {
            r <- runif(N, pmin, pmax)
        }
        sizes <- qpois(r, size.lambda)
        if (any(sizes < size.min) | any(sizes > size.max)) 
            cat("\n*** warning: invalid size values occured.\n")
    }
    ntypes <- length(vtypes)
    if (!is.na(vtypes[1])) 
        if (length(vtypes) == 1) 
            types <- rep(vtypes, N)
        else types <- sample(vtypes, N, replace = TRUE, prob = ptypes)
    else types <- rep(NA, N)
    parents<-list(wisp.id(pars.population,newname=as.character(substitute(pars.population))))
    pop <- list(region = reg, groupID = groupID, posx = pos.x, 
        posy = pos.y, groupsize = sizes, ntypes = ntypes, types = types, 
        minexposure = expos.min, maxexposure = expos.max, exposure = expos, parents=parents, created=date(), seed=seed)
    class(pop) <- "population"
    pop
}




summary.population<-function (pop, plot=FALSE, digits=5, ...) 
{
    if (!is.population(pop)) 
        stop("\nThe parameter <pop> must be of type 'population'.\n")
    cat("\n")
    cat("POPULATION SUMMARY\n")
    cat("------------------\n")
    cat("creation date   :", pop$created,"\n")
    cat("parent object(s) (class, name, creation date):\n")
    for(i in 1:length(pop$parents)) {
      cat("      ",paste("(",pop$parents[[i]]$class,", ",pop$parents[[i]]$name,", ",pop$parents[[i]]$created,")",sep=""),"\n")
    }
    if(is.numeric(pop$seed)) cat("random number seed used: ",pop$seed,"\n")
    cat("\n")
    cat("Region (length x width):", pop$region$length, "x", pop$region$width, 
        "\n\n")
    n.groups <- length(sort(unique(pop$groupsize)))
    one.exposure <- (pop$minexposure == pop$maxexposure)
    N<-length(pop$groupID)
    Es<-mean(pop$groupsize)
    cat("Number of groups     :", N, "\n")
    cat("Number of individuals:", signif(N*Es,digits), "\n")
    cat("\n")
    cat("Group sizes          :", sort(unique(pop$groupsize)),"\n")
    cat("Mean group size      :", signif(mean(pop$groupsize),digits),"\n")
    cat("\n")
    cat("Exposure boundaries  : [", pop$minexposure, ",", pop$maxexposure, "]\n", sep = "")
    cat("Mean exposure        :", signif(mean(pop$exposure),digits),"\n")
    cat("\n")
    if(!is.na(pop$types[1])) {
      cat("Types                :", names(table(pop$types)), "\n")
      cat("Numbers of each type :", table(pop$types), "\n")
    }
    if(plot) plot(pop)
}


plot.population<-function (pop, show.sizes=TRUE, show.exps=TRUE, dsf=0.75, group.col="black", type="details", title="Group locations") 
{
 if (!is.population(pop)) stop("\nThe parameter <pop> must be of type 'population'.\n")
 par.old <- par(no.readonly = TRUE)
# set scaling factor for labels, axes and text to be 50% (plot window height)/5
 if(type!="details") cex<-0.9*par()$din[2]/5
 else {
   cex<-0.75*par()$din[2]/5
   dsf<-dsf*0.5 # size of dots in animal location plot
   par(mfrow = c(2, 2))
 }
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE, mgp=c(3, 1, 0)*cex)
 plot(pop$region, reset.pars=FALSE)
 title(main=title)
 plot.groups(pop, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col=group.col)
# points(pop$posx, pop$posy, pch = 19, cex = dot.size, col = col.exps, xpd = TRUE)
 if(type=="details") {
   one.exposure<-(pop$minexposure==pop$maxexposure)
   n.groups <- length(sort(unique(pop$groupsize)))
   xlim<-c(pop$minexposure, pop$maxexposure)
   if (!one.exposure) hist(pop$exposure, main = "Exposure distribution", xlab = "Exposure", xlim=xlim)
   else plot.text(paste("All exposures are ",pop$exposure[1]),col="black", cex=cex)

   if (n.groups > 1) {
     min.size <- min(pop$groupsize)
     max.size <- max(pop$groupsize)
     
     hist(pop$groupsize, breaks = (c(0, min.size:max.size) + 0.5), 
          main = "Group size distribution", xlab = "Group size")
   } else plot.text(paste("All group sizes are ",pop$groupsize[1]),col="black", cex=cex)

   if (!is.na(pop$types[1])) {
     typename <- sort(unique(pop$types))
     n.types <- length(typename)
     typenumber <- 1:n.types
     type.n <- rep(NA, length(pop$types))
     for (i in 1:n.types) type.n[pop$types == typename[i]] <- i
            ht <- hist(type.n, breaks = (c(0:n.types) + 0.5), 
###                main = "Type distribution", xlab = "Type code", 
                plot = FALSE)$counts
### 'hist' with plot = FALSE doesn't use main or xlab, and warns! (MM 1 June 07)
     barplot(ht, names.arg=typename,xlab="Type",main="Type distribution",col="grey")
   } else plot.text(paste("All groups are of the same type"), cex=cex)
 }
 par(par.old)
}



plot.groups<-function (pop, show.sizes=TRUE, show.exps=TRUE, newplot=TRUE, dsf=0.5, group.col="black") 
#-----------------------------------------------------------------------------
# Plots group locations. You should call plot.region() with reset.pars=FALSE
# once before calling this function, to draw the region and get the correct
# aspect ratio for the plot.
#-----------------------------------------------------------------------------
{
 if (!is.population(pop)) stop("\nThe parameter <pop> must be of type 'population'.\n")
 if ((show.sizes != TRUE) & (show.sizes != FALSE)) stop("\nThe parameter <show.sizes> must be TRUE or FALSE.\n")
 if ((show.exps != TRUE) & (show.exps != FALSE)) stop("\nThe parameter <show.exps> must be TRUE or FALSE.\n")
# if ((newplot != TRUE) & (newplot != FALSE)) stop("\nThe parameter <newplot> must be TRUE or #FALSE.\n")
 if (!is.numeric(dsf)) stop("\nThe parameter <dsf> must be numeric.\n")
 if (dsf <= 0) stop("\nThe parameter <dsf> must be greater than 0.\n")
 if (!(group.col %in% c("black", "red", "green", "blue", "yellow"))) 
   stop(paste("\nThe selected color <group.col> can only be 'black', 'red',", "'green', 'blue' or 'yellow'.\n"))
 if (show.exps) {
   if (pop$maxexposure == pop$minexposure) lev <- rep(0, length(pop$exposure))
   else lev <- 0.75 * (1 - (pop$exposure - pop$minexposure)/(pop$maxexposure - pop$minexposure))
   if (group.col == "black") col.exps <- rgb(lev, lev, lev)
   if (group.col == "red") col.exps <- rgb(1, lev, lev)
   if (group.col == "green") col.exps <- rgb(lev, 1, lev)
   if (group.col == "yellow") col.exps <- rgb(1, 1, lev)
   if (group.col == "blue") col.exps <- rgb(lev, lev, 1)
 }
 else col.exps <- group.col
 dot.size <- dsf
 if (show.sizes) dot.size <- sqrt(pop$groupsize) * dot.size
 points(pop$posx, pop$posy, pch=19, cex=dot.size, col=col.exps, xpd=TRUE)
}

#adjust.exposure <- function (mu, shape, min.exposure, max.exposure,...)
#
#   #-----------------------------------------------------
#   # description:
#   #   The function creates an object that contains
#   #   parameters needed to calculate distribution of
#   #   exposure values of a population.
#   #
#   #   Therefore the function draws this distribution funciton
#   #   which the user can change interactively by changing 
#   #   the values of some parameter step by step.
#   # 
#   #   Start values can be passed to the function.
#   #
#   # author: M. Erdelmeier
#   #-----------------------------------------------------
#
#   #-----------------------------------------------------
#   # input/output-variables:
#   #-----------------------------------------------------
#
#   # name         | type | I/O | description
#   #---------------------------------------------------------------------
#   # max.exposure | real |  I  | biggest possible exposure value
#   # min.exposure | real |  I  | littlest possible exposure value
#   # mu           | real |  I  | mean of the beta distribution
#   #              |      |     | (boundaries expanded to
#   #              |      |     | [min.expos, max.expos]
#   # shape        | real |  I  | shape parameter of beta distribution
#
#
#   #-----------------------------------------------------
#   # used objects
#   #-----------------------------------------------------
#
#   # name           | type            | R/W | description
#   #---------------------------------------------------------------------
#
#
#   #-----------------------------------------------------
#   # local variables
#   #-----------------------------------------------------
#
#   # name         | type         | description
#   #-----------------------------------------------------------------
#   # alpha        | real         | parameter of beta distribution
#   # beta         | real         | parameter of beta distribution
#   # cmd          | char         | given command input
#   # dmean        | real         | increasement/decreasement in mean value
#   # dshape       | real         | increasement/decreasement in shape value
#   # exit         | Boolean      | T if function shall be left
#   # input        | char         | given user input
#   # m            | real         | mean of beta distribution (transformed
#   #              |              | to [0,1]
#   # mode         | char         | shows which parameter values are
#   #              |              | changed in the moment ("mean",
#   #              |              | "shape")
#   # mu.new       | real         | new calculated mean value
#   # n            | int          | number of x values (needed in plotting)
#   # new.stepsize  | real         | new stepsize value given by user
#   # p            | vector       | density of beta distribution that
#   #              | of real      | shall be plotted
#   # prompt       | char         | shown input prompt
#   # p.max        | real         | maximum y value of plotting
#   # quant        | vector       | quantiles of beta distribution used in
#   #              | of real      | plotting
#   # refresh      | Boolean      | TRUE if plot refresh is necessary
#   # stepsize      | real         | stepsize factor of increasement/decreasement
#   #              |              | steps of parameters
#   # shape.new    | real         | new calculated shape value
#   # text         | vector       | vector for legend display
#   #              | of char      |
#   # which.legend | int          | shows selected legend:
#   #              |              |   0: no legend
#   #              |              |   1: parameter legend
#   # x            | vector       | vector of x values (used in plotting)
#   #              | of real      |
#   # valid        | boolean      | TRUE if user gave a valid input
#   # y.max        | real         | plot limit in y direction
#
#
#   #-------------------------------------------------------
#   # programming part
#   #-------------------------------------------------------
#
#{
#
#   # initialisations
#   stepsize <- 1           # start value
#   exit <- F              # not finished
#   refresh <- T           # plot a new image
#   which.legend <- 1      # show parameter legend
#   mode <- "mean"         # first change mean value
#   n <- 100               # number of plot values
#   y.max <- 10            # y plot boundary
#
#   # get plot values
#   x <- seq(min.exposure, max.exposure, length=n)
#
#   # show help text
#   cat ("\n")
#   cat ("********************************\n")
#   cat ("***    press 'h' for help    ***\n")
#   cat ("********************************\n\n")
#
#   # repeat until user break
#   while (exit==FALSE)
#   {
#      # refresh of plot necessary ?
#      if (refresh==TRUE)
#      {
#         # transform <mu> into interval [0,1]
#         m <- (mu-min.exposure)/(max.exposure-min.exposure)
#
#         # transform parameter of beta function
#         alpha <- m/shape
#         beta <- (1-m)/shape
#
#         # calculate probabilities 
#         quant <- seq(0, 1, length=n)
#         p <- dbeta(quant, alpha, beta)
#
#         # calculate a "well dimensioned" y boundary
#         p.max <- max(dbeta(0.05, alpha, beta), dbeta(0.95, alpha, beta),
#                      dbeta(m, alpha, beta))
#         if (!is.na(p.max))
#         {
#            if (p.max<1)
#               y.max <- 1             # set to 1
#            if ((p.max>1) & (p.max<5))
#               y.max <- 5             # set to 5
#            if (p.max>5)
#               y.max <- 10            # set to 10
#         }
#
#         # draw probability graph (with scaling lines)
#         plot(1,1, xlim=c(min(x), max(x)), ylim=c(0,y.max), type="n",
#               xlab="exposure", ylab="probability",...)
#         abline(h=seq(0.5, abs(2*y.max)/2, by=0.5), col="grey")
#         lines (x, p, col="blue")
#
#         # show parameter legend? 
#         if (which.legend==1)
#         {
#            # legend of parameter explication
#            text <- c(paste("mean =", mu),
#                      paste("shape =", shape))
#            legend (min(x) ,y.max, text, bg='gray90')
#         }
#      }
#
#      # get user selection
#      refresh <- FALSE       # init: no plot refresh
#      valid <- FALSE         # init: invalid input
#      prompt <- paste ("<",mode, "> mode, stepsize <", stepsize, 
#           "> : ", sep="")
#      cat (prompt)
#      cmd <- readline() 
#
#      # quit selected
#      if (cmd=="q")
#      {
#         exit <- TRUE       # leave function
#         valid <- TRUE      # valid command
#      }
#
#      # help selected
#      if (cmd=="h")
#      {
#         # show selectable commands
#         cat ("\n\n------------------- HELP --------------------\n")
#         cat ("\n")
#         cat ("The image shows the distribution of the\n")
#         cat ("exposure values of a population.\n")
#         cat ("\n")
#         cat ("Commands:\n")
#         cat ("m : switch to changing of <m>ean value\n")
#         cat ("p : switch to changing of sha<p>e value\n")
#         cat ("+ : increase value\n")
#         cat ("- : decrease value\n")
#         cat ("s : change <s>tepsize of + and -\n")
#         cat ("l : toggle <l>egend display\n")
#         cat ("q : <q>uit\n")
#         cat ("h : show help\n")
#         cat ("---------------------------------------------\n\n")
#
#         valid <- T      # valid command
#      }
#
#      # change to mean value
#      if (cmd=="m")
#      {
#         mode <- "mean"       # change mode
#         valid <- TRUE           # valid command
#      }
#
#      # change to shape value
#      if (cmd=="p")
#      {
#         mode <- "shape"      # change mode
#         valid <- TRUE           # valid command
#      }
#
#      # toggle legend display
#      if (cmd=="l")
#      {   
#         # jump to next legend      
#         if (which.legend<1)
#         {
#            # switch to legend display
#            which.legend <- which.legend+1
#         } else
#         {
#            # turn legend off
#            which.legend <- 0
#         }
#         refresh <- TRUE       # new plot
#         valid <- TRUE         # valid command
#      }
#
#      # increase or decrease selected?
#      if ((cmd=="+") | (cmd=="-"))
#      {
#         # initializations
#         dmean <- 0
#         dshape <- 0
#
#         # increase current parameter
#         if (cmd=="+")
#         {
#            # set increase value of current parameter
#            if (mode=="mean")
#               dmean <- 1
#            if (mode=="shape")
#               dshape <- 1
#
#            refresh <- TRUE       # new plot
#         }
#
#         # decrease current parameter
#         if (cmd=="-")
#         {
#            # set decrease value of curreent parameter
#            if (mode=="mean")
#               dmean <- -1
#            if (mode=="shape")
#               dshape <- -1
#
#            refresh <- TRUE       # new plot
#         }
#
#         # calculate changes
#         mu.new <- mu + stepsize*dmean
#         shape.new <- shape + stepsize*dshape
#
#         # test if changed values are valid. If so, take over
#         # changes 
#         if ((mu.new<=min.exposure) | (mu.new>=max.exposure))
#         { 
#            cat ("*** Boundary reached\n")
#         } else mu <- mu.new
#         if (shape.new<=0)
#         { 
#            cat ("*** Boundary reached\n")
#         } else shape <- shape.new
#
#         valid <- TRUE      # valid command
#      }
#
#      # change stepsize
#      if (cmd=="s")
#      {
#         # get new stepsize value
#         cat ("\nNew stepsize value : ")
#         input <- readline()
#
#         # test input value
#         new.stepsize <- as.numeric(input)
#         if (is.na(new.stepsize) | !is.numeric(new.stepsize))
#         {
#            cat ("\n\n*** Invalid stepsize value\n")
#         } else
#         {
#            if (new.stepsize<0)
#            {
#               cat ("\n\n*** Invalid stepsize value\n")
#            } else
#            {
#               # store new value
#               stepsize <- new.stepsize
#            }
#         }
#
#         cat ("\n")      # insert line
#         valid <- TRUE      # valid command
#      }
#
#      # test if command was valid
#      if (valid==FALSE)
#         cat ("*** Unrecognized command\n")
#
#   } # end while
#
#   # display final message
#   cat ("\nSelected parameter values stored for later use ...\n\n")
#
#   # return result
#   pars <- list(mu=mu, shape=shape)
#   return(pars)
#}

adjust.exposure <- function(xmean, shape, xmin, xmax)
# Suggested alternative by Mike Meredith, mmeredith@wcs.org, 26 May 2007
# Code cleaned up 29 May 2007

{
   shape <- min(shape, 10) # Shapes >10 not very interesting!

 ## Odd variables needed for plotting:
   x <- seq(xmin, xmax, length=101)
   theta <- seq(0,1,length=101)
   mean.resol <- (xmax - xmin)/100

 ## Set up tcl/tk environment:
    require(tcltk)
    xmn <- tclVar(xmean)
    shp <- tclVar(shape)
    sh <- tclVar(0)     # 1 = show histogram

 ## Function to draw the figure
    beta.refresh <- function(...) {
        xmean <<- as.numeric(tclvalue(xmn))
        shape <<- as.numeric(tclvalue(shp))
        shist <- as.numeric(tclvalue(sh))
        mu <- (xmean - xmin) / (xmax - xmin)
        alpha <- mu / shape
        beta <- (1-mu) / shape
        if(shist) {
           nbars <- 10  # Number of bars in histogram; change is desired
           qs <- seq(0, 1, length=nbars + 1)
           beta.hist <- diff(pbeta(qs,alpha,beta)) * nbars
        } else
           beta.hist <- 0
        beta.curve <- dbeta(theta, alpha, beta)
        ylim <- range(0, beta.curve[is.finite(beta.curve)],beta.hist)
        plot(x, beta.curve, type='n',ylim=ylim, main="Adjust exposure",
           xlab = "Exposure", ylab = "", yaxt='n')
        if(shist) {
           xs <- seq(xmin, xmax, length=nbars + 1)
           rect(xs[1:nbars],0,xs[2:(nbars+1)],beta.hist)
         }
         points(x, beta.curve, type='l', col='red', lwd=2)
    }

 ## Draw initial graphic:
    beta.refresh()
    bringToTop()

 ## Do the tcl/tk window:
    m <- tktoplevel()
    tkwm.title(m, "Adjust Exposure")
    tkwm.geometry(m, "+0+0")

 # Slider for mean:
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Mean", width = "10"), side = "right")
    sc <- tkscale(fr, command = beta.refresh, from = xmin+mean.resol, 
        to = xmax-mean.resol, orient = "horiz", resolution = mean.resol,
        showvalue = T, variable = xmn)
    tkpack(sc, side = "left")

 # Slider for shape:
    tkpack(fr <- tkframe(m), side = "top")
    tkpack(tklabel(fr, text = "Shape", width = "10"), side = "right")
    sc <- tkscale(fr, command = beta.refresh, from = 0.1, variable=shp,
        to = 10, orient = "horiz", resolution = 0.1, showvalue = T)
    tkpack(sc, side = "left")

 # Check box for 'Show histogram':
    tkpack(fr <- tkframe(m), side = "top")
    sc <- tkcheckbutton(fr, command = beta.refresh, variable = sh,
       text = "Show histogram")
    tkpack(sc, side = "left")

 # Button
    tkpack(tkbutton(m, text = "DONE", command = function() tkdestroy(m)))

 ## Wait until "DONE"
   tkwait.window(m)

 ## Return the result: 
   return(list(mu=xmean, shape=shape))
}
is.population <- function (pop) 
#----------------------------------------------------------------
# description:
#    tests if the given object <pop> if of type "population"
#
# author: M. Erdelmeier
#----------------------------------------------------------------
{
    ## test if <pop> is of the type "population"
    inherits(pop, "population")
}


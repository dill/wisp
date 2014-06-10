# Removal method functions


generate.design.rm<-function (reg, n.occ = 2, effort = rep(1, n.occ)) 
{
    if (!is.region(reg)) 
        stop("\n*** The parameter <reg> must be of type 'region'.\n")
    if (!is.numeric(n.occ)) 
        stop("\n*** The number of occasions must be numeric.\n")
    if (n.occ != as.integer(n.occ)) 
        stop("\n*** The number of occasions must be of type integer.\n")
    if (n.occ < 2) 
        stop("\n*** The number of occasions must be at least 2.\n")
    if (!is.numeric(effort)) 
        stop("\n*** The search efforts must be numeric.\n")
    if (length(effort) != n.occ) 
        stop(paste("\n*** The number of given values for search effort", 
            "must be identical to the number of search occasions.\n"))
    parents<-list(wisp.id(reg,newname=as.character(substitute(reg))))
    des <- list(region = reg, number.occasions = n.occ, effort = effort, parents=parents, created=date())
    class(des) <- "design.rm"
    return(des)
}

is.design.rm <- function (des) 
{
    # test if <des> is of the type "design.rm"
    inherits(des, "design.rm")
}


summary.design.rm<-function(des)
{
# check class:
 if (!is.design.rm(des)) stop("\nThe parameter <des> must be of type 'design.rm'.\n")
 cat("\n")
 cat("REMOVAL METHODS DESIGN SUMMARY\n")
 cat("------------------------------\n")
 cat("creation date   :", des$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(des$parents)) {
   cat("      ",paste("(",des$parents[[i]]$class,", ",des$parents[[i]]$name,", ",des$parents[[i]]$created,")",sep=""),"\n")
 }
 cat("\n")
 cat("Number of sampling occasions    :", des$number.occasions,"\n")
 cat("Sampling effort on each occasion:", des$effort,"\n")
 cat("Region dimensions (length x width):", des$reg$length, "x", des$reg$width, "\n")
}


#setpars.survey.rm<-function (pop, des, pmin, pmax = pmin, improvement = 0) 
#{
#    if (!is.population(pop)) 
#        stop("\n*** The parameter <pop> must be of type 'population'.\n")
#    if (!is.design.rm(des)) 
#        stop("\n*** The parameter <des> must be of type 'design.rm'.\n")
#    if (!equal(pop$region, des$region)) 
#        stop(paste("\n*** The given population and design were defined", 
#            "with different regions.\n"))
#    parents<-list(wisp.id(pop,newname=as.character(substitute(pop))), wisp.id(des,newname=as.character(substitute(des))))
#    min.exposure <- pop$minexposure
#    max.exposure <- pop$maxexposure
#    if (!is.numeric(pmin) | !is.numeric(pmax)) 
#        stop("\n<pmin > and <pmax > must be numeric.\n")
#    if ((pmin < 0) | (pmax < 0)) 
#        stop("\n<pmin> and <pmax> cannot be negative.\n")
#    if ((pmin >= 1) | (pmax >= 1)) 
#        stop("\n<pmin> and <pmax> cannot be 1 or bigger.\n")
#    if (pmin > pmax) 
#        stop("\n<pmin> cannot be bigger than <pmax>.\n")
#    if ((min.exposure == max.exposure) & (pmin != pmax)) 
#        print(paste("warning: Exposure boundaries are identical. Therefore", 
#            "<pmax> is ignored."))
#    if (!is.numeric(improvement)) 
#        stop("\n <improvement> must be numeric.\n")
#    if (improvement < 0) 
#        stop("\n <improvement> cannot be negative.\n")
#    theta <- transform.setpars.parameter(min.exposure, max.exposure, pmin, pmax, improvement)
#    pars <- list(population = pop, design = des, theta0 = theta$zero, 
#        theta1 = theta$one, theta2 = theta$two, parents=parents, created=date())
#    class(pars) <- "pars.survey.rm"
#    return(pars)
#}

# HERE'S AN UPDATE WHICH ALLOWS DIFFERENT CAPTURE PROBS FOR DIFFERENT TYPES:
setpars.survey.rm<-function (pop, des, pmin, pmax = pmin, improvement=0.0, type.prob = 1, separate.removal=FALSE, type.hetro=FALSE) 
{
    if (!is.population(pop)) 
        stop("\n*** The parameter <pop> must be of type 'population'.\n")
    if (!is.design.rm(des)) 
        stop("\n*** The parameter <des> must be of type 'design.rm'.\n")
    if (!equal(pop$region, des$region)) 
        stop(paste("\n*** The given population and design were defined", 
            "with different regions.\n"))
        
    if (!is.numeric(type.prob)) 
        stop("\n***Parameter type.prob: The population fraction of each type must be numeric.\n")
    if (!is.logical(separate.removal)) 
        stop("\n*** The parameter <separate.removal> must be of type logical.\n")
    if (!is.logical(type.hetro)) 
        stop("\n*** The parameter <type.hetro> must be of type logical.\n")
    
    # CHANGED by Stefan Kirchfeld, 2002-07-26
    # Only if heterogeneity shall be considered the parameter type.prob is important !
    if ( type.hetro )
    {
        if(pop$ntypes != length(type.prob)) 
        {
          cat("\n*** Error: The number of fractions given (type.prob) does not match the number of types\nin the population.")
          cat("\n*** If type.hetro is set TRUE then for each type a population fraction lower than 1\nhas to be given in type.prob parameter !")
          stop("\n*** type.prob is now: ",type.prob,".\n")
        }
    }
    t.prob<-rep(1,pop$ntypes)
    t.prob<-t.prob*type.prob
        
    for (i in 1:length(t.prob)) 
      if(t.prob [i]>1) {
          cat("\n A type.prob was >1; all rescaled so max(type.prob)=1.\n")
          t.prob<-t.prob/max(t.prob)
          cat("\ntype.prob is now: ",t.prob,".\n")
      }
    parents<-list(wisp.id(pop,newname=as.character(substitute(pop))), 
                  wisp.id(des,newname=as.character(substitute(des))))
    n.groups <- length(pop$groupID)
    min.exposure <- pop$minexposure
    max.exposure <- pop$maxexposure
    if (!is.numeric(pmin) | !is.numeric(pmax)) 
        stop("\n<pmin> and <pmax> must be numeric.\n")
    if ((pmin < 0) | (pmax < 0)) 
        stop("\n<pmin> and <pmax> cannot be negative.\n")
    if ((pmin >= 1) | (pmax >= 1)) 
        stop(paste("\n<pmin> and <pmax> cannot be as big as", 
            "or bigger than 100 percent.\n"))
    if (pmin > pmax) 
        stop("\n<pmin> cannot be bigger than <pmax>.\n")
    if ((min.exposure == max.exposure) & (pmin != pmax)) 
        print(paste("warning: Exposure boundaries are identical. Therefore", 
            "<pmax> is ignored."))
    if (!is.numeric(improvement)) 
        stop("\n <improvement> must be numeric.\n")
    if (improvement < 0) 
        stop("\n <improvement> cannot be negative.\n")
    theta <- transform.setpars.parameter(min.exposure, max.exposure, pmin, pmax, improvement)
    pars <- list(population = pop, design = des, theta0 = theta$zero, 
        theta1 = theta$one, theta2 = theta$two, type.prob=t.prob, separate.removal= separate.removal, type.hetro = type.hetro, parents=parents, created=date())
    class(pars) <- "pars.survey.rm"
   return(pars)
}

is.pars.survey.rm<-function (survpars) 
{
   inherits(survpars,"pars.survey.rm")
}

summary.pars.survey.rm<-function(pars,plot=FALSE, digits=5)
{
# check class:
 if (!is.pars.survey.rm(pars)) stop("\nThe parameter <pars> must be of type 'pars.survey.rm'.\n")
 cat("\n")
 cat("REMOVAL METHODS SURVEY PARAMETER SUMMARY\n")
 cat("----------------------------------------\n")
 cat("creation date   :", pars$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(pars$parents)) {
   cat("      ",paste("(",pars$parents[[i]]$class,", ",pars$parents[[i]]$name,", ",pars$parents[[i]]$created,")",sep=""),"\n")
 }
 x<-pars$population$exposure
# xp<-seq(pars$population$minexposure,pars$population$maxexposure,length=50)
 eff<-pars$des$effort
 th0<-pars$theta0
 th1<-pars$theta1
 th2<-pars$theta2
# calculate p's as funciton of exposures in population
 K<-length(eff)
 p<-detection.removalmethods(th0, th1, th2, x, eff)
 meanp<-apply(p,2,mean)
 max.meanp.occ<-which(meanp==max(meanp))[1]
 min.meanp.occ<-which(meanp==min(meanp))[1]
 max.meanp<-meanp[max.meanp.occ]
 min.meanp<-meanp[min.meanp.occ]
 cat("\n")
 cat("Capture function: p(catch)= 1-exp{-(theta0+theta1*exposure)*(1+theta2*(s-1))}\n")
 cat("  Intercept parameter theta0  : ",signif(pars$theta0,digits),"\n")
 cat("  Exposure parameter theta1   : ",signif(pars$theta1,digits),"\n")
 cat("  Improvement parameter theta2: ",signif(pars$theta2,digits),"\n")
 cat("\n")
 cat("Mean capture prob. in population with no removals\n")
 cat("                First occasion:", signif(meanp[1],digits),"\n")
 cat("                 Last occasion:", signif(meanp[K],digits),"\n")
 cat(paste("   Highest mean p (occasion ",max.meanp.occ,"):",sep=""), signif(max.meanp,digits),"\n")
 cat(paste("    Lowest mean p (occasion ",min.meanp.occ,"):",sep=""), signif(min.meanp,digits),"\n")
 cat("\n")
 cat("Number of sampling occasions    :", pars$design$number.occasions,"\n")
 cat("Sampling effort on each occasion:", pars$design$effort,"\n")
 cat("\n")
 cat("Region dimensions (length x width):", pars$design$region$length, "x", pars$design$region$width, "\n")

 if(plot) plot(pars)
}


plot.pars.survey.rm<-function(pars, type="p")
{
 if (!is.pars.survey.rm(pars)) 
   stop("\n*** The parameter <pars> must be of type 'pars.survey.rm'.\n")
 if(type!="p.dbn" && type!="p") 
   stop("\n *** The parameter <type> must be 'p.dbn' or 'p'.")
 x<-pars$population$exposure
 xp<-seq(pars$population$minexposure,pars$population$maxexposure,length=50)
 eff<-pars$des$effort
 th0<-pars$theta0
 th1<-pars$theta1
 th2<-pars$theta2
# calculate p's as funciton of exposure
 px<-detection.removalmethods(th0, th1, th2, xp, eff)
# calculate p's in population
 p<-detection.removalmethods(th0, th1, th2, x, eff)
# plotting parameters
 n.occ<-dim(px)[2]
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 old.par<-par(no.readonly=TRUE)
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 if(type=="p") {
   par(mfrow=c(1,1))
   # plot capture functions
   plot(xp,c(rep(0,(length(xp)-1)),max(px)),type="n", main="Capture function(s)", xlab="Exposure", ylab="Capture probability", ylim=c(0,1))
   for(j in 1:n.occ) {
     lines(xp,px[,j],col=j)
   }
   labels<-paste("Occasion",as.character(1:n.occ))
#   legend(max(xp),min(px),labels,lty=1,col=1:n.occ,xjust=1,yjust=0) 
    legend(max(xp),0,labels,lty=1,col=1:n.occ,xjust=1,yjust=0) 
  }
 if(type=="p.dbn") {
#  plot p distributions
   par(mfrow=c(2,2))
#   get breaks for p histograms, then add 0, 1 if neccessary:
   meanp<-apply(p,2,mean)
   high.occ<-which(meanp==max(meanp))[1]
   low.occ<-which(meanp==min(meanp))[1]
   hst<-hist(p[,1], main=paste("p distribution, first (occasion ",1,")",sep=""), xlab="Capture probability (red line is mean)", ,col=1)
   lines(c(meanp[1],meanp[1]),c(0,max(hst$counts)),col="red",lty=2)
   hst<-hist(p[,n.occ], main=paste("p distribution, last (occasion ",n.occ,")",sep=""), xlab="Capture probability (red line is mean)",col=n.occ)
   lines(c(meanp[n.occ],meanp[n.occ]),c(0,max(hst$counts)),col="red",lty=2)
   hst<-hist(p[,low.occ], main=paste("p distribution, lowest mean p (occasion ",low.occ,")",sep=""), xlab="Capture probability (red line is mean)", col=low.occ)
   lines(c(meanp[low.occ],meanp[low.occ]),c(0,max(hst$counts)),col="red",lty=2)
   hst<-hist(p[,high.occ], main=paste("p distribution, highest mean p (occasion ",high.occ,")",sep=""), xlab="Capture probability (red line is mean)", col=high.occ)
   lines(c(meanp[high.occ],meanp[high.occ]),c(0,max(hst$counts)),col="red",lty=2)
   par(old.par)
 }
}



#generate.sample.rm<-function (pars.survey.rm, seed=NULL) 
#{
# if (!is.pars.survey.rm(pars.survey.rm)) stop("\nThe parameter <pars.survey.rm> must be of type 'pars.survey.rm'.\n")
# if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) stop("\nThe parameter <seed> must be a number or a WiSP #object.\n")
# if (is.wisp.class(seed)) {
#   if (is.element("seed",names(seed))) {
#     if(!is.null(seed$seed)) {
#       seed<-seed$seed
#       set.seed(seed) # use seed stored in passed wisp object
#     }
#   }
# }
# if(is.numeric(seed)) set.seed(seed)
# pop <- pars.survey.rm$population
# des <- pars.survey.rm$design
# exposure <- pop$expos
# n.groups <- length(exposure)
# n.occ <- des$number.occasions
# theta0 <- pars.survey.rm$theta0
# theta1 <- pars.survey.rm$theta1
# theta2 <- pars.survey.rm$theta2
# effort <- rep(1, n.occ)
# p.detect <- detection.removalmethods(theta0 = theta0, theta1 = theta1, 
#     theta2 = theta2, exposure = exposure, effort = effort)
# removal <- matrix(0, nrow = n.groups, ncol = n.occ)
# detected <- removal
# for (i in 1:n.groups) {
#   det <- rbinom(n.occ, 1, p.detect[i])
#   if (any(det == 1)) {
#     detected[i, min((1:n.occ)[det == 1])] <- 1
#   }
# }
# for (s in 2:n.occ) removal[, s] <- detected[, (s - 1)]
# removal[, 1] <- 0
# parents<-list(wisp.id(pars.survey.rm,newname=as.character(substitute(pars.survey.rm))))
# samp<-list(population = pop, design = des, removal = removal, detected = detected, parents=parents, #created=date(),seed=seed)
# class(samp) <- "sample.rm"
# return(samp)
#}


# HERE'S AN UPDATE THAT ALLOWS DIFFERENT CAPTURE PROBS FOR DIFFERENT TYPES:
generate.sample.rm<-function (pars.survey.rm, seed=NULL) 
{
 if (!is.pars.survey.rm(pars.survey.rm)) stop("\nThe parameter <pars.survey.rm> must be of type 'pars.survey.rm'.\n")
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
 parents<-list(wisp.id(pars.survey.rm,newname=as.character(substitute(pars.survey.rm))))
    pop <- pars.survey.rm$population
    des <- pars.survey.rm$design
    exposure <- pop$expos
    n.groups <- length(exposure)
    n.occ <- des$number.occasions
    effort <- des$effort
    theta0 <- pars.survey.rm$theta0
    theta1 <- pars.survey.rm$theta1
    theta2 <- pars.survey.rm$theta2
    type.prob <- pars.survey.rm$type.prob
    separate.removal <- pars.survey.rm$separate.removal
    type.hetro <- pars.survey.rm$type.hetro
    p.detect <- detection.removalmethods(theta0=theta0, theta1=theta1, theta2=theta2, 
                                         exposure=exposure, effort=effort)
    detection <- matrix(0, nrow = n.groups, ncol = n.occ)
    removal <- matrix(0, nrow = n.groups, ncol = n.occ)
    typename<-sort(unique(pop$types))

    for (i in 1:n.groups) {
        if(!type.hetro)
          detected <- rbinom(n.occ, 1, p.detect[i,])
        else
          detected <- rbinom(n.occ, 1, p.detect[i,] *type.prob[which(typename==pop$types[i])])
        # CHANGED by Stefan Kirchfeld, 2002-07-26
        # if type.hetro == TRUE then use 'type.prob' but otherwise don't !
        # (used to always use 'type.prob' if 'seperate.removal' was TRUE)
        if(separate.removal) {
          if (type.hetro)
            removed <- c(0,rbinom((n.occ-1), 1, p.detect[i,]*type.prob[which(typename==pop$types[i])]))
          else
            removed <- c(0, rbinom((n.occ-1), 1, p.detect[i,])) 
        }
        else
          removed <- c(0,detected[1:(n.occ-1)])
        if (any(detected == 1)) {
            first.removal<- min((1:n.occ)[removed == 1],(n.occ+1))
            present<-c(1:n.occ)<first.removal
            detection[i,]<-(present & detected)*1
            if(first.removal<=n.occ) removal[i,first.removal] <- 1
        }
    }

    samp <- list(population = pop, design = des, removal = removal, detected=detection, type.hetro=type.hetro, type.prob=type.prob, parents=parents, created=date(),seed=seed)
    class(samp) <- "sample.rm"
    return(samp)
}



is.sample.rm <- function (samp) 
{
 inherits(samp, "sample.rm")
}


summary.sample.rm<-function (samp) 
{
 if (!is.sample.rm(samp)) stop("\nThe parameter <samp> must be of type 'sample.rm'.\n")
 cat("\n")
 cat("SAMPLE SUMMARY (REMOVAL METHODS)\n")
 cat("--------------------------------\n")
 cat("creation date   :", samp$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(samp$parents)) {
   cat("      ",paste("(",samp$parents[[i]]$class,", ",samp$parents[[i]]$name,", ",samp$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(samp$seed)) cat("random number seed used: ",samp$seed,"\n")
 cat("\n")
 typename <- unique(samp$population$types)
 if(!is.na(typename[1])) {
   typename<-sort(typename)
   n.types<-length(typename)
   namelen<-nchar(typename)
   maxnamelen<-max(namelen)
 } else {
   n.types<-1
 }
 n.occ<-samp$design$number.occasions
 R<-matrix(rep(0,n.types*n.occ),nrow=n.types)
 n<-R
 if(n.types>1) {
   for(i in 1:n.types) {
     R[i,] <- apply(samp$removal[samp$population$type==typename[i],], 2, sum)
     n[i,] <- apply(samp$detected[samp$population$type==typename[i],], 2, sum)
   }
 } else {
     R[1,] <- apply(samp$removal, 2, sum)
     n[1,] <- apply(samp$detected, 2, sum)
 }
for(i in 1:n.types) R[i,]<-cumsum(R[i,1:n.occ])
 cat("Number of survey occasions                    :", n.occ, "\n\n")
 cat("Total number of captured groups               :", sum(n), "\n")
 cat("Total number removed by start of each occasion:", apply(R,2,sum), "\n")
 if(n.types>1) { 
   for(i in 1:n.types) {
     blanks<-paste(rep(" ",(maxnamelen-namelen[i])),collapse="")
     cat(paste("Number ",blanks,typename[i]," removed by start of each occasion:",sep=""), R[i,], "\n")
   }
 }
 cat("Total number captured on each occasion        :", apply(n,2,sum), "\n")
 if(n.types>1) { 
   for(i in 1:n.types) {
     blanks<-paste(rep(" ",(maxnamelen-namelen[i])),collapse="")
     cat(paste("Number ",blanks,typename[i]," captured on each occasion        :",sep=""), n[i,], "\n")
   }
 }
}


plot.sample.rm<-function (samp, which.occasion=0, show.sizes=T, show.exps=T, dsf=1, whole.population=FALSE) 
{
    if (!is.sample.rm(samp)) stop(paste("\n*** The parameter <samp> must be of class 'sample.rm'.\n"))
    pop <- samp$population
    des <- samp$design
    n.occ <- des$number.occasions
    if (!is.numeric(which.occasion)) stop("\nThe parameter <which.occasion> must be numeric.\n")
    if (which.occasion != as.integer(which.occasion)) 
        stop("\nThe parameter <which.occasion> must be of integer type.\n")
    if (which.occasion < 0) 
        stop("\nThe parameter <which.occasion> must be at minimum zero.\n")
    if (which.occasion > n.occ) 
        stop(paste("\nThe parameter <which.occasion> cannot be greater than", "the number of survey occasions.\n"))
    if (!is.numeric(dsf)) stop("\nThe parameter <dsf> must be numeric.\n")
    if (dsf <= 0) stop("\nThe parameter <dsf> must be positive.\n")
    if ((show.sizes != T) & (show.sizes != F)) 
        stop("\nThe parameter <show.sizes> must be TRUE or FALSE.\n")
    if ((show.exps != T) & (show.exps != F)) 
        stop("\nThe parameter <show.exps> must be TRUE or FALSE.\n")
    if ((whole.population != T) & (whole.population != F)) 
        stop("\nThe parameter <whole.population> must be TRUE or FALSE.\n")
    par.was <- par(no.readonly = T)
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 plot(pop$region, reset.pars=FALSE)
    if (whole.population == T) 
        plot.groups(pop, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="black")
    if (which.occasion == 0) {
        i.min <- 1
        i.max <- n.occ
    }
    else {
        i.min <- which.occasion
        i.max <- which.occasion
    }
    for (i in i.min:i.max) {
        inside <- (samp$detected[, i] == 1)
        if (any(inside)) {
            seen <- pop
            seen$groupID <- pop$groupID[inside]
            seen$posx <- pop$posx[inside]
            seen$posy <- pop$posy[inside]
            seen$groupsize <- pop$groupsize[inside]
            seen$types <- pop$types[inside]
            seen$exposure <- pop$exposure[inside]
             plot.groups(seen, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="red")
        }
    }
    par(new = par.was$new)
}


obscure.sample.rm<-function(samp)
#----------------------------------------------------------------
# Removes all information about undetected animals from an object
# of class sample.rm. Returns object of same class.
#----------------------------------------------------------------
{
if (!is.sample.rm(samp)) 
  stop("\n*** <samp> is not an object of type 'sample.rm'.\n")
t<-samp
# mark all deteceted animals
detected<-apply(samp$detected,1,sum)
# then filter out all information about others
t$population$groupID<-samp$population$groupID[detected==1]
t$population$posx<-samp$population$posx[detected==1]
t$population$posy<-samp$population$posy[detected==1]
t$population$groupsize<-samp$population$groupsize[detected==1]
t$population$types<-samp$population$types[detected==1]
t$population$exposure<-samp$population$exposure[detected==1]
t$removal<-samp$removal[(detected==1),]
t$detected<-samp$detected[(detected==1),]
t$created<-date()
t
}


point.est.rm=function (samp, numerical = TRUE, plot = FALSE) 
{
  if (!is.sample.rm(samp)) 
      stop("\n*** Argument <samp> must be an object of type 'sample.rm'.\n")
  seen <- apply(samp$detected, 1, sum) > 0
  rs <- apply(samp$removal, 2, sum)
  ns <- apply(samp$detected, 2, sum)
  nall <- sum(ns)
  if(nall==0) {
    parents <- list(wisp.id(samp, newname=as.character(substitute(samp))))
    Nhat.grp=0; Nhat.ind=0
    pointest <- list(sample=samp, numerical=numerical, Nhat.grp=Nhat.grp, Nhat.ind=Nhat.ind, phat=NA, Es=NA, log.Likelihood=NA, 
        AIC=NA, parents=parents, created=date())
    warning("No captures for estimating Nhat by removal method; set Nhat to 0")  
  }else {
    n.occ <- length(ns)
    Es <- mean(samp$population$groupsize[seen])
    log.Likelihood <- NA
    if (n.occ > 2 & !numerical) 
        stop("Explicit estimator only available for 2 capture occasions. Use numerical=TRUE")
    if (numerical == F) {
        if (ns[1] > ns[2]) {
            Nhat.grp <- ns[1]^2/(ns[1] - ns[2])
            Nhat.ind <- Nhat.grp * Es
            phat <- (ns[1] - ns[2])/ns[1]
            llk <- function(x) {
                N <- exp(x[1]) + nall
                p <- exp(x[2])/(1+exp(x[2])) # logit transformatin to keep p in (0,1)
                Ns <- N - cumsum(rs)
                temp1 <- lgamma(Ns + 1) - lgamma(Ns - ns + 1) - 
                  lgamma(ns + 1)
                temp2 <- ns * log(p)
                temp3 <- (Ns - ns) * log(1 - p)
                llk <- -sum(temp1 + temp2 + temp3)
                return(llk)
            }
            transform.Nthetatox <- function(x) {
                x <- c(log(x[1] - nall), log(x[2]/(1-x[2]))) # logit transformatin to keep p in (0,1)
                return(x)
            }
            Nthetahat <- c(Nhat.grp, phat)
            x <- transform.Nthetatox(Nthetahat)
            log.Likelihood <- -llk(x)
        }
        else {
            Nhat.grp <- -1
            Nhat.ind <- -1
            phat <- -1
        }
    }
    else {
        llk <- function(x) {
            N <- exp(x[1]) + nall
            p <- exp(x[2])/(1+exp(x[2])) # logit transformatin to keep p in (0,1)
            Ns <- N - cumsum(rs)
            temp1 <- lgamma(Ns + 1) - lgamma(Ns - ns + 1) - lgamma(ns + 1)
            temp2 <- ns * log(p)
            temp3 <- (Ns - ns) * log(1 - p)
            llk <- -sum(temp1 + temp2 + temp3)
            return(llk)
        }
        transform.xtoNtheta <- function(x) {
            N <- exp(x[1]) + nall
            theta <- exp(x[2])/(1+exp(x[2])) # logit transformatin to keep p in (0,1)
            return(c(N, theta))
        }
        transform.Nthetatox <- function(x) {
            x <- c(log(x[1] - nall), log(x[2]/(1-x[2]))) # logit transformatin to keep p in (0,1)
            return(x)
        }
        if (n.occ == 2) 
            startNtheta <- c(ns[1]^2/(ns[1] - ns[2]), max((ns[1] - ns[2])/ns[1],.Machine$double.xmin))
#        startNtheta <- c(((1 + 1/n.occ) * nall), max((ns[1]/nall),.Machine$double.xmin)) # old code; changed to line below, to make Nhat and p consistent
        else startNtheta <- c(((1 + 1/n.occ) * nall), max((1/(1 + 1/n.occ)),.Machine$double.xmin))
        startx <- transform.Nthetatox(startNtheta)
        res <- suppressWarnings(nlm(llk, startx))  ###  MM June 2007
        Nthetahat <- transform.xtoNtheta(res$estimate)
        Nhat.grp <- Nthetahat[1]
        Nhat.ind <- Nhat.grp * Es
        phat <- Nthetahat[2]
        log.Likelihood <- -res$minimum
    }
    AIC <- -2 * log.Likelihood + 2 * length(phat)
    parents <- list(wisp.id(samp, newname=as.character(substitute(samp))))
    pointest <- list(sample = samp, numerical = numerical, Nhat.grp = Nhat.grp, 
        Nhat.ind = Nhat.ind, phat = phat, Es = Es, log.Likelihood = log.Likelihood, 
        AIC = AIC, parents = parents, created = date())
  }
  class(pointest) <- "point.est.rm"
  if (plot & Nhat.grp > 0) 
      plot(pointest)
  return(pointest)
}


is.point.est.rm<-function(est)
{
 inherits(est, "point.est.rm")
}


summary.point.est.rm<-function(est, digits=5) 
{
 if (!is.point.est.rm(est)) stop("\nThe parameter <est> must be of class 'point.est.rm'.\n")
 cat("\n")
 cat("POINT ESTIMATE SUMMARY (SIMPLE REMOVAL METHOD)\n")
 cat("----------------------------------------------\n")
 cat("creation date   :", est$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(est$parents)) {
   cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(est$seed)) cat("random number seed used: ",est$seed,"\n")
 cat("\n")
 cat("Numerical estimation method? :",est$numerical,"\n")
 cat("\n")
 n.occ<-est$sample$design$number.occasions
 R <- apply(est$sample$removal, 2, sum)
 R<-cumsum(R)
 n <- apply(est$sample$detected, 2, sum)
 cat("Number of survey occasions                   :", n.occ, "\n\n")
 cat("Total number of captured groups              :", sum(n), "\n")
 cat("Number removed by start of each occasion     :", R, "\n")
 cat("Number captured on each occasion             :", n, "\n")
 cat("\n")
 cat("Estimated capture probability (each occasion):", signif(est$phat,digits), "\n")
 cat("Estimated number of groups                   :", round(est$Nhat.grp), "\n")
 cat("Estimated number of individuals              :", round(est$Nhat.ind), "\n")
 cat("Estimated mean group size                    :", signif(est$Es,digits), "\n")
 cat("\n")
 cat("log(Likelihood)                              :", est$log.Likelihood, "\n")
 cat("AIC                                          :", est$AIC, "\n")
}


plot.point.est.rm<-function(est, new=TRUE, plot.N=TRUE, within.N=0.01)
{
 if (!is.point.est.rm(est)) stop("\nThe parameter <est> must be of class 'point.est.rm'.\n")
 N <- est$Nhat.grp #(moved up)
 if(N<=0) {
   warning("Removal method estimate Nhat<=0. No plot created")
   return()
 }
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 old.par<-par(no.readonly=TRUE)
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 p<-est$phat
 R<-cumsum(apply(as.matrix(est$sample$removal[,2:length(est$sample$removal[1,])]), 2, sum))
 last.n<-sum(est$sample$detected[,length(est$sample$detected[1,])])
 R<-c(R,R[length(R)]+last.n)
 smax<- -log(within.N)/p
 big<-FALSE
 if(smax>100) {
   big<-TRUE
   smax<-100
 }
 occ <- c(1:(smax+1))
 if(N>100*R[length(R)]) big<-TRUE
 ER <- cumsum(N * p * (1 - p)^(occ - 1))
 if(big) ymax<-max(ER)
 else    ymax<-max(ER, N)
 if (new) plot(c(0,occ), c(0,ER), ylim = c(0, ymax), type = "n", xlab = "Removal occasion", 
            ylab = "Cumulative removals")
 lines(c(0,occ), c(0,ER))
 if (plot.N) {
   if(!big) {
     lines(occ, rep(N, length(occ)), lty = 2, col = "blue")
     text(1, N, as.character(round(N)), cex = 0.8, col = "blue", pos = 1, offset = -0.02)
   } else {
     text(1, max(ER), paste("N estimate: ",as.character(round(N)),sep=""), cex = 0.8, col = "blue", pos = 4, offset = -0.02)
   }
 }
 R<-cumsum(apply(as.matrix(est$sample$removal[,2:length(est$sample$removal[1,])]), 2, sum))
 last.n<-sum(est$sample$detected[,length(est$sample$detected[1,])])
 R<-c(R,R[length(R)]+last.n)
# points(1:(dim(est$sample$removal)[2]-1), R, pch = 19)
 points(1:(dim(est$sample$removal)[2]), R, pch = 19)
 points(0,0,pch=1)
 par(old.par)
}


point.est.ce<-function (samp, plot = FALSE) 
{
    if (!is.sample.rm(samp) && !samp$bootsample.rm) 
        stop("\n*** Argument <samp> must be an object of type 'sample.rm'.\n")
    if (is.sample.rm(samp)) {
        effort <- samp$design$effort
        seen <- apply(samp$detected, 1, sum) > 0
        rs <- apply(samp$removal, 2, sum)
        ns <- apply(samp$detected, 2, sum)
        nall <- sum(ns)
        n.occ <- length(ns)
        Es <- mean(samp$population$groupsize[seen])
    }
    else if (!is.null(samp$bootsample.ce) && samp$bootsample.ce) {
        effort <- samp$effort
        Es <- 1
        rs <- samp$rs
        ns <- samp$ns
        nall <- sum(ns)
        n.occ <- length(ns)
    }
    log.Likelihood <- NA
    llk <- function(x) {
        N <- exp(x[1]) + nall
        theta <- exp(x[2])
        Ns <- N - cumsum(rs)
        ps <- 1 - exp(-theta * effort)
        temp1 <- lgamma(Ns + 1) - lgamma(Ns - ns + 1) - lgamma(ns + 
            1)
        temp2 <- ns * log(ps)
        temp3 <- (Ns - ns) * log(1 - ps)
        llk <- -sum(temp1 + temp2 + temp3)
        return(llk)
    }
    transform.xtoNtheta <- function(x) {
        N <- exp(x[1]) + nall
        theta <- exp(x[2])
        return(c(N, theta))
    }
    transform.Nthetatox <- function(x) {
        x <- c(log(x[1] - nall), log(x[2]))
        return(x)
    }
    startNtheta <- c(((1 + 1/n.occ) * nall), (ns[1]/nall))
    startx <- transform.Nthetatox(startNtheta)
    res <- suppressWarnings(nlm(llk, startx))
    Nthetahat <- transform.xtoNtheta(res$estimate)
    Nhat.grp <- Nthetahat[1]
    theta <- Nthetahat[2]
    log.Likelihood <- -res$minimum
    AIC <- -2 * log.Likelihood + 2 * length(theta)
    pshat <- 1 - exp(-theta * effort)
    Nhat.grp <- round(Nhat.grp)
    Nhat.ind <- round(Nhat.grp * Es)
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
 pointest <- list(sample=samp, Nhat.grp = Nhat.grp, Nhat.ind = (Nhat.grp*Es), theta = theta, phat = pshat, 
             Es = Es, log.Likelihood = log.Likelihood, AIC = AIC, parents=parents, created=date())
    class(pointest) <- "point.est.ce"
    if (plot) plot(pointest)
    return(pointest)
}

is.point.est.ce<-function(est)
{
 inherits(est, "point.est.ce")
}

summary.point.est.ce<-function(est, digits=5) 
{
 if (!is.point.est.ce(est)) stop("\nThe parameter <est> must be of class 'point.est.ce'.\n")
 cat("\n")
 cat("POINT ESTIMATE SUMMARY (CATCH-EFFORT METHOD)\n")
 cat("--------------------------------------------\n")
 cat("creation date   :", est$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(est$parents)) {
   cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(est$seed)) cat("random number seed used: ",est$seed,"\n")
 cat("\n")
 n.occ<-est$sample$design$number.occasions
 R <- apply(est$sample$removal, 2, sum)
 n <- apply(est$sample$detected, 2, sum)
 cat("Number of survey occasions                    :", n.occ, "\n\n")
 cat("Total number of captured groups               :", sum(n), "\n")
 cat("Number removed by start of each occasion      :", R, "\n")
 cat("Number captured on each occasion              :", n, "\n")
 cat("\n")
 cat("Capture function model:\n")
 cat("    p(detect) = exp(-theta * effort)\n")
 cat("    Parameters: \n")
 cat("      theta = ", signif(sqrt(est$theta[1]), digits=digits), "\n")  # changed $thetahat to $theta[1]
 cat("Effort used on each occasion                  :", signif(est$sample$design$effort,digits), "\n")
 cat("Estimated capture probability on each occasion:", signif(est$phat,digits), "\n")
 cat("\n")
 cat("Estimated number of groups                    :", round(est$Nhat.grp), "\n")
 cat("Estimated number of individuals               :", round(est$Nhat.ind), "\n")
 cat("Estimated mean group size                     :", signif(est$Es,digits), "\n")
 cat("\n")
 cat("log(Likelihood)                               :", est$log.Likelihood, "\n")
 cat("AIC                                           :", est$AIC, "\n")
}

plot.point.est.ce<-function(est, new=TRUE, plot.N=TRUE, within.N=0.005)
{
 if (!is.point.est.ce(est)) stop("\nThe parameter <est> must be of class 'point.est.ce'.\n")
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 old.par<-par(no.readonly=TRUE)
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 cumeff<-cumsum(est$sample$design$effort)
 emax<-max(c(-log(within.N)/est$theta, cumeff))  # changed $thetahat to $theta[1]
 effort <- seq(0, emax, length = 50)
 N<-est$Nhat.grp
 ER<-N * (1-exp(-est$theta*effort))
 if (new) plot(effort, ER, ylim = c(0, max(ER, N)), type = "n", 
          xlab = "Cumulative effort", ylab = "Cumulative removals")
 lines(effort, ER)
 if (plot.N) {
    lines(effort, rep(N, length(effort)), lty = 2, col = "blue")
          text(0, N, as.character(round(N)), cex = 0.8, col = "blue", 
          pos = 1, offset = -0.02)
 }
 points(cumeff, cumsum(apply(est$sample$detected, 2, sum)), pch = 19)
 points(0, 0)
 par(old.par)
}


point.est.cir<-function (samp, numerical=TRUE) # changed from est.type="numerical" to numerical=TRUE for consistency
{
 if (is.null(samp$bootsample.rm) || !samp$bootsample.rm) {
   if (!is.sample.rm(samp)) 
     stop("\nThe parameter <samp> must be of type 'sample.rm'.\n")
 }
 if (!is.logical(numerical)) 
   stop("\nThe parameter <numerical> must be 'TRUE' or 'FALSE'.\n")
 if (samp$population$ntypes<=1) 
   stop("\nThe population must contain at least 2 types in order to use the change-in-ratio method.\n")
 if (!numerical) est <- estimate.cir.analytic(samp)
 if (numerical) est <- estimate.cir.numerical(samp)
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
 est$parents<-parents; est$created<-date(); est$numerical=numerical
 return(est)
}


# PROBLEM: It looks like the analytic estimator MUST always give Nhat=number removed on first occasion
# SOMETHING ODD/WRONG HERE!
# Usually, removal method implemented when removals are not first sample, maybe that is the problem?
# I think it's the fact that both types are removed with equal probability that causes the problem- 
# need to hit one type of animal hard to get contrast needed to estimate N?
estimate.cir.analytic<-function (samp, ci.type = "normal", vlevels = c(0.025, 0.975), 
    sampling = "binom", with.ci = FALSE) 
{
    type <- sort(unique(samp$population$types))
    if (length(type) != 2) 
        stop("\n *** Can't use analytic estimation method unless there are exactly 2 types\n")
    if (samp$design$number.occasions != 2) 
        stop("\n *** Can't use analytic estimation method unless there are exactly 2 survey occasions\n")
    if (sampling != "hypergeom" & sampling != "Hypergeom" & sampling != 
        "binom" & sampling != "Binom") 
        stop("\n*** Parameter <sampling> must be hypergeom, Hypergeom, binom, or Binom\n")
    n <- matrix(rep(0, 4), nrow = 2, ncol = 2)
    R <- n
    seen <- apply(samp$detected, 1, sum) > 0
    Es <- mean(samp$population$groupsize[seen])
    for (typ in 1:2) {
        n[, typ] <- apply(samp$detected[samp$population$types == type[typ], ], 2, sum)
        R[, typ] <- apply(samp$removal[samp$population$types == type[typ], ], 2, sum)
    }
    pizero <- c(1, 1)
    for (occ in 1:2) pizero[occ] <- n[occ, 1]/sum(n[occ, ])
    if ((pizero[1] - pizero[2]) == 0) {
        stop("\n*** Cannot compute an estimator: Denominator 'p_1(1) - p_2(1)' is zero! ***")
    }
    Nhat <- (R[2, 1]-sum(R[2, ])*pizero[2])/(pizero[1]-pizero[2])
    if (Nhat < 0) Nhat <- Inf
    total.detected <- sum(R[2, ]) + sum(samp$detected[, 2])
    if (Nhat < total.detected) {
        warning("*** The estimator Nhat was smaller than the total number of detected animals! (It has been reset to equal the number detected) ***\n")
        Nhat<-total.detected
    }
    if (with.ci) {
        seNhat <- c(NA, NA)
        CI <- seNhat
        if (ci.type == "normal") {
            Varpi <- c(1, 1)
            Ns <- c(Nhat, Nhat - sum(R[2, ]))
            for (occ in 1:2) {
                if (sampling == "hypergeom" | sampling == "Hypergeom") 
                  Varpi[occ] <- (pizero[occ] * (1 - pizero[occ])) * 
                    (1 - sum(n[occ, ])/Ns[occ])/(sum(n[occ, ]) - 
                    1)
                else Varpi[occ] <- (pizero[occ] * (1 - pizero[occ]))/sum(n[occ, 
                  ])
            }
            VarNhat <- sum((Ns^2) * Varpi)/(pizero[1] - pizero[2])^2
            CI <- Nhat + qnorm(vlevels) * sqrt(VarNhat)
            if (CI[1] < 0) 
                CI[1] <- 0
            CI <- round(CI)
        }
        ci <- list(Nhat.grp = CI)
        intest <- list(levels = vlevels, ci = ci, boot.mean = NA, 
            boot.dbn = NA, seNhat = sqrt(VarNhat))
        class(intest) <- "int.est.cir"
        return(intest)
    }
    else {
      pointest <- list(sample=samp, Nhat.grp=Nhat, Nhat.ind=Nhat*Es, theta=NA, phat=NA, Es=Es, 
                     Nshats=Nhat*pizero, log.Likelihood=NA, AIC=NA) # changed thetahat to theta
      class(pointest) <- "point.est.cir"
      return(pointest)
    }
}


estimate.cir.numerical<-function(samp) 
{
    name.types <- sort(unique(samp$population$types))
    n.types <- length(name.types)
    tech.effort <- samp$design$effort
    n.occ <- length(tech.effort)
    samp.types <- samp$population$types
    seen <- apply(samp$detected, 1, sum) > 0
    Es <- mean(samp$population$groupsize[seen])
    if (is.sample.rm(samp)) {
        rem <- samp$removal
        det <- samp$detected
        type.array.det <- array(rep(0, (nrow(det)*ncol(det)*n.types)), c(nrow(det), ncol(det), n.types))
        type.array.rem <- type.array.det
        for (j in 1:n.types) {
            target <- (samp.types == name.types[j])
            type.array.det[target, , j] <- det[target, ]
            type.array.rem[target, , j] <- rem[target, ]
        }
        llk <- function(x) {
            llk.value <- rep(0, n.types)
            Ns <- rep(0, n.types)
            for (i in 1:n.types) {
                ns <- apply(type.array.det[, , i], 2, sum)
                n.all <- sum(type.array.det[, , i])
                rs <- apply(type.array.rem[, , i], 2, sum)
                RS <- sum(type.array.rem[, , i])
                n.occ <- length(ns)
                Ns[i] <- exp(x[i]) + RS
                teta <- exp(x[n.types + 1])
                nav <- Ns[i] - cumsum(rs)
                pls <- 1 - exp(-teta * tech.effort)
                temp1 <- lgamma(nav + 1) - lgamma(nav - ns + 
                  1) - lgamma(ns + 1)
                temp2 <- ns * log(pls)
                temp3 <- (nav - ns) * log(1 - pls)
                llk.value[i] <- -sum(temp1 + temp2 + temp3)
            }
            return(sum(llk.value))
        }
    }
    else if (!is.null(samp$bootsample.rm) && samp$bootsample.rm) {
        type.array.det <- samp$detect
        type.array.rem <- samp$removed
        llk <- function(x) {
            llk.value <- rep(0, n.types)
            Ns <- rep(0, n.types)
            for (i in 1:n.types) {
                ns <- type.array.det[, , i]
                n.all <- sum(type.array.det[, , i])
                rs <- type.array.rem[, , i]
                RS <- sum(type.array.rem[, , i])
                n.occ <- length(ns)
                Ns[i] <- exp(x[i]) + RS
                teta <- exp(x[n.types + 1])
                nav <- Ns[i] - cumsum(rs)
                pls <- 1 - exp(-teta * tech.effort)
                temp1 <- lgamma(nav + 1) - lgamma(nav - ns + 
                  1) - lgamma(ns + 1)
                temp2 <- ns * log(pls)
                temp3 <- (nav - ns) * log(1 - pls)
                llk.value[i] <- -sum(temp1 + temp2 + temp3)
            }
            return(sum(llk.value))
        }
    }
    transform.xtoNsteta <- function(x) {
        Ns <- rep(0, n.types)
        for (i in 1:n.types) {
            Ns[i] <- exp(x[i]) + sum(type.array.rem[, , i])
        }
        teta <- exp(x[n.types + 1])
        return(c(Ns, teta))
    }
    transform.Nstetatox <- function(Nsteta) {
        x <- rep(0, n.types + 1)
        for (i in 1:n.types) {
            x[i] <- log(Nsteta[i] - sum(type.array.rem[, , i]))
        }
        x[n.types + 1] <- log(Nsteta[n.types + 1])
        return(x)
    }
    startNsteta <- rep(0, n.types + 1)
    for (i in 1:n.types) {
        startNsteta[i] <- (1 + 1/n.occ) * sum(type.array.det[, , i])
    }
    startNsteta[n.types + 1] <- 1/tech.effort[1]
    startx <- transform.Nstetatox(startNsteta)
    res <- suppressWarnings(nlm(llk, startx))  ###  MM June 2007
    xhat <- res$estimate
    Nstetahat <- transform.xtoNsteta(xhat)
    Nhat <- round(Nstetahat[1:n.types])
    thetahat <- Nstetahat[n.types + 1]
    pshat <- 1 - exp(-thetahat * tech.effort)
    Nhat.grp<-sum(Nhat[1:n.types])
#   need to deal with Nhat.grp's that are less than number detected!
    typename <- sort(unique(samp$population$types))
    n.types <- length(typename)
    R<-matrix(rep(0,n.types*n.occ),nrow=n.types)
    n<-R
    for(i in 1:n.types) {
      R[i,] <- apply(samp$removal[samp$population$type==typename[i],], 2, sum)
      n[i,] <- apply(samp$detected[samp$population$type==typename[i],], 2, sum)
    }
    total.detected<-sum(n)
    if (Nhat.grp < total.detected) {
        warning("*** The estimator Nhat was smaller than the total number of detected animals! (It has been reset to equal the number detected) ***\n")
        Nhat.grp<-total.detected
    }
    Nhat.ind <- Nhat.grp * Es
    log.Likelihood <- -res$minimum
    AIC <- -2 * log.Likelihood + 2 * length(thetahat)
    pointest <- list(sample=samp, Nhat.grp=Nhat.grp, Nhat.ind=Nhat.grp*Es, theta = thetahat, phat = pshat, Es=Es, Nshats = round(Nhat), log.Likelihood=log.Likelihood, AIC=AIC) # changed thetahat to theta
    class(pointest) <- "point.est.cir"
    return(pointest)
}


is.point.est.cir<-function(est)
{
 inherits(est, "point.est.cir")
}



summary.point.est.cir<-function(est, digits=5) 
{
 if (!is.point.est.cir(est)) stop("\nThe parameter <est> must be of class 'point.est.cir'.\n")
 cat("\n")
 cat("POINT ESTIMATE SUMMARY (CHANGE-IN-RATIO METHOD)\n")
 cat("-----------------------------------------------\n")
 cat("creation date   :", est$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(est$parents)) {
   cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(est$seed)) cat("random number seed used: ",est$seed,"\n")
 cat("\n")
 typename <- sort(unique(est$sample$population$types))
 namelen<-nchar(typename)
 maxnamelen<-max(namelen)
 n.types <- length(typename)
 n.occ<-est$sample$design$number.occasions
 R<-matrix(rep(0,n.types*n.occ),nrow=n.types)
 n<-R
 for(i in 1:n.types) {
   R[i,] <- apply(est$sample$removal[est$sample$population$type==typename[i],], 2, sum)
   n[i,] <- apply(est$sample$detected[est$sample$population$type==typename[i],], 2, sum)
 }
 cat("Numerical estimation method? :",est$numerical,"\n")
 cat("\n")
 cat("Number of survey occasions                    :", n.occ, "\n\n")
 cat("Total number of captured groups               :", sum(n), "\n")
 cat("Total number removed by start of each occasion:", apply(R,2,sum), "\n")
 for(i in 1:n.types) {
   blanks<-paste(rep(" ",(maxnamelen-namelen[i])),collapse="")
   cat(paste("Number ",blanks,typename[i]," removed by start of each occasion:",sep=""), R[i,], "\n")
 }
 cat("Total number captured on each occasion        :", apply(n,2,sum), "\n")
 for(i in 1:n.types) {
   blanks<-paste(rep(" ",(maxnamelen-namelen[i])),collapse="")
   cat(paste("Number ",blanks,typename[i]," captured on each occasion        :",sep=""), n[i,], "\n")
 }
 cat("\n")
 cat("Capture function model:\n")
 cat("    p(detect) = exp(-theta * effort)\n")
 cat("    Parameters: \n")
 cat("      theta = ", signif(sqrt(est$theta[1]), digits=digits), "\n")  # changed $thetahat to $theta[1]
 cat("Effort used on each occasion                  :", signif(est$sample$design$effort,digits), "\n")
 if(est$numerical) cat("Estimated capture probability on each occasion:", signif(est$phat,digits), "\n")
 cat("\n")
 cat("Estimated number of groups                    :", round(est$Nhat.grp), "\n")
 cat("Estimated number of groups by type            :", paste(typename,round(est$Nshats),sep=":"), "\n")
 cat("Estimated number of individuals               :", round(est$Nhat.ind), "\n")
 cat("Estimated number of individuals by type       :", paste(typename,round(est$Nshats*est$Es),sep=":"), "\n")
 cat("Estimated mean group size                     :", signif(est$Es,digits), "\n")
 cat("\n")
 cat("log(Likelihood)                               :", est$log.Likelihood, "\n")
 cat("AIC                                           :", est$AIC, "\n")
}

int.est.rm<-function (samp, ci.type="boot.nonpar", nboot=999, vlevels=c(0.025,0.975), numerical=TRUE, plot=FALSE, seed=NULL, ...) 
# NOTE: Profile likelihood CI does not seem to be working
{
 n.occ<-samp$design$number.occasions
 if (n.occ > 2 & !numerical) stop("Explicit estimator only available for 2 capture occasions. Use numerical=TRUE")
 if (!is.sample.rm(samp)) stop("\n*** <samp> is not an object of type 'sample.rm'.\n")
# profile likelihood does not seem to be working:
# if (!(ci.type %in% c("boot.par", "boot.nonpar", "profile"))) 
#     stop(paste("\n*** Invalid <ci.type>. These are implemented:", "'boot.par', 'boot.nonpar', and 'profile' \n"))
 if (!(ci.type %in% c("boot.par", "boot.nonpar"))) 
     stop(paste("\n*** Invalid <ci.type>. These are implemented:", "'boot.par' and 'boot.nonpar' \n"))
 if (!is.numeric(vlevels)) stop("\n*** All <vlevels> values must be numeric.\n")
 if (any(vlevels < 0) | any(vlevels > 1)) stop("\n*** All <vlevels> values must be between 0 and 1.\n")
 if (!is.numeric(nboot)) stop("\n*** <nboot> must be numeric.\n")
 if (nboot < 2) stop("\n*** <nboot> must be at least 2.\n")
 if ((plot != T) & (plot != F)) stop("\n*** <plot> must be TRUE or FALSE.\n")
 if (ci.type != "boot.par" & ci.type != "boot.nonpar" & ci.type != "profile") plot <- FALSE
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
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
 dets <- samp$detected
 rems <- samp$removal
 rs <- apply(rems, 2, sum)
 ns <- apply(dets, 2, sum)
 nn <- sum(ns)
 n.occ <- length(ns)
 res <- point.est.rm(samp, numerical=numerical)
 Nhat <- round(res$Nhat.grp)
 phat <- res$phat
 maxest <- 100 * Nhat
 Es <- res$Es
 seen <- apply(dets, 1, sum) > 0
 groupsize <- samp$population$groupsize[seen]
 civec <- rep(NA, length(vlevels))
 ci <- list(Nhat.grp = civec, Nhat.ind = civec, phat = civec, Es = civec)
 boot.dbn <- NULL
 boot.mean <- NULL
 if (ci.type == "profile") {
   if (n.occ != 2) {
     stop("Profile likelihood CI can currently only cope with 2 capture occasions.")
   }
   else if (Nhat >= 0) {
     if (abs(vlevels[1] - (1 - vlevels[length(vlevels)])) > 1e-07) 
        stop("For profile likelihood, you need symmetric vlevels (e.g. vlevels=c(0.025,0.975) for a 95% CI)")
     llk <- function(N, p) {
        Ns <- rep(N, 2) - rs
        temp1 <- lgamma(Ns + 1) - lgamma(Ns - ns+1) - lgamma(ns + 1)
        temp2 <- ns * log(p)
        temp3 <- (Ns - ns) * log(1 - p)
        llk <- -sum(temp1 + temp2 + temp3)
        return(llk)
     }
     llkmin <- llk(Nhat, phat)
     W <- 0
     N <- Nhat
     conf.level <- vlevels[length(vlevels)] - vlevels[1]
     q <- qchisq(conf.level, 1)/2
     while (W < q) {
       p <- (nn)/(2 * N - ns[1])
       W <- llk(N, p) - llkmin
       if (W < q) N <- N - 1
     }
     Nlo <- N
     W <- 0
     N <- Nhat
     while (W < q) {
       p <- (nn)/(2 * N - ns[1])
       W <- llk(N, p) - llkmin
       if (W < q) 
       N <- N + 1
     }
     Nhi <- N
     ci$Nhat.grp[1] <- Nlo
     ci$Nhat.grp[length(vlevels)] <- Nhi
   }
 }
 if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
   b.Nhat.grp <- rep(0, nboot)
   b.Nhat.ind <- rep(0, nboot)
   b.phat <- rep(0, nboot)
   b.Es <- rep(0, nboot)
   b.samp <- samp
 }
 if (ci.type == "boot.nonpar") {
   rem <- rems[seen, ]
   det <- dets[seen, ]
   n0 <- Nhat - nrow(det)
   size <- c(samp$population$groupsize[seen], rep(0, n0))
   zero <- matrix(rep(0, n0 * ncol(rem)), ncol = ncol(rem))
   rem <- matrix(c(t(rem), zero), ncol = ncol(rem), byrow = T)
   det <- matrix(c(t(det), zero), ncol = ncol(rem), byrow = T)
   for (i in 1:nboot) {
     repeat {
       index <- sample(1:Nhat, Nhat, replace = T)
       b.samp$removal <- rem[index, ]
       b.samp$detected <- det[index, ]
       b.samp$population$groupsize <- size[index]
       est <- point.est.rm(b.samp, numerical=numerical)
       b.Nhat.grp[i] <- est$Nhat.grp
       b.Nhat.ind[i] <- est$Nhat.ind
       b.phat[i] <- est$phat
       b.Es[i] <- est$Es
       if (est$Nhat.grp < maxest) break
     }
   }
 }
 if (ci.type == "boot.par") {
   for (i in 1:nboot) {
     repeat {
       det <- matrix(0, nrow = Nhat, ncol = n.occ)
       rem <- det
       boot.result <- rbinom(n.occ * Nhat, 1, phat)
       saw <- matrix(boot.result, nrow = Nhat, ncol = n.occ)
       for (i in 1:Nhat) if (any(saw[i, ] == 1)) det[i, min((1:n.occ)[det == 1])] <- 1
       for (s in 2:n.occ) rem[, s] <- detected[, (s - 1)]
       rem[, 1] <- 0
       b.samp$removal <- rem
       b.samp$detected <- det
       est <- point.est.rm(b.samp, numerical=numerical)
       b.Nhat.grp[i] <- est$Nhat.grp
       b.Nhat.ind[i] <- est$Nhat.ind
       b.phat[i] <- est$phat
       b.Es[i] <- NA
       if (est$Nhat.grp < maxest) break
     }
   }
 }
 if (ci.type == "boot.par" | ci.type == "boot.nonpar") {
   boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, phat = b.phat, Es = b.Es)
   valid<-(b.Nhat.grp != Inf & boot.dbn[[1]] > 0)
   boot.mean <- list(Nhat.grp=mean(b.Nhat.grp[valid]), Nhat.ind=mean(b.Nhat.ind[valid]), 
            phat=mean(b.phat[valid]), Es = mean(b.Es[valid]))
   se <- list(Nhat.grp=sqrt(var(b.Nhat.grp[valid])), Nhat.ind=sqrt(var(b.Nhat.ind[valid])), 
            phat=sqrt(var(b.phat[valid])), Es=sqrt(var(b.Es[valid])))
   cv<-list(Nhat.grp=se$Nhat.grp/boot.mean$Nhat.grp , Nhat.ind=se$Nhat.ind/boot.mean$Nhat.ind, 
            phat=se$phat/boot.mean$phat, Es=se$Es/boot.mean$Es)
   ninvalid <- length(valid) - sum(valid)
   if (ninvalid > 0) 
     warning(paste(as.character(ninvalid), " inadmissable estimates omitted from bootstrap results."))
   for (i in 1:length(ci)) {
     sort.est <- sort(boot.dbn[[i]][valid])
     cin <- round((nboot - ninvalid) * vlevels, 0)
     cin <- ifelse(cin < 1, 1, cin)
     ci[[i]] <- sort.est[cin]
   }
 }
 proflik<-NA
 if (plot==TRUE & ci.type=="profile") {
   Nleft <- round(Nhat + 1.5 * (ci$Nhat.grp[1] - Nhat))
   Nright <- round(Nhat + 1.5 * (ci$Nhat.grp[length(vlevels)] - Nhat))
   W <- rep(0, (Nright - Nleft + 1))
   for (N in Nleft:Nright) {
     p <- (nn)/(2*N-ns[1])
     W[N-Nleft+1] <- llk(N,p)-llkmin
   }
   crit <- q * (-1)
   N <- c(Nleft:Nright)
   N <- N[W < Inf & !is.nan(W)]
   W <- W[W < Inf & !is.nan(W)]
   proflik<-list(crit=crit, N=N, W=W, Nleft=Nleft, Nright=Nright)
 }
 intest <- list(levels=vlevels, ci=ci, boot.mean=boot.mean, boot.dbn=boot.dbn, se=se, cv=cv, ci.type=ci.type, 
                 proflik=proflik, numerical=numerical, parents=parents, created=date(), seed=seed)
 class(intest) <- "int.est.rm"
 if(plot) plot(intest,type="hist")
 return(intest)
}

is.int.est.rm<-function(est)
{
 inherits(est, "int.est.rm")
}



summary.int.est.rm<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "phat"), digits=5)
{
 if(!is.int.est.rm(iest)) 
   stop("Argument <iest>. must be of class int.est.rm\n")
 addtext1<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 addtext2<-paste("Numerical estimator used?         : ",iest$numerical,sep="")
 addtext<-paste(addtext1,addtext2,sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.rm<-function(iest, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}





int.est.ce<-function (samp, ci.type="boot.nonpar", nboot=999, vlevels=c(0.025,0.975), plot=FALSE, seed=NULL, ...) 
{
 if (!is.sample.rm(samp)) stop("\n*** <samp> is not an object of type 'sample.rm'.\n")
 if (!(ci.type %in% c("boot.nonpar"))) 
     stop(paste("\n*** Invalid <ci.type>. Only nonparametric bootstrap implemented: ci.type='boot.nonpar' \n"))
 if (!is.numeric(vlevels)) stop("\n*** All <vlevels> values must be numeric.\n")
 if (any(vlevels < 0) | any(vlevels > 1)) stop("\n*** All <vlevels> values must be between 0 and 1.\n")
 if (!is.numeric(nboot)) stop("\n*** <nboot> must be numeric.\n")
 if (nboot < 2) stop("\n*** <nboot> must be at least 2.\n")
 if (!is.logical(plot)) stop("\n*** <plot> must be TRUE or FALSE.\n")
 if (ci.type != "boot.par" & ci.type != "boot.nonpar" & ci.type != "profile") plot <- FALSE
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
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
 dets <- samp$detected
 rems <- samp$removal
 rs <- apply(rems, 2, sum)
 ns <- apply(dets, 2, sum)
 nn <- sum(ns)
 n.occ <- length(ns)
 res <- point.est.ce(samp)
 Nhat <- round(res$Nhat.grp)
 phat <- res$phat
 maxest <- 100 * Nhat
 Es <- res$Es
 seen <- apply(dets, 1, sum) > 0
 groupsize <- samp$population$groupsize[seen]
 boot.dbn <- NULL
 boot.mean <- NULL
 if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
   b.Nhat.grp <- rep(0, nboot)
   b.Nhat.ind <- rep(0, nboot)
   b.theta <- rep(0, nboot)
###         Fixed comma problem in following ER  June 07
   b.phat <- matrix(rep(0, nboot*n.occ), nrow=nboot, ncol=n.occ, dimnames=list(replicate=1:nboot, paste("occasion",1:n.occ)))
   b.Es <- rep(0, nboot)
   b.samp <- samp
 }
 if (ci.type == "boot.nonpar") {
   rem <- rems[seen, ]
   det <- dets[seen, ]
   n0 <- Nhat - nrow(det)
   size <- c(samp$population$groupsize[seen], rep(0, n0))
   zero <- matrix(rep(0, n0 * ncol(rem)), ncol = ncol(rem))
   rem <- matrix(c(t(rem), zero), ncol = ncol(rem), byrow = T)
   det <- matrix(c(t(det), zero), ncol = ncol(rem), byrow = T)
   for (i in 1:nboot) {
     repeat {
       index <- sample(1:Nhat, Nhat, replace = T)
       b.samp$removal <- rem[index, ]
       b.samp$detected <- det[index, ]
       b.samp$population$groupsize <- size[index]
       est <- point.est.ce(b.samp)
       b.Nhat.grp[i] <- est$Nhat.grp
       b.Nhat.ind[i] <- est$Nhat.ind
       b.theta[i] <- est$theta
       b.phat[i, ] <- est$phat
       b.Es[i] <- est$Es
       if (est$Nhat.grp < maxest) break
     }
   }
 }
 if (ci.type == "boot.par" | ci.type == "boot.nonpar") {
   boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, theta=b.theta, phat = b.phat, Es = b.Es)
   valid<-(b.Nhat.grp != Inf & boot.dbn[[1]] > 0)
   boot.mean <- list(Nhat.grp=mean(b.Nhat.grp[valid]), Nhat.ind=mean(b.Nhat.ind[valid]), 
            theta = mean(b.theta[valid]), phat=apply(as.matrix(b.phat[valid,]),2,mean), Es = mean(b.Es[valid]))
   se <- list(Nhat.grp=sqrt(var(b.Nhat.grp[valid])), Nhat.ind=sqrt(var(b.Nhat.ind[valid])), 
            theta=sqrt(var(b.theta[valid])), phat=sqrt(apply(as.matrix(b.phat[valid,]),2,var)), 
            Es=sqrt(var(b.Es[valid])))
   cv<-list(Nhat.grp=se$Nhat.grp/boot.mean$Nhat.grp , Nhat.ind=se$Nhat.ind/boot.mean$Nhat.ind, 
            theta=se$theta/boot.mean$theta, phat=se$phat/boot.mean$phat, Es=se$Es/boot.mean$Es)
   ninvalid <- length(valid) - sum(valid)
   if (ninvalid > 0) 
     warning(paste(as.character(ninvalid), " inadmissable estimates omitted from bootstrap results."))
   civec <- rep(NA, length(vlevels))
#  careful with coding the next line depends on which estimates are vectors and which scalars
#  (in this function only phat is a vector):
   ci <- list(Nhat.grp=civec, Nhat.ind=civec, theta=civec, phat=matrix(rep(civec,n.occ), nrow=n.occ, ncol=length(vlevels), 
            dimnames = list(dimnames(b.phat)[[2]], rep("", 2))), Es = civec)
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
                 parents=parents, created=date(), seed=seed)
 class(intest) <- "int.est.ce"
 if(plot) plot(intest,type="hist")
 return(intest)
}

is.int.est.ce<-function(est)
{
 inherits(est, "int.est.ce")
}



summary.int.est.ce<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "theta", "phat"), digits=5)
{
 if(!is.int.est.ce(iest)) 
   stop("Argument <iest>. must be of class int.est.ce\n")
 addtext<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.ce<-function(iest, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}


int.est.cir<-function (samp, ci.type="boot.nonpar", nboot=999, vlevels=c(0.025,0.975), plot=FALSE, numerical=TRUE, seed=NULL, ...) 
{
 if (!is.sample.rm(samp)) stop("\n*** <samp> is not an object of type 'sample.rm'.\n")
 if (!(ci.type %in% c("boot.nonpar"))) 
     stop(paste("\n*** Invalid <ci.type>. Only nonparametric bootstrap implemented: ci.type='boot.nonpar' \n"))
 if (!is.numeric(vlevels)) stop("\n*** All <vlevels> values must be numeric.\n")
 if (any(vlevels < 0) | any(vlevels > 1)) stop("\n*** All <vlevels> values must be between 0 and 1.\n")
 if (!is.numeric(nboot)) stop("\n*** <nboot> must be numeric.\n")
 if (nboot < 2) stop("\n*** <nboot> must be at least 2.\n")
 if (!is.logical(plot)) stop("\n*** <plot> must be TRUE or FALSE.\n")
 if (ci.type != "boot.par" & ci.type != "boot.nonpar" & ci.type != "profile") plot <- FALSE
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
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
 dets <- samp$detected
 rems <- samp$removal
 rs <- apply(rems, 2, sum)
 ns <- apply(dets, 2, sum)
 nn <- sum(ns)
 n.occ <- length(ns)
 res <- point.est.cir(samp, numerical=numerical)
 Nhat <- round(res$Nhat.grp)
 phat <- res$phat
 maxest <- 100 * Nhat
 Es <- res$Es
 seen <- apply(dets, 1, sum) > 0
 groupsize <- samp$population$groupsize[seen]
 boot.dbn <- NULL
 boot.mean <- NULL
 if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
   b.Nhat.grp <- rep(0, nboot)
   b.Nhat.ind <- rep(0, nboot)
   b.theta <- rep(0, nboot)
###      Extra comma on following line found by Mike Merridith WCS June 2007   
   b.phat <- matrix(rep(0, nboot), nrow=nboot, ncol=n.occ, dimnames=list(replicate=1:nboot, paste("occasion",1:n.occ)))
   b.Es <- rep(0, nboot)
   b.samp <- samp
 }
 if (ci.type == "boot.nonpar") {
   rem <- rems[seen, ]
   det <- dets[seen, ]
   n0 <- Nhat - nrow(det)
   if(n0>0) {
    types<-c(samp$population$types[seen], rep(samp$population$types[1], n0)) # need to get types same length as it is used in estimator
    size <- c(samp$population$groupsize[seen], rep(0, n0))
    zero <- matrix(rep(0, n0 * ncol(rem)), ncol = ncol(rem))
    rem <- matrix(c(t(rem), zero), ncol = ncol(rem), byrow = T)
    det <- matrix(c(t(det), zero), ncol = ncol(rem), byrow = T)
   }
   else {
    types <- samp$population$types[seen] # need to get types same length as it is used in estimator
    size <- samp$population$groupsize[seen]
    rem <- matrix(t(rem), ncol = ncol(rem), byrow = T)
    det <- matrix(t(det), ncol = ncol(rem), byrow = T)
   }
   for (i in 1:nboot) {
     repeat {
       index <- sample(1:Nhat, Nhat, replace = T)
       b.samp$removal <- rem[index, ]
       b.samp$detected <- det[index, ]
       b.samp$population$groupsize <- size[index]
       b.samp$population$types <- types[index]  # need to get types as they are used in estimator
       est <- point.est.cir(b.samp, numerical=numerical)
       b.Nhat.grp[i] <- est$Nhat.grp
       b.Nhat.ind[i] <- est$Nhat.ind
       b.theta[i] <- est$theta
       b.phat[i, ] <- est$phat
       b.Es[i] <- est$Es
       if (est$Nhat.grp < maxest) break
     }
   }
 }
 if (ci.type == "boot.par" | ci.type == "boot.nonpar") {
   boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, theta=b.theta, phat = b.phat, Es = b.Es)
   valid<-(b.Nhat.grp != Inf & boot.dbn[[1]] > 0)
   boot.mean <- list(Nhat.grp=mean(b.Nhat.grp[valid]), Nhat.ind=mean(b.Nhat.ind[valid]), 
            theta = mean(b.theta), phat=apply(as.matrix(b.phat[valid,]),2,mean), Es = mean(b.Es[valid]))
   se <- list(Nhat.grp=sqrt(var(b.Nhat.grp[valid])), Nhat.ind=sqrt(var(b.Nhat.ind[valid])), 
            theta=sqrt(var(b.theta[valid])), phat=sqrt(apply(as.matrix(b.phat[valid,]),2,var)), 
            Es=sqrt(var(b.Es[valid])))
   cv<-list(Nhat.grp=se$Nhat.grp/boot.mean$Nhat.grp , Nhat.ind=se$Nhat.ind/boot.mean$Nhat.ind, 
            theta=se$theta/boot.mean$theta, phat=se$phat/boot.mean$phat, Es=se$Es/boot.mean$Es)
   ninvalid <- length(valid) - sum(valid)
   if (ninvalid > 0) 
     warning(paste(as.character(ninvalid), " inadmissable estimates omitted from bootstrap results."))
   civec <- rep(NA, length(vlevels))
#  careful with coding the next line depends on which estimates are vectors and which scalars
#  (in this function only phat is a vector):
   ci <- list(Nhat.grp=civec, Nhat.ind=civec, theta=civec, phat=matrix(rep(civec,n.occ), nrow=n.occ, ncol=length(vlevels), 
            dimnames = list(dimnames(b.phat)[[2]], rep("", 2))), Es = civec)
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
                 parents=parents, created=date(), seed=seed)
 class(intest) <- "int.est.cir"
 if(plot) plot(intest,type="hist")
 return(intest)
}


is.int.est.cir<-function(est)
{
 inherits(est, "int.est.cir")
}

summary.int.est.cir<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "theta", "phat"), digits=5)
{
 if(!is.int.est.cir(iest)) 
   stop("Argument <iest>. must be of class int.est.cir\n")
 addtext<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.cir<-function(iest, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}



point.sim.rm<-function (pop.spec, survey.spec, design.spec, B=99, numerical=TRUE, plot=FALSE, show=FALSE, seed=NULL, ...) 
{
 if (!is.pars.survey.rm(survey.spec)) {
   stop("\nsurvey.spec must be of class 'pars.survey.rm'.\n")
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
 stats<-c("Nhat.grp","Nhat.ind", "Es") 
 len<-length(stats)
 res <- matrix(0, nrow = B, ncol=len)
 res <- as.data.frame(res)
 names(res)<-stats
 out.est <- NULL
#    design.spec <- survey.spec$design
 for (i in 1:B) {
   if (is.population(pop.spec)) mypop <- pop.spec
   if (is.pars.population(pop.spec)) {
     mypop <- generate.population(pop.spec)
     random.pop<-TRUE
   }
   if (is.design.rm(design.spec)) mydes <- design.spec
#   if (is.pars.design.rm(design.spec)) {
#     mydes <- generate.design.rm(design.spec)
#     random.design<-TRUE
#   }
   survey.spec$population <- mypop
   survey.spec$design <- mydes
   mysamp <- generate.sample.rm(survey.spec)
   out.est <- point.est.rm(mysamp, numerical=numerical)
   res[i, stats] <- out.est[stats]
   if(show) plot(out.est)
 }
 sim<-list(est=res, true=true, numerical=numerical, 
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
 class(sim) <- "point.sim.rm"
 if(plot) plot(sim, est="Nhat.grp")
 return(sim)
}


is.point.sim.rm<-function(sim)
{
 inherits(sim, "point.sim.rm")
}

#plot.point.sim.rm<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), breaks="Sturges", type="both", ...)
plot.point.sim.rm<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
 if(length(est)>1) breaks="Sturges"
 for(i in 1:length(est)) {
   if(i>1) windows(height=5, width=4)
   sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
 }
}


summary.point.sim.rm<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), digits=5)
{
 if(!is.point.sim.rm(sim)) 
   stop("Argument <sim>. must be of class point.sim.rm\n")
 addtext<-paste("Numerical estimator? = ",sim$numerical, sep="")
 summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}



point.sim.ce<-function (pop.spec, survey.spec, design.spec, B=99, plot=FALSE, show=FALSE, seed=NULL, ...) 
{
 if (!is.pars.survey.rm(survey.spec)) {
   stop("\nsurvey.spec must be of class 'pars.survey.rm'.\n")
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
 stats<-c("Nhat.grp","Nhat.ind", "Es") 
 len<-length(stats)
 res <- matrix(0, nrow = B, ncol=len)
 res <- as.data.frame(res)
 names(res)<-stats
 out.est <- NULL
#    design.spec <- survey.spec$design
 for (i in 1:B) {
   if (is.population(pop.spec)) mypop <- pop.spec
   if (is.pars.population(pop.spec)) {
     mypop <- generate.population(pop.spec)
     random.pop<-TRUE
   }
   if (is.design.rm(design.spec)) mydes <- design.spec
#   if (is.pars.design.ce(design.spec)) {
#     mydes <- generate.design.ce(design.spec)
#     random.design<-TRUE
#   }
   survey.spec$population <- mypop
   survey.spec$design <- mydes
   mysamp <- generate.sample.rm(survey.spec)
   out.est <- point.est.ce(mysamp)
   res[i, stats] <- out.est[stats]
   if(show) plot(out.est)
 }
 sim<-list(est=res, true=true, 
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
 class(sim) <- "point.sim.ce"
 if(plot) plot(sim, est="Nhat.grp")
 return(sim)
}


is.point.sim.ce<-function(sim)
{
 inherits(sim, "point.sim.ce")
}

#plot.point.sim.ce<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), breaks="Sturges", type="both", ...)
plot.point.sim.ce<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
 if(length(est)>1) breaks="Sturges"
 for(i in 1:length(est)) {
   if(i>1) windows(height=5, width=4)
   sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
 }
}


summary.point.sim.ce<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), digits=5)
{
 if(!is.point.sim.ce(sim)) 
   stop("Argument <sim>. must be of class point.sim.ce\n")
 addtext<-""
 summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}



point.sim.cir<-function (pop.spec, survey.spec, design.spec, B=99, plot=FALSE, numerical=TRUE, seed=NULL, ...) 
{
 if (!is.pars.survey.rm(survey.spec)) {
   stop("\nsurvey.spec must be of class 'pars.survey.rm'.\n")
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
 stats<-c("Nhat.grp","Nhat.ind", "Es") 
 len<-length(stats)
 res <- matrix(0, nrow = B, ncol=len)
 res <- as.data.frame(res)
 names(res)<-stats
 out.est <- NULL
#    design.spec <- survey.spec$design
 for (i in 1:B) {
   if (is.population(pop.spec)) mypop <- pop.spec
   if (is.pars.population(pop.spec)) {
     mypop <- generate.population(pop.spec)
     random.pop<-TRUE
   }
   if (is.design.rm(design.spec)) mydes <- design.spec
#   if (is.pars.design.cir(design.spec)) {
#     mydes <- generate.design.cir(design.spec)
#     random.design<-TRUE
#   }
   survey.spec$population <- mypop
   survey.spec$design <- mydes
   mysamp <- generate.sample.rm(survey.spec)
   out.est <- point.est.cir(mysamp, numerical=numerical)
   res[i, stats] <- out.est[stats]
#   if(show) plot(out.est)
 }
 sim<-list(est=res, true=true, numerical=numerical, 
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
 class(sim) <- "point.sim.cir"
 if(plot) plot(sim, est="Nhat.grp")
 return(sim)
}


is.point.sim.cir<-function(sim)
{
 inherits(sim, "point.sim.cir")
}

#plot.point.sim.cir<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), breaks="Sturges", type="both", ...)
plot.point.sim.cir<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
 if(length(est)>1) breaks="Sturges"
 for(i in 1:length(est)) {
   if(i>1) windows(height=5, width=4)
   sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
 }
}


summary.point.sim.cir<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), digits=5)
{
 if(!is.point.sim.cir(sim)) 
   stop("Argument <sim>. must be of class point.sim.cir\n")
 addtext<-""
 summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}



# Non-plotting functions

plot.design.rm<-function(x,col="black")
{
 plot.text("There is no useful plot for this class of object",col=col)
}

plot.point.est.cir<-function(x,col="black")
{
 plot.text("There is no useful plot for this class of object",col=col)
}






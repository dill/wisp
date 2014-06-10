generate.design.cr<- function (reg, n.occ = 2, effort = rep(1, n.occ))
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
       stop("\n*** The survey effort must be numeric.\n")
   if (length(effort) != n.occ) 
       stop(paste("\n*** The number of given values of survey effort", 
           "must be identical to the number of survey occasions.\n"))
parents<-list(wisp.id(reg,newname=as.character(substitute(reg))))
   pars <- list(region = reg, number.occasions = n.occ, effort = effort, parents=parents, created=date())
   class(pars) <- "design.cr"
   pars
}

is.design.cr<-function (des) 
{
    inherits(des, "design.cr")
}

summary.design.cr<-function(des)
{
#  check class:
   if (!is.design.cr(des)) stop("\nThe parameter <des> must be of type 'design.cr'.\n")
   cat("\n")
   cat("MARK-RECAPTURE DESIGN SUMMARY\n")
   cat("-----------------------------\n")
   cat("creation date   :", des$created,"\n")
   cat("parent object(s) (class, name, creation date):\n")
   for(i in 1:length(des$parents)) {
      cat("      ",paste("(",des$parents[[i]]$class,", ",des$parents[[i]]$name,", ",des$parents[[i]]$created,")",sep=""),"\n")
   }
   cat("\n")
   cat("Number of sampling occasions      :", des$number.occasions,"\n")
   cat("Sampling effort on each occasion  :", des$effort,"\n")
   cat("Region dimensions (length x width):", des$reg$length, "x", des$reg$width, "\n")
}

plot.design.cr<-function(x,col="black")
{
   plot.text("There is no useful plot for this class of object",col=col)
}

setpars.survey.cr<-function (pop, des, pmin.unmarked, pmax.unmarked = pmin.unmarked, 
   pmin.marked = pmin.unmarked, pmax.marked = pmax.unmarked, improvement = 0) 
{
   if (!is.population(pop)) 
       stop("\n*** The parameter <pop> must be of type 'population'.\n")
   if (!is.design.cr(des)) 
       stop("\n*** The parameter <des> must be of type 'design.cr'.\n")
   if (!equal(pop$region, des$region)) 
       stop(paste("\n*** The given population and design were defined", 
           "with different regions.\n"))
   min.exposure <- pop$minexposure
   max.exposure <- pop$maxexposure
   if (!is.numeric(pmin.unmarked) | !is.numeric(pmax.unmarked) | 
       !is.numeric(pmin.marked) | !is.numeric(pmax.marked)) 
       stop("\n*** All <pmin> and <pmax> values must be numeric.\n")
   if ((pmin.unmarked < 0) | (pmax.unmarked < 0) | (pmin.marked < 
       0) | (pmax.marked < 0)) 
       stop("\n*** The <pmin> and <pmax> values cannot be negative.\n")
   if ((pmin.unmarked >= 1) | (pmax.unmarked >= 1) | (pmin.marked >= 
       1) | (pmax.marked >= 1)) 
       stop("\n*** The <pmin> and <pmax> values cannot be 1 or bigger.\n")
   if ((pmin.unmarked > pmax.unmarked) | (pmin.marked > pmax.marked)) 
       stop("\n*** The <pmin> values cannot be bigger than <pmax> values.\n")
   if ((min.exposure == max.exposure) & (pmin.unmarked != pmax.unmarked)) 
       print(paste("warning: Exposure boundaries are identical. Therefore", 
           "<pmax.unmarked> is ignored."))
   if ((min.exposure == max.exposure) & (pmin.marked != pmax.marked)) 
       print(paste("warning: Exposure boundaries are identical. Therefore", 
           "<pmax.marked> is ignored."))
   if (!is.numeric(improvement)) 
       stop("\n*** <improvement> must be numeric.\n")
   if (improvement < 0) 
       stop("\n*** <improvement> cannot be negative.\n")
   parents<-         list(wisp.id(pop,newname=as.character(substitute(pop))),wisp.id(des,newname=as.character(substitute(des))))
   theta.unmarked <- transform.setpars.parameter(min.exposure, 
       max.exposure, pmin.unmarked, pmax.unmarked, improvement)
   theta.marked <- transform.setpars.parameter(min.exposure, 
       max.exposure, pmin.marked, pmax.marked, improvement)
   pars <- list(population = pop, design = des, theta0.unmarked = theta.unmarked$zero, 
       theta0.marked = theta.marked$zero, theta1.unmarked = theta.unmarked$one, 
       theta1.marked = theta.marked$one, theta2 = theta.unmarked$two, parents=parents, created=date())
   class(pars) <- "pars.survey.cr"
   pars
}

is.pars.survey.cr<-function (survpars) 
{
   inherits(survpars,"pars.survey.cr")
}

plot.pars.survey.cr<-function(pars, type="p", marked=FALSE)  {
   if (!is.pars.survey.cr(pars)) 
   stop("\n*** The parameter <pars> must be of type 'pars.survey.cr'.\n")
   if(type!="p" && type!="p.dbn") 
   stop("\n *** The parameter <type> must be 'p' or 'p.dbn'.")
   x<-pars$population$exposure
   xp<-seq(pars$population$minexposure,pars$population$maxexposure,length=50)
   eff<-pars$des$effort
   theta0.unmarked<-pars$theta0.unmarked
   theta1.unmarked<-pars$theta1.unmarked
   theta0.marked<-pars$theta0.marked
   theta1.marked<-pars$theta1.marked
   theta2<-pars$theta2
#  calculate p's as funciton of exposure
   px.unmarked<-detection.capture.recapture(theta0.unmarked, theta1.unmarked, theta2, xp, eff)
   px.marked<-detection.capture.recapture(theta0.marked, theta1.marked, theta2, xp, eff)
#  calculate p's in population
   p.unmarked<-detection.capture.recapture(theta0.unmarked, theta1.unmarked, theta2, x, eff)
   p.marked<-detection.capture.recapture(theta0.marked, theta1.marked, theta2, x, eff)
#  plotting parameters
   n.occ <-pars$design$number.occasions
#  set scaling factor for labels, axes and text to be 90% (plot window height)/5
   old.par<-par(no.readonly=TRUE)
   cex<-0.9*par()$din[2]/5
   par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)

   if(type=="p") {
      par(mfrow=c(1,2))
   #  plot capture functions
      plot(xp,c(rep(0,(length(xp)-1)),max(px.unmarked)),ylim=c(0,1), type="n", main="Capture function(s) for Unmarked", xlab="Exposure", ylab="Capture probability")
      for(j in 1:n.occ) {
         if(pars$population$minexposure==pars$population$maxexposure) {
           points(xp,px.unmarked[,j],col=j, pch=j)
           labels<-paste("Occasion",as.character(1:n.occ))
           legend(max(xp),0,labels,pch=1:n.occ,col=1:n.occ,xjust=1,yjust=0, cex=0.66*cex) 
         } else {
           lines(xp,px.unmarked[,j],col=j)
           labels<-paste("Occasion",as.character(1:n.occ))
           legend(max(xp),0,labels,lty=1,col=1:n.occ,xjust=1,yjust=0, cex=0.66*cex) 
         }
      }
      plot(xp,c(rep(0,(length(xp)-1)),max(px.marked)),ylim=c(0,1), type="n", main="Capture function(s) for Marked", xlab="Exposure", ylab="Capture probability")
      for(j in 1:n.occ) {
         if(pars$population$minexposure==pars$population$maxexposure) {
           points(xp,px.marked[,j],col=j, pch=j)
           labels<-paste("Occasion",as.character(1:n.occ))
           legend(max(xp),0,labels,pch=1:n.occ,col=1:n.occ,xjust=1,yjust=0, cex=0.66*cex) 
         } else {
           lines(xp,px.marked[,j],col=j)
           labels<-paste("Occasion",as.character(1:n.occ))
           legend(max(xp),0,labels,lty=1,col=1:n.occ,xjust=1,yjust=0, cex=0.66*cex) 
         }
      } 
   }

  if(type=="p.dbn" & !marked) {
      par(cex.lab=0.6,cex.main=0.7,mfrow=c(2,2))
   #  get breaks for p histograms, then add 0, 1 if neccessary:
      meanp.unmarked<-apply(p.unmarked,2,mean)
      high.occ.unmarked<-which(meanp.unmarked==max(meanp.unmarked))[1]
      low.occ.unmarked<-which(meanp.unmarked==min(meanp.unmarked))[1]
      hst<-hist(p.unmarked[,1], main=paste("p distribution of unmarked, first (occasion ",1,")",sep=""), xlab="Capture probability (red line is mean)", col=1)
      lines(c(meanp.unmarked[1],meanp.unmarked[1]),c(0,max(hst$counts)),col="red",lty=2)
      hst<-hist(p.unmarked[,n.occ], main=paste("p distribution of unmarked, last (occasion ",n.occ,")",sep=""), xlab="Capture probability (red line is mean)",col=n.occ)
      lines(c(meanp.unmarked[n.occ],meanp.unmarked[n.occ]),c(0,max(hst$counts)),col="red",lty=2)
      hst<-hist(p.unmarked[,low.occ.unmarked], main=paste("p distribution of unmarked, lowest mean p (occasion ",low.occ.unmarked,")",sep=""), xlab="Capture probability (red line is mean)", col=low.occ.unmarked)           
      lines(c(meanp.unmarked[low.occ.unmarked],meanp.unmarked[low.occ.unmarked]), c(0,max(hst$counts)),col="red",lty=2)
      hst<-hist(p.unmarked[,high.occ.unmarked], main=paste("p distribution of unmarked, highest mean p (occasion ",high.occ.unmarked,")",sep=""), xlab="Capture probability (red line is mean)", col=high.occ.unmarked)
      lines(c(meanp.unmarked[high.occ.unmarked],meanp.unmarked[high.occ.unmarked]), c(0,max(hst$counts)),col="red",lty=2)
      par(old.par)
   }

   if(type=="p.dbn" & marked) {
      par(cex.lab=0.6,cex.main=0.7,mfrow=c(2,2))
   #  get breaks for p histograms, then add 0, 1 if neccessary:
      meanp.marked<-apply(p.marked,2,mean)
      high.occ.marked<-which(meanp.marked==max(meanp.marked))[1]
      low.occ.marked<-which(meanp.marked==min(meanp.marked))[1]
      hst<-hist(p.marked[,1], main=paste("p distribution of marked, first (occasion ",1,")",sep=""), xlab="Capture probability (red line is mean)", col=1)
      lines(c(meanp.marked[1],meanp.marked[1]),c(0,max(hst$counts)),col="red",lty=2)
      hst<-hist(p.marked[,n.occ], main=paste("p distribution of marked, last (occasion ",n.occ,")",sep=""), xlab="Capture probability (red line is mean)",col=n.occ)
      lines(c(meanp.marked[n.occ],meanp.marked[n.occ]),c(0,max(hst$counts)),col="red",lty=2)
      hst<-hist(p.marked[,low.occ.marked], main=paste("p distribution of marked, lowest mean p (occasion ",low.occ.marked,")",sep=""), xlab="Capture probability (red line is mean)", col=low.occ.marked)           
      lines(c(meanp.marked[low.occ.marked],meanp.marked[low.occ.marked]), c(0,max(hst$counts)),col="red",lty=2)
      hst<-hist(p.marked[,high.occ.marked], main=paste("p distribution of marked, highest mean p (occasion ",high.occ.marked,")",sep=""), xlab="Capture probability (red line is mean)", col=high.occ.marked)
      lines(c(meanp.marked[high.occ.marked],meanp.marked[high.occ.marked]), c(0,max(hst$counts)),col="red",lty=2)
      par(old.par)
   }
}


summary.pars.survey.cr<-function(pars,plot=FALSE, digits=5)
{
#  check class:
   if (!is.pars.survey.cr(pars)) stop("\nThe parameter <pars> must be of type 'pars.survey.cr'.\n")
   cat("\n")
   cat("MARK-RECAPTURE SURVEY PARAMETER SUMMARY\n")
   cat("---------------------------------------\n")
   cat("creation date   :", pars$created,"\n")
   cat("parent object(s) (class, name, creation date):\n")
   for(i in 1:length(pars$parents)) {
      cat("      ",paste("(",pars$parents[[i]]$class,", ",pars$parents[[i]]$name,", ",pars$parents[[i]]$created,")",sep=""),"\n")
   }
   x<-pars$population$exposure
   xp<-seq(pars$population$minexposure,pars$population$maxexposure,length=50)
   eff<-pars$des$effort
   theta0.unmarked<-pars$theta0.unmarked
   theta0.marked<- pars$theta0.marked
   theta1.unmarked<- pars$theta1.unmarked
   theta1.marked<- pars$theta1.marked
   theta2<- pars$theta2
#  calculate p's as funciton of exposures in population
   K<-length(eff)
   p.unmarked<-detection.capture.recapture(theta0.unmarked, theta1.unmarked, theta2, x, eff)
   p.marked<-detection.capture.recapture(theta0.marked, theta1.marked, theta2, x, eff)
   meanp.unmarked<-apply(p.unmarked,2,mean)
   meanp.marked<-apply(p.marked,2,mean)
   max.meanp.occ.unmarked<-which(meanp.unmarked==max(meanp.unmarked))[1]
   max.meanp.occ.marked<-which(meanp.marked==max(meanp.marked))[1]
   min.meanp.occ.unmarked<-which(meanp.unmarked==min(meanp.unmarked))[1]
   min.meanp.occ.marked<-which(meanp.marked==min(meanp.marked))[1]
   max.meanp.unmarked<-meanp.unmarked[max.meanp.occ.unmarked]
   max.meanp.marked<-meanp.marked[max.meanp.occ.marked]
   min.meanp.unmarked<-meanp.unmarked[min.meanp.occ.unmarked]
   min.meanp.marked<-meanp.marked[min.meanp.occ.marked]
   cat("\n")
   cat("  Capture function:  p(exposure,mark,occasion)= 1-exp( -theta0[marked or unmarked] -theta1[marked or unmarked]*exposure -effort*(1 + theta2*(occasions-1)))\n")
   cat("\n")
   cat("Intercept parameter theta0 for unmarked   : ",signif(pars$theta0.unmarked,digits),"\n")
   cat("  Intercept parameter theta0 for marked   : ",signif(pars$theta0.marked,digits),"\n")
   cat(" Exposure parameter theta1 for unmarked   : ",signif(pars$theta1.unmarked,digits),"\n")
   cat("   Exposure parameter theta1 for marked   : ",signif(pars$theta1.marked,digits),"\n")
   cat("  Improvement parameter theta2: ",signif(pars$theta2,digits),"\n")    
   cat("\n")
   cat("Mean capture prob. in population with no marks\n")
   cat("                First occasion:", signif(meanp.unmarked[1],digits),"\n")
   cat("                 Last occasion:", signif(meanp.unmarked[K],digits),"\n")
   cat(paste("  Highest mean p for unmarked (occasion ",max.meanp.occ.unmarked,"):",sep=""), signif(max.meanp.unmarked,digits),"\n")
   cat(paste("Highest mean p for marked (occasion ",max.meanp.occ.marked,")    :",sep=""), signif(max.meanp.marked,digits),"\n")
   cat(paste("  Lowest mean p for unmarked (occasion ",min.meanp.occ.unmarked,") :",sep=""), signif(min.meanp.unmarked,digits),"\n")
   cat(paste("Lowest mean p for marked (occasion ",min.meanp.occ.marked,")     :",sep=""), signif(min.meanp.marked,digits),"\n")
   cat("\n")
   cat("Number of sampling occasions      :", pars$design$number.occasions,"\n")
   cat("Sampling effort on each occasion  :", pars$design$effort,"\n")
   cat("\n")
   cat("Region dimensions (length x width):", pars$design$region$length, "x", pars$design$region$width, "\n")

   if(plot) plot(pars)
}



generate.sample.cr<-function (pars.survey.cr, seed=NULL) 
{
   if (!is.pars.survey.cr(pars.survey.cr)) stop("\nThe parameter <pars.survey.cr> must be of type 'pars.survey.cr'.\n")
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
   parents<-list(wisp.id(pars.survey.cr,newname=as.character(substitute(pars.survey.cr))))
   pop <- pars.survey.cr$population
   des <- pars.survey.cr$design
   effort <- pars.survey.cr$design$effort
   exposure <- pop$exposure
   n.groups <- length(exposure)
   n.occ <- des$number.occasions
   theta0.unmarked <- pars.survey.cr$theta0.unmarked
   theta0.marked <- pars.survey.cr$theta0.marked
   theta1.unmarked <- pars.survey.cr$theta1.unmarked
   theta1.marked <- pars.survey.cr$theta1.marked
   theta2 <- pars.survey.cr$theta2
   effort <- rep(1, n.occ)
   p.detect.unmarked <- detection.capture.recapture(theta0 = theta0.unmarked, theta1 = theta1.unmarked, theta2 = theta2, exposure = exposure, effort = effort)
   p.detect.marked <- detection.capture.recapture(theta0 = theta0.marked, theta1 = theta1.marked, theta2 = theta2, exposure = exposure, effort = effort)

   detected <- matrix(0, nrow = n.groups, ncol = n.occ)
   for (i in 1:n.groups) {
      det <- rbinom(n.occ, 1, p.detect.unmarked[i, ])
      if (any(det[1:(n.occ - 1)] == 1)) {
         detected[i, min((1:n.occ)[det == 1])] <- 1
         first.detection <- (which(det == 1))[1]
         range <- (first.detection + 1):n.occ
         det[range] <- rbinom(length(range), 1, p.detect.marked[i,range])
      }
      detected[i, ] <- det
   }
   samp <- list(population = pop, design = des, capture = detected, parents=parents, created=date(), seed=seed)
   class(samp) <- "sample.cr"
   return(samp)
}


is.sample.cr<-function (samp) 
{
    inherits(samp, "sample.cr")
}


summary.sample.cr<-function(samp, digits=5) 
{
 if (!is.sample.cr(samp)) stop("\nThe parameter <samp> must be of class 'sample.cr'.\n")
 cat("\n")
 cat("SAMPLE SUMMARY MARK-RECAPTURE METHODS\n")
 cat("-------------------------------------\n")
 cat("creation date   :", samp$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(samp$parents)) {
   cat("      ",paste("(",samp$parents[[i]]$class,", ",samp$parents[[i]]$name,", ",samp$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(samp$seed)) cat("random number seed used: ",samp$seed,"\n")
 cat("\n")
 n.occ<-samp$design$number.occasions
 fr<-t(apply(samp$capture,1,cumsum))
 marked<-(fr>0)*1
 marked[,2:length(marked[1,])]<-marked[,1:(length(marked[1,])-1)]
 marked[,1]<-0
 fs<-apply(samp$capture,1,sum)
 m<-apply(samp$capture*marked, 2, sum)
 f<-rep(0,n.occ)
 for(i in 1:n.occ) f[i]<-length(which(fr[,n.occ]==i)) 
 n <- apply(samp$capture, 2, sum)
 cat("Number of survey occasions                  :", n.occ, "\n\n")
 cat("Total number of captured groups             :", sum(n-m), "\n")
 cat("Number captured on each occasion            :", n, "\n")
 cat("Number of marked captured on each occasion  :", m, "\n")
 cat("Number of unmarked captured on each occasion:", n-m, "\n")
 cat("Capture frequencies:\n")
 for(i in 1:n.occ) if(i==1)
   cat(paste("                       Number caught once   : ",f[i],"\n", sep=""))
 else
   cat(paste("                       Number caught ",i," times: ",f[i],"\n", sep=""))
}


plot.sample.cr<-function (samp, which.occasion = 0, show.sizes = TRUE, show.exps = TRUE, 
    dsf = 0.75, whole.population = FALSE, type="freq") 
{
    if (!is.sample.cr(samp)) 
        stop("\nThe parameter <samp> must be of type 'sample.cr'.\n")
    plot.sample.capture.methods(samp, which.occasion = which.occasion, 
        show.sizes = show.sizes, show.exps = show.exps, dsf = dsf, 
        whole.population = whole.population, type=type)
}

plot.sample.capture.methods<-function (samp, which.occasion=0, show.sizes=TRUE, show.exps=TRUE, 
    dsf=0.75, whole.population=FALSE, type="freq") 
{
    if (!is.sample.cr(samp)) 
        stop(paste("\n*** The parameter <samp> must be an object of class 'sample.cr'\n"))
if(type=="locations") {
    pop <- samp$population
    des <- samp$design
    n.occ <- des$number.occasions
    if (!is.numeric(which.occasion)) 
        stop("\nThe parameter <which.occasion> must be numeric.\n")
    if (which.occasion != as.integer(which.occasion)) 
        stop("\nThe parameter <which.occasion> must be of integer type.\n")
    if (which.occasion < 0) 
        stop("\nThe parameter <which.occasion> must be at minimum zero.\n")
    if (which.occasion > n.occ) 
        stop(paste("\nThe parameter <which.occasion> cannot be greater than", 
            "the number of survey occasions.\n"))
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
    par.was <- par(no.readonly = TRUE)
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 cex<-0.75*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 plot(pop$region, reset.pars=FALSE)
    if (whole.population == TRUE) {
        plot.groups(pop, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="black")
    }
    if (which.occasion == 0) {
        i.min <- 1
        i.max <- n.occ
    }
    else {
        i.min <- 1
        i.max <- which.occasion
    }
    for (i in i.min:i.max) {
        inside <- (samp$capture[, i] == 1)
        if (any(inside)) {
            seen <- pop
            seen$groupID <- pop$groupID[inside]
            seen$posx <- pop$posx[inside]
            seen$posy <- pop$posy[inside]
            seen$groupsize <- pop$groupsize[inside]
            seen$types <- pop$types[inside]
            seen$exposure <- pop$exposure[inside]
            if ((i == which.occasion) | (which.occasion == 0)) {
                group.col <- "red"
            }
            else {
                group.col <- "blue"
            }
             plot.groups(seen, show.sizes=show.sizes, show.exps=show.exps, dsf=dsf, group.col="red")
        }
    }
    par(new = par.was$new)
}else {
 n.occ<-samp$design$number.occasions
 fr<-t(apply(samp$capture,1,cumsum))
 marked<-(fr>0)*1
 marked[,2:length(marked[1,])]<-marked[,1:(length(marked[1,])-1)]
 marked[,1]<-0
 fs<-apply(samp$capture,1,sum)
 m<-apply(samp$capture*marked, 2, sum)
 f<-rep(0,n.occ)
 for(i in 1:n.occ) f[i]<-length(which(fr[,n.occ]==i)) 
 n <- apply(samp$capture, 2, sum)
 old.par<-par(no.readonly=TRUE)
 cex<-0.75*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 par(mfrow=c(1,2))
 occasion<-1:n.occ
# capture data on each occasion:
 plot(occasion,n,type="l",ylim=c(0,1.3*max(n)),xlab="Capture occasion", ylab="Number of groups captured", main="")
 points(occasion,n, pch=19, cex=0.5, col="black")
 lines(occasion,m,col="red")
 points(occasion,m, pch=19, cex=0.5, col="red")
 lines(occasion,n-m,col="blue")
 points(occasion,n-m, pch=19, cex=0.5, col="blue")
 lines(c(1,n.occ),c(0,0),lty=2)
 labels<-c("Total captures", "Marked", "Unmarked")
 legend(1.5,1.05*max(n),labels,lty=1,col=c("black","red","blue"),xjust=0,yjust=0, cex=0.75*cex) 
# frequency of captures:
 plot(1:n.occ,f,type="l", ylim=c(0,max(f)), xlab="Number of times captured", ylab="Number of groups", main="")
 points(occasion,f, pch=19, cex=0.5, col="black")
 lines(c(1,n.occ),c(0,0),lty=2)
 par(old.par)
}
}

obscure.sample.cr<-function (samp) 
{
    if (!is.sample.cr(samp)) 
        stop("\n*** <samp> is not an object of type 'sample.cr'.\n")
    t <- samp
    detected <- apply(samp$capture, 1, sum)>0
    t$population$groupID <- samp$population$groupID[detected]
    t$population$posx <- samp$population$posx[detected]
    t$population$posy <- samp$population$posy[detected]
    t$population$groupsize <- samp$population$groupsize[detected]
    t$population$types <- samp$population$types[detected]
    t$population$exposure <- samp$population$exposure[detected]
    t$capture <- samp$capture[detected, ]
    t$created<-date()
    t
}


point.est.crM0 <- function (samp, init.N = -1, Chapmod = FALSE, numerical = TRUE) 
{
   cap <- samp$capture
   n.occ <- length(cap[1, ])
   parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
   Marked <- t(apply(cbind((cap[, 1] * 0), cap[, 1:(n.occ - 
       1)]), 1, cumsum))
   marked <- (Marked == 1 & cap == 1) * 1
   n <- apply(cap, 2, sum)
   M <- apply(Marked, 2, sum)
   m <- apply(marked, 2, sum)
   seen <- apply(cap, 1, sum) > 0
   Es <- mean(samp$population$groupsize[seen])
   Nhat.grp <- NA
   Nhat.ind <- NA
   phat <- NA
   transform.xtoNp <- function(x) {
       N <- exp(x[1]) + nall
       p <- exp(x[2])/(1 + exp(x[2]))
       return(c(N, p))
   }
   transform.Nptox <- function(Np) {
       x <- c(log(Np[1] - nall), log(Np[2]/(1 - Np[2])))
       return(x)
   }
   n.row <- nrow(obscure.sample.cr(samp)$capture)
   llk <- function(x) {
       N <- exp(x[1]) + nall
       p <- exp(x[2])/(1 + exp(x[2]))
       N.unmarkt <- N - cumsum(c(0, ns.unmarkt[-nocc]))   ###  Mike Merridith June 07
       temp3 <- (ns.unmarkt + ns.markt) * log(p)
       temp4 <- (N.unmarkt - ns.unmarkt + N.markt - ns.markt) * 
           log(1 - p)
       llk <- -((lgamma(N + 1) - lgamma(N - n.row + 1)) + sum(temp3 + 
           temp4))
       return(llk)
   }
   first.capture <- cap
   not.captured <- rep(TRUE, nrow(cap))
   for (j in 1:ncol(first.capture)) {
       first.capture[, j] <- first.capture[, j] & not.captured
       not.captured <- not.captured & !first.capture[, j]
   }
   markt.capture <- cap - first.capture
   ns.unmarkt <- apply(first.capture, 2, sum)
   nocc <- ncol(cap)
   ns.markt <- apply(markt.capture, 2, sum)
   N.markt <- c(0, cumsum(ns.unmarkt[1:(nocc - 1)]))
   nall <- sum(first.capture)
   N.Chap <- (n[1] + 1) * (n[2] + 1)/(m[2] + 1) - 1
   ifelse(init.N == -1, N.start <- nrow(cap) + 5, N.start <- init.N)
   if(numerical & N.start<nrow(cap)) {
     warning("init.N is less than number of captured animals (n).\n Program used n+5 as the starting value")
     N.start<-nrow(cap) + 5
   }
   startNp <- c(N.start, 0.5, 0.5)
   startx <- transform.Nptox(startNp)
### Use 'suppressWarnings' to stop incomprehensible messages (MM 3 June 07)
    suppressWarnings(res <- nlm(llk, startx))
   xhat <- res$estimate
   Np.hat <- transform.xtoNp(xhat)
   logLik <- -res$minimum
   resDev <- -2 * logLik
   AIC <- -2 * logLik + 2 * length(Np.hat)
   if (n.occ == 2 & numerical == FALSE) {
       if (Chapmod) 
           Nhat.grp <- (n[1] + 1) * (n[2] + 1)/(m[2] + 1) - 
               1
       else Nhat.grp <- n[1] * n[2]/m[2]
       ptothat <- (n[1] + n[2] - m[2])/Nhat.grp
       phat <- 1 - sqrt(1 - ptothat)
       Np.hat <- c(Nhat.grp, phat)
   }
   pointest <- list(sample=samp, Nhat.grp = Np.hat[1], Nhat.ind = Np.hat[1] * 
       Es, phat = Np.hat[2], Es = Es, log.Likelihood = logLik, 
       res.Deviance = resDev, AIC = AIC, parents=parents, created=date())
   class(pointest) <- "point.est.crM0"
   return(pointest)
}


is.point.est.crM0<-function (est) 
{
   inherits(est, "point.est.crM0")
}



summary.point.est.crM0<-function(est, digits=5) 
{
   if (!is.point.est.crM0(est)) stop("\nThe parameter <est> must be of class 'point.est.crM0'.\n")
   cat("\n")
   cat("POINT ESTIMATE SUMMARY (MARK-RECAPTURE METHOD M0)\n")
   cat("-------------------------------------------------\n")
   cat("creation date   :", est$created,"\n")
   cat("parent object(s) (class, name, creation date):\n")
   for(i in 1:length(est$parents)) {
      cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
   }
   if(is.numeric(est$seed)) cat("random number seed used: ",est$seed,"\n")
   cat("\n")
   n.occ<-est$sample$design$number.occasions
   fr<-t(apply(est$sample$capture,1,cumsum))
   marked<-(fr>0)*1
   marked[,2:length(marked[1,])]<-marked[,1:(length(marked[1,])-1)]
   marked[,1]<-0
   fs<-apply(est$sample$capture,1,sum)
   m<-apply(est$sample$capture*marked, 2, sum)
   f<-rep(0,n.occ)
   for(i in 1:n.occ) f[i]<-length(which(fr[,n.occ]==i)) 
   n <- apply(est$sample$capture, 2, sum)
   cat("Number of survey occasions                  :", n.occ, "\n\n")
   cat("Total number of captured groups             :", sum(n-m), "\n")
   cat("Number captured on each occasion            :", n, "\n")
   cat("Number of marked captured on each occasion  :", m, "\n")
   cat("Number of unmarked captured on each occasion:", n-m, "\n")
   cat("Capture frequencies:\n")
   for(i in 1:n.occ) if(i==1)
   cat(paste("                       Number caught once   :",f[i],"\n", sep=""))
   else
   cat(paste("                       Number caught ",i," times:",f[i],"\n", sep=""))
   cat("\n")
   cat("Estimated number of groups                  :", round(est$Nhat.grp), "\n")
   cat("Estimated number of individuals             :", round(est$Nhat.ind), "\n")
   cat("Estimated mean group size                   :", signif(est$Es,digits), "\n")
   cat("Estimated capture probability               :",signif(est$phat,digits),"\n")
   cat("\n")
   cat("Residual deviance                           :", est$res.Deviance, "\n")
   cat("log(Likelihood)                             :", est$log.Likelihood, "\n")
   cat("AIC                                         :", est$AIC, "\n")
}



plot.point.est.crM0<-function(x,col="black")
{
 plot.text("No plot has been implemented for this class of object",col=col)
}



point.est.crMb<- function (samp, init.N = -1) {
   parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
   chall <- samp$capture
   seen <- apply(chall, 1, sum) > 0
   ch <- chall[seen, ]
   nocc <- ncol(ch)
   nall <- nrow(ch)
   n <- apply(ch, 2, sum)
   Marked <- t(apply(cbind((ch[, 1] * 0), ch[, 1:(nocc - 1)]), 
       1, cumsum)) > 0
   marked <- (Marked & ch == 1)
   m <- apply(marked, 2, sum)
   u <- n - m
   M <- apply(Marked, 2, sum)
   Es <- mean(samp$population$groupsize[seen])
   n.row <- nrow(ch)
   llk <- function(x) {
      N <- exp(x[1]) + nall
      pu <- exp(x[2])/(1 + exp(x[2]))
      pm <- exp(x[3])/(1 + exp(x[3]))
      U <- N - M
      temp3 <- u * log(pu)
      temp4 <- m * log(pm)
      temp5 <- (U - u) * log(1 - pu)
      temp6 <- (M - m) * log(1 - pm)
      llk <- -((lgamma(N + 1) - lgamma(N - n.row + 1)) + sum(temp3 + 
           temp4 + temp5 + temp6))
      return(llk)
   }   
   transform.xtoNp <- function(x) {
      N <- exp(x[1]) + nall
      pu <- exp(x[2])/(1 + exp(x[2]))
      pm <- exp(x[3])/(1 + exp(x[3]))
      return(c(N, pu, pm))
   }
   transform.Nptox <- function(Np) {
      x <- c(log(Np[1] - nall), log(Np[2]/(1 - Np[2])), log(Np[3]/(1 - 
         Np[3])))
      return(x)
   }
   ifelse(init.N == -1, N.start <- nrow(ch) + 5, N.start <- init.N)
   if(N.start<nall) {
     warning("init.N is less than number of captured animals (n).\n Program used n+5 as the starting value")
     N.start<-nrow(cap) + 5
   }
   startNp <- c(nrow(ch) + 5, 0.5, 0.5)
   startx <- transform.Nptox(startNp)
### Use 'suppressWarnings' to stop incomprehensible messages (MM 3 June 07)
    suppressWarnings(res <- nlm(llk, startx))
   xhat <- res$estimate
   Nphat <- transform.xtoNp(xhat)
   logLik <- -res$minimum
   resDev <- -2 * logLik
   AIC <- -2 * logLik + 2 * length(Nphat)
   phat <- matrix(Nphat[2:3], nrow = 2, ncol = 1, dimnames = list(c("unmarked", 
       "marked"), ""))
   pointest <- list(sample=samp, Nhat.grp = Nphat[1], Nhat.ind = Nphat[1] * 
       Es, phat = phat, Es = Es, log.Likelihood = logLik, res.Deviance = resDev, 
       AIC = AIC, init.N=init.N, parents=parents, created=date())
   class(pointest) <- "point.est.crMb"
   return(pointest)
}  

is.point.est.crMb<-function (est) 
{
    inherits(est, "point.est.crMb")
}


summary.point.est.crMb<-function(est, digits=5) 
{
 if (!is.point.est.crMb(est)) stop("\nThe parameter <est> must be of class 'point.est.crMb'.\n")
 cat("\n")
 cat("POINT ESTIMATE SUMMARY (MARK-RECAPTURE METHOD Mb)\n")
 cat("-------------------------------------------------\n")
 cat("creation date   :", est$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(est$parents)) {
   cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(est$seed)) cat("random number seed used: ",est$seed,"\n")
 cat("\n")
 n.occ<-est$sample$design$number.occasions
 fr<-t(apply(est$sample$capture,1,cumsum))
 marked<-(fr>0)*1
 marked[,2:length(marked[1,])]<-marked[,1:(length(marked[1,])-1)]
 marked[,1]<-0
 fs<-apply(est$sample$capture,1,sum)
 m<-apply(est$sample$capture*marked, 2, sum)
 f<-rep(0,n.occ)
 for(i in 1:n.occ) f[i]<-length(which(fr[,n.occ]==i)) 
 n <- apply(est$sample$capture, 2, sum)
 cat("Number of survey occasions                  :", n.occ, "\n\n")
 cat("Total number of captured groups             :", sum(n-m), "\n")
 cat("Number captured on each occasion            :", n, "\n")
 cat("Number of marked captured on each occasion  :", m, "\n")
 cat("Number of unmarked captured on each occasion:", n-m, "\n")
 cat("Capture frequencies:\n")
 for(i in 1:n.occ) if(i==1)
   cat(paste("                       Number caught once   : ",f[i],"\n", sep=""))
 else
   cat(paste("                       Number caught ",i," times: ",f[i],"\n", sep=""))
 cat("\n")
 cat("Estimated number of groups                  :", round(est$Nhat.grp), "\n")
 cat("Estimated number of individuals             :", round(est$Nhat.ind), "\n")
 cat("Estimated mean group size                   :", signif(est$Es,digits), "\n")
 cat("Estimated capture probabilities             :",signif(est$phat,digits),"\n")
 cat("\n")
 if(est$init.N>0) cat("Starting value for N in numerical search   :", est$init.N,"\n")
 cat("Residual deviance                           :", est$res.Deviance, "\n")
 cat("log(Likelihood)                             :", est$log.Likelihood, "\n")
 cat("AIC                                         :", est$AIC, "\n")
}



plot.point.est.crMb<-function(x,col="black")
{
 plot.text("No plot has been implemented for this class of object",col=col)
}



point.est.crMt<- function (samp, init.N = -1) {
    parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
    chall <- samp$capture
    seen <- apply(chall, 1, sum) > 0
    ch <- chall[seen, ]
    nocc <- ncol(ch)
    nall <- nrow(ch)
    n <- apply(ch, 2, sum)
    Marked <- t(apply(cbind((ch[, 1] * 0), ch[, 1:(nocc - 1)]), 
        1, cumsum)) > 0
    marked <- (Marked & ch == 1)
    m <- apply(marked, 2, sum)
    u <- n - m
    M <- apply(Marked, 2, sum)
    Es <- mean(samp$population$groupsize[seen])
    boot.dbn <- NULL
    boot.mean <- NULL
    n.row <- nrow(ch)
    llk <- function(x) {
       lx <- length(x)
       N <- exp(x[1]) + nall
       p <- exp(x[2:lx])/(1 + exp(x[2:lx]))
       U <- N - M
       temp3 <- u * log(p)
       temp4 <- m * log(p)
       temp5 <- (U - u) * log(1 - p)
       temp6 <- (M - m) * log(1 - p)
       llk <- -((lgamma(N + 1) - lgamma(N - n.row + 1)) + sum(temp3 + 
           temp4 + temp5 + temp6))
       return(llk)
    }
    transform.xtoNp <- function(x) {
       lx <- length(x)
       N <- exp(x[1]) + nall
       p <- exp(x[2:lx])/(1 + exp(x[2:lx]))
       return(c(N, p))
    }
    transform.Nptox <- function(Np) {
       lx <- length(Np)
       x <- c(log(Np[1] - nall), log(Np[2:lx]/(1 - Np[2:lx])))
       return(x)
    }
    ifelse(init.N == -1, N.start <- nrow(ch) + 5, N.start <- init.N)
   if(N.start<nall) {
     warning("init.N is less than number of captured animals (n).\n Program used n+5 as the starting value")
     N.start<-nrow(cap) + 5
   }
    startNp <- c(N.start, rep(0.5, nocc))
    startx <- transform.Nptox(startNp)
### Use 'suppressWarnings' to stop incomprehensible messages (MM 3 June 07)
    suppressWarnings(res <- nlm(llk, startx))
    xhat <- res$estimate
    Nphat <- transform.xtoNp(xhat)
    logLik <- -res$minimum
    resDev <- -2 * logLik
    AIC <- -2 * logLik + 2 * length(Nphat)
    phat <- matrix(Nphat[2:(nocc + 1)], nrow = nocc, ncol = 1, 
        dimnames = list(occasion = 1:nocc, ""))
    pointest <- list(sample=samp, Nhat.grp = Nphat[1], Nhat.ind = Nphat[1] * 
        Es, phat = phat, Es = Es, log.Likelihood = logLik, res.Deviance = resDev, 
        AIC = AIC, init.N=init.N, parents=parents, created=date())
    class(pointest) <- "point.est.crMt"
    return(pointest)
}

is.point.est.crMt<-function (est) 
{
   inherits(est, "point.est.crMt")
}


summary.point.est.crMt<-function(est, digits=5) 
{
   if (!is.point.est.crMt(est)) stop("\nThe parameter <est> must be of class 'point.est.crMt'.\n")
   cat("\n")
   cat("POINT ESTIMATE SUMMARY (MARK-RECAPTURE METHOD Mt)\n")
   cat("-------------------------------------------------\n")
   cat("creation date   :", est$created,"\n")
   cat("parent object(s) (class, name, creation date):\n")
   for(i in 1:length(est$parents)) {
      cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
   }
   if(is.numeric(est$seed)) cat("random number seed used: ",est$seed,"\n")
   cat("\n")
   n.occ<-est$sample$design$number.occasions
   fr<-t(apply(est$sample$capture,1,cumsum))
   marked<-(fr>0)*1
   marked[,2:length(marked[1,])]<-marked[,1:(length(marked[1,])-1)]
   marked[,1]<-0
   fs<-apply(est$sample$capture,1,sum)
   m<-apply(est$sample$capture*marked, 2, sum)
   f<-rep(0,n.occ)
   for(i in 1:n.occ) f[i]<-length(which(fr[,n.occ]==i)) 
   n <- apply(est$sample$capture, 2, sum)
   cat("Number of survey occasions                  :", n.occ, "\n\n")
   cat("Total number of captured groups             :", sum(n-m), "\n")
   cat("Number captured on each occasion            :", n, "\n")
   cat("Number of marked captured on each occasion  :", m, "\n")
   cat("Number of unmarked captured on each occasion:", n-m, "\n")
   cat("Capture frequencies:\n")
   for(i in 1:n.occ) if(i==1)
   cat(paste("                       Number caught once   :",f[i],"\n", sep=""))
   else
   cat(paste("                       Number caught ",i," times:",f[i],"\n", sep=""))
   cat("\n")
   cat("Estimated number of groups                  :", round(est$Nhat.grp), "\n")
   cat("Estimated number of individuals             :", round(est$Nhat.ind), "\n")
   cat("Estimated mean group size                   :", signif(est$Es,digits), "\n")
   cat("Estimated capture probabilities             :",signif(est$phat,digits),"\n")
   cat("\n")
   if(est$init.N>0) cat("Starting value for N in numerical search   :", est$init.N,"\n")
   cat("Residual deviance                           :", est$res.Deviance, "\n")
   cat("log(Likelihood)                             :", est$log.Likelihood, "\n")
   cat("AIC                                         :", est$AIC, "\n")
}



plot.point.est.crMt<-function(x,col="black")
{
   plot.text("No plot has been implemented for this class of object",col=col)
}




point.est.crMh<-function (samp, num.mix = 2, init.N = -1) 
{
 if (!is.sample.cr(samp) && !samp$bootsample.cr) 
   stop("\n*** Argument <samp> must be an object of type 'sample.rm'.\n")
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
#  Pledger's functions that are called in the main point.est.crMh function:
    freq.gen <- function(captmat) {
        n.occ <- ncol(captmat)
        freq.val <- rep(0, n.occ)
        for (i in 1:n.occ) {
            freq.val[i] <- sum(apply(captmat, 1, sum) == i)
        }
        freq.val
    }
    minusll.m0 <- function(invect) {
        popno <- invect[1]
        pcapt <- capt/popno/ntimes
        loglik <- lgamma(popno + 1) - lgamma(popno - anim + 1) + 
            capt * log(pcapt) + (ntimes * popno - capt) * log(1 - 
            pcapt)
        -loglik
    }
    minusll.mhg <- function(invect) {
        popn <- invect[1]
        alpha.in <- invect[2:(g + 1)]
        pi.in <- invect[(g + 2):(2 * g)]
        theta.v <- (cumprod(alpha.in[g:1]))[g:1]
        pi.v <- c(pi.in, 1 - sum(pi.in))
        f.vect <- c(popn - anim, freq.vect)
        if (((min(theta.v) <= 0) | (max(theta.v) > 1)) | ((min(pi.v) < 
            0) | (max(pi.v) > 1))) 
            loglik <- -10^8
        else {
            loglik <- lgamma(popn + 1) - lgamma(popn - anim + 
                1)
            for (i in 0:ntimes) {
                term <- sum(pi.v * theta.v^i * (1 - theta.v)^(ntimes - 
                  i))
                loglik <- loglik + f.vect[i + 1] * log(term)
            }
        }
        -loglik
    }
# MAIN CODE:  
#  Calculate expected group size by taking the simple mean of the groupsizes of the D observed animals. 
    seen <- apply(samp$capture, 1, sum) > 0
    Es <- mean(samp$population$groupsize[seen])
#  Pledger's code:
    Gmax <- num.mix
    freq.vect <- freq.gen(samp$capture)
    ntimes <- length(freq.vect)
    anim <- sum(freq.vect)
    capt <- sum((1:ntimes) * freq.vect)
# Set up the output matrices, for info, capture probs and proportions.
    models.list <- c("M0", "Mh2", "Mh3", "Mh4", "Mh5", "Mh6", "Mh7", "Mh8", "Mh9", "Mh10")
    info.mat <- matrix(0, Gmax, 8)
    dimnames(info.mat) <- list(models.list[1:Gmax], c("Max.ll", 
        "Res.Dev.", "npar", "AIC?", "Rel.AIC?", "N.hat", "se(N.hat)", 
        "Mean p"))
    captprobs.mat <- matrix(NA, Gmax, Gmax)
    dimnames(captprobs.mat) <- list(models.list[1:Gmax], models.list[1:Gmax])
    proportions.mat <- matrix(NA, Gmax, Gmax)
    dimnames(proportions.mat) <- list(models.list[1:Gmax], models.list[1:Gmax])
#  Store results from fitting the M0 model so that the Nhat estimate from M0 can be used to 
#  determine the starting value in the optimisation routine when fitting the Mgh models.
   if(init.N<=nrow(samp$capture)& init.N!= -1) {
     warning("init.N less than or equal to the number of captured animals (n).\n Program used init.N=-1 as the starting value")
   }
    ifelse(init.N <= nrow(samp$capture), init.N <- -1, init.N <- init.N)
    mo.res <- point.est.crM0(samp, init.N = init.N)
    info.mat[1, -5] <- round(c(mo.res$log.Likelihood, mo.res$res.Deviance, 
        2, mo.res$AIC, mo.res$Nhat.grp, NA, mo.res$phat), 4)
    captprobs.mat[1, 1] <- round(mo.res$phat, 4)
    proportions.mat[1, 1] <- 1
#  Fit Mhg model (g>1)
    for (g in 2:Gmax) {
        ifelse(init.N == -1, N.start <- max(info.mat[g - 1, 6], 
            anim + 5), N.start <- init.N)
        last.theta <- captprobs.mat[(g - 1), (1:(g - 1))]
        theta.start <- c(last.theta * 0.9, last.theta[g - 1])
        alpha.start <- theta.start/c(theta.start[-1], 1)
        last.pi <- proportions.mat[(g - 1), (1:(g - 1))]
        pi.start <- last.pi * (1 - 1/g)
        start.vect <- c(N.start, alpha.start, pi.start)
        # Fit the current model:
        mhg.fit <- optim(par = c(N.start, alpha.start, pi.start), 
            fn = minusll.mhg, method = "L-BFGS-B", lower = c(anim, 
                rep(1e-04, (2 * g - 1))), upper = c(Inf, rep(0.9999, 
                (2 * g - 1))), hessian = TRUE)
        # Save the results:
        max.ll <- -mhg.fit$value
        res.dev <- -2 * max.ll
        npar <- 2 * g
        AIC <- res.dev + 2 * npar
        N.hat <- mhg.fit$par[1]
        alpha.out <- mhg.fit$par[2:(g + 1)]
        pi.out <- mhg.fit$par[(g + 2):(2 * g)]
        theta.hat <- (cumprod(alpha.out[g:1]))[g:1]
        pi.hat <- c(pi.out, 1 - sum(pi.out))
        mean.p <- sum(pi.hat * theta.hat)
        hess <- mhg.fit$hessian
        se.Nhat <- NA
        if (det(hess) > 0) 
            se.Nhat <- sqrt(solve(hess)[1, 1])
        info.mat[g, ] <- round(c(max.ll, res.dev, npar, AIC, 
            0, N.hat, se.Nhat, mean.p), 4)
        captprobs.mat[g, 1:g] <- round(theta.hat, 4)
        proportions.mat[g, 1:g] <- round(pi.hat, 4)
        mhg.phat <- matrix(theta.hat, nrow = g, ncol = 1, dimnames = list(group = 1:g, 
            ""))
    }
    # Calculate the relative AIC:
    info.mat[, 5] <- info.mat[, 4] - min(info.mat[, 4])
    mhg.out <- list(info = info.mat, captprobs = captprobs.mat, 
        proportions = proportions.mat)
    Nhat.grp <- NA
    Nhat.ind <- NA
    phat <- NA
    ppnhat<-mhg.phat #just to get the right column names
    ppnhat[1:g]<-proportions.mat[g,1:g]
    pointest <- list(sample=samp, Nhat.grp=info.mat[g, 6], Nhat.ind=info.mat[g,6]*Es, phat=mhg.phat, proportions=ppnhat,Es=Es, 
                    log.Likelihood=info.mat[g,1], res.Deviance = info.mat[g, 2], AIC = info.mat[g,4], init.N=init.N, parents=parents, created=date())
    class(pointest) <- "point.est.crMh"
    return(pointest)
}


is.point.est.crMh<-function (est) 
{
    inherits(est, "point.est.crMh")
}


summary.point.est.crMh<-function(est, digits=5) 
{
 if (!is.point.est.crMh(est)) stop("\nThe parameter <est> must be of class 'point.est.crMh'.\n")
 cat("\n")
 cat("POINT ESTIMATE SUMMARY (MARK-RECAPTURE METHOD Mh)\n")
 cat("-------------------------------------------------\n")
 cat("creation date   :", est$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(est$parents)) {
   cat("      ",paste("(",est$parents[[i]]$class,", ",est$parents[[i]]$name,", ",est$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(est$seed)) cat("random number seed used: ",est$seed,"\n")
 cat("\n")
 n.occ<-est$sample$design$number.occasions
 fr<-t(apply(est$sample$capture,1,cumsum))
 marked<-(fr>0)*1
 marked[,2:length(marked[1,])]<-marked[,1:(length(marked[1,])-1)]
 marked[,1]<-0
 fs<-apply(est$sample$capture,1,sum)
 m<-apply(est$sample$capture*marked, 2, sum)
 f<-rep(0,n.occ)
 for(i in 1:n.occ) f[i]<-length(which(fr[,n.occ]==i)) 
 n <- apply(est$sample$capture, 2, sum)
 cat("Number of survey occasions                  :", n.occ, "\n\n")
 cat("Total number of captured groups             :", sum(n-m), "\n")
 cat("Number captured on each occasion            :", n, "\n")
 cat("Number of marked captured on each occasion  :", m, "\n")
 cat("Number of unmarked captured on each occasion:", n-m, "\n")
 cat("Capture frequencies:\n")
 for(i in 1:n.occ) if(i==1)
   cat(paste("                       Number caught once   : ",f[i],"\n", sep=""))
 else
   cat(paste("                       Number caught ",i," times: ",f[i],"\n", sep=""))
 cat("\n")
 cat("Estimated number of groups                  :", round(est$Nhat.grp), "\n")
 cat("Estimated number of individuals             :", round(est$Nhat.ind), "\n")
 cat("Estimated mean group size                   :", signif(est$Es,digits), "\n")
 cat("Estimated capture probabilities             :",signif(est$phat,digits),"\n")
 cat("Estimated mixture proportions               :",signif(est$proportions,digits),"\n")
 cat("\n")
 if(est$init.N>0) cat("Starting value for N in numerical search  :", est$init.N,"\n")
 cat("Residual deviance                           :", est$res.Deviance, "\n")
 cat("log(Likelihood)                             :", est$log.Likelihood, "\n")
 cat("AIC                                         :", est$AIC, "\n")
}



plot.point.est.crMh<-function(x,col="black")
{
 plot.text("No plot has been implemented for this class of object",col=col)
}

int.est.crM0<-function (samp, init.N=-1, ci.type="boot.nonpar", nboot=999, 
    vlevels=c(0.025, 0.975), plot=FALSE, Chapmod=FALSE, numerical=FALSE, seed=NULL) 
{
 if(ci.type!="boot.nonpar" & ci.type!="boot.par") stop("Only bootstrap CI estimation implemented so far.\n")
 if (!is.sample.cr(samp)) 
   stop("\n*** <samp> is not an object of type 'sample.cr'.\n")
 if (!(ci.type %in% c("boot.par", "boot.nonpar", "normal", "lognormal", "profile"))) 
   stop(paste("\n*** Unrecognised <ci.type>. These are valid:", 
        "'boot.par', 'boot.nonpar', 'profile', 'normal', and 'lognormal' \n"))
 if (!is.numeric(vlevels)) 
   stop("\n*** All <vlevels> values must be numeric.\n")
 if (any(vlevels < 0) | any(vlevels > 1)) 
   stop("\n*** All <vlevels> values must be between 0 and 1.\n")
 if (!is.numeric(nboot)) 
   stop("\n*** <nboot> must be numeric.\n")
 if (nboot < 2) 
   stop("\n*** <nboot> must be at least 2.\n")
 if ((plot != TRUE) & (plot != FALSE)) 
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
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
 cap <- samp$capture
 n.occ <- ncol(cap)
 res <- point.est.crM0(samp, init.N=init.N, Chapmod=Chapmod, numerical=numerical)
 Marked <- t(apply(cbind((cap[, 1] * 0), cap[, 1:(n.occ - 1)]), 1, cumsum))
 marked <- (Marked == 1 & cap == 1) * 1
 n <- apply(cap, 2, sum)
 M <- apply(Marked, 2, sum)
 m <- apply(marked, 2, sum)
 Nhat <- round(res$Nhat.grp)
 phat <- res$phat
 seen <- apply(cap, 1, sum) > 0
 groupsize <- samp$population$groupsize[seen]
 Es <- mean(groupsize)
 boot.dbn <- NULL
 boot.mean <- NULL
 civec <- rep(NA, length(vlevels))
 ci <- list(Nhat.grp=civec, Nhat.ind=civec, phat=civec, Es = civec)
 if (n.occ == 2 & !numerical) {
   seDeltaprod <- function(mean, var) {
     sqrt(sum(var/(mean^2))) * prod(mean)
   }
   if (ci.type == "normal") {
     VarNhat <- (n[1]+1)*(n[2]+1)*(n[1]-m[2])*(n[2]-m[2])/((m[2]+1)^2*(m[2]+1))
     ci$Nhat.grp <- Nhat + qnorm(vlevels) * sqrt(VarNhat)
     ci$Nhat.ind <- Nhat * Es + qnorm(vlevels) * seDeltaprod(c(Nhat,Es), c(VarNhat, var(groupsize)))
     ci$Es <- Es + qnorm(vlevels) * sqrt(var(groupsize))
   }
   if (ci.type == "lognormal") {
     VarNhat <- (n[1] + 1) * (n[2] + 1) * (n[1] - m[2]) * 
                (n[2] - m[2])/((m[2] + 1)^2 * (m[2] + 1))
     VarlogNhat <- log(1 + VarNhat/Nhat^2)
     VarlogEs <- log(1 + var(groupsize)/Es^2)
     ci$Nhat.grp <- exp(log(Nhat) + qnorm(vlevels) * sqrt(VarlogNhat))
     ci$Nhat.ind <- exp(log(Nhat * Es) + qnorm(vlevels) * 
     sqrt(log(1 + (seDeltaprod(c(Nhat, Es), c(VarNhat,var(groupsize))))^2/(Nhat * Es)^2)))
     ci$Es <- exp(log(Es) + qnorm(vlevels) * sqrt(VarlogEs))
   }
   if (ci.type == "profile") {
     if (abs(vlevels[1] - (1 - vlevels[length(vlevels)])) > 1e-07) 
       stop("For profile likelihood, you need symmetric vlevels (e.g. vlevels=c(0.025,0.975) for a 95% CI)")
     nn <- sum(n - m)
     llk <- function(N, p) {
       temp1 <- lgamma(N + 1) - sum(lgamma(m + 1)) - lgamma(N - nn + 1)
       temp2 <- sum(n) * log(p) + (2 * N - sum(n)) * log(1 - p)
       llk <- -sum(temp1 + temp2)
       return(llk)
     }
     llkmin <- llk(Nhat, phat)
       W <- 0
       N <- Nhat
       conf.level <- vlevels[length(vlevels)] - vlevels[1]
       q <- qchisq(conf.level, 1)/2
       while (W < q) {
         p <- (n[1] + n[2])/(2 * N)
         W <- llk(N, p) - llkmin
           if (W < q) N <- N - 1
       }
       Nlo <- N
       W <- 0
       N <- Nhat
       while (W < q) {
         p <- (n[1] + n[2])/(2 * N)
         W <- llk(N, p) - llkmin
         if (W < q) N <- N + 1
       }
       Nhi <- N
       ci$Nhat.grp[1] <- Nlo
       ci$Nhat.grp[length(vlevels)] <- Nhi
     }
   }
   else if (ci.type == "normal" | ci.type == "lognormal" | ci.type == "profile") 
        stop("\nNormal, lognormal, profile likelihood CI only implemented for 2 sampling occasions, Petersen or Chapman.\n")
   if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
     b.Nhat.grp <- rep(0, nboot)
     b.Nhat.ind <- rep(0, nboot)
     b.phat <- rep(0, nboot)
     b.Es <- rep(0, nboot)
     b.samp <- samp
   }
   if (ci.type == "boot.nonpar") {
     cap <- samp$capture
     seen <- apply(cap, 1, sum) > 0
     cap <- cap[seen, ]
     n0 <- Nhat - nrow(cap)
     size <- c(samp$population$groupsize[seen], rep(0, n0))
     zero <- matrix(rep(0, n0 * ncol(cap)), ncol = ncol(cap))
     cap <- matrix(c(t(cap), zero), ncol = ncol(cap), byrow = TRUE)
     for (i in 1:nboot) {
       index <- sample(1:Nhat, Nhat, replace = TRUE)
       b.samp$capture <- cap[index, ]
       b.samp$population$groupsize <- size[index]
       est <- point.est.crM0(b.samp, init.N=init.N, Chapmod=Chapmod, numerical=numerical)
       b.Nhat.grp[i] <- est$Nhat.grp
       b.Nhat.ind[i] <- est$Nhat.ind
       b.phat[i] <- est$phat
       b.Es[i] <- est$Es
     }
   }
   if (ci.type == "boot.par") {
     for (i in 1:nboot) {
       boot.result <- rbinom(n.occ * Nhat, 1, phat)
       b.samp$capture <- matrix(boot.result, ncol = n.occ)
       est <- point.est.crM0(b.samp, init.N=init.N, Chapmod=Chapmod, numerical=numerical)
       b.Nhat.grp[i] <- est$Nhat.grp
       b.Nhat.ind[i] <- est$Nhat.ind
       b.phat[i] <- est$phat
       b.Es[i] <- -1
     }
   }
   if (ci.type == "boot.par" | ci.type == "boot.nonpar") {
        boot.dbn <- list(Nhat.grp=b.Nhat.grp, Nhat.ind=b.Nhat.ind, phat=b.phat, Es=b.Es)
   valid<-(b.Nhat.grp != Inf & boot.dbn[[1]] > 0)
        boot.mean <- list(Nhat.grp = mean(b.Nhat.grp[valid]), Nhat.ind = mean(b.Nhat.ind[valid]), 
            phat = mean(b.phat[valid]), Es = mean(b.Es[valid]))
   se <- list(Nhat.grp=sqrt(var(b.Nhat.grp[valid])), Nhat.ind=sqrt(var(b.Nhat.ind[valid])), 
            phat=sqrt(var(b.phat[valid])), Es=sqrt(var(b.Es[valid])))
   cv<-list(Nhat.grp=se$Nhat.grp/boot.mean$Nhat.grp , Nhat.ind=se$Nhat.ind/boot.mean$Nhat.ind, 
            phat=se$phat/boot.mean$phat, Es=se$Es/boot.mean$Es)
        for (i in 1:length(ci)) {
            sort.est <- sort(boot.dbn[[i]])
            cin <- round(nboot * vlevels, 0)
            cin <- ifelse(cin < 1, 1, cin)
            ci[[i]] <- sort.est[cin]
        }
   }
# Profile likelihood plot stuff  from old int.est.crM0 (kept here to implement later)
#   if (plot == T & ci.type == "profile") {
#     Nleft <- round(Nhat + 1.5 * (ci$Nhat.grp[1] - Nhat))
#     Nright <- round(Nhat + 1.5 * (ci$Nhat.grp[length(vlevels)] - Nhat))
#    W <- rep(0, (Nleft - Nleft + 1))
#     for (N in Nleft:Nright) {
#       p <- (n[1] + n[2])/(2 * N)
#       W[N - Nleft + 1] <- llk(N, p) - llkmin
#     }
#     crit <- q * (-1)
#        N <- c(Nleft:Nright)
#        old.par <- par(no.readonly = TRUE)
#        par(plt = c(0.2, 0.9, 0.65, 0.9))
#        plot(N, -W, type = "l", xlab = "N", ylab = "W(N)/2")
#        lines(c(Nleft, Nright), c(crit, crit), lty = 4)
#        yshift <- (max(W) - min(W)) * 0.05
#       xshift <- (Nright - Nleft) * 0.025
#       text((ci$Nhat.grp[1] - xshift), (crit + yshift), labels = as.character(ci$Nhat.grp[1]))
#       text((ci$Nhat.grp[length(vlevels)] + xshift), (crit + 
#           yshift), labels = as.character(ci$Nhat.grp[length(vlevels)]))
#       par(old.par)
#   }
    intest <- list(levels=vlevels, ci=ci, boot.mean=boot.mean, boot.dbn=boot.dbn, init.N=init.N, Chapmod=Chapmod, 
           numerical=numerical, se=se, cv=cv, ci.type=ci.type, parents=parents, created=date(), seed=seed)
    class(intest) <- "int.est.crM0"
  if(plot) plot(intest, type="hist")
    return(intest)
}


is.int.est.crM0<-function(est)
{
 inherits(est, "int.est.crM0")
}


summary.int.est.crM0<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "phat"), digits=5)
{
 if(!is.int.est.crM0(iest)) 
   stop("Argument <iest>. must be of class int.est.crM0\n")
 addtext1<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 addtext2<-paste("Chapman's estimator?              : ",iest$Chapmod,"\n",sep="")
 addtext3<-paste("Numerical estimation?             : ",iest$numerical,"\n",sep="")
 addtext4<-paste("Initial N for numerical search    : ",iest$init.N,"\n",sep="")
 addtext<-paste(addtext1, addtext2, addtext3, addtext4, sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.crM0<-function(iest, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}

int.est.crMt<-function (samp, init.N = -1, ci.type = "boot.nonpar", nboot = 999, 
    vlevels = c(0.025, 0.975), plot = FALSE, seed=NULL) 
{
 if(ci.type!="boot.nonpar" & ci.type!="boot.par") stop("Only bootstrap CI estimation implemented so far.\n")
    if (!is.sample.cr(samp)) 
        stop("\n*** <samp> is not an object of type 'sample.crMb'.\n")
    if (!(ci.type %in% c("boot.nonpar", "boot.par"))) 
        stop(paste("\n*** Unrecognised <ci.type>. Valid options: ", 
            "'boot.par' and 'boot.nonpar'\n"))
    if (!is.numeric(vlevels)) 
        stop("\n*** All <vlevels> values must be numeric.\n")
    if (any(vlevels < 0) | any(vlevels > 1)) 
        stop("\n*** All <vlevels> values must be between 0 and 1.\n")
    if (!is.numeric(nboot)) 
        stop("\n*** <nboot> must be numeric.\n")
    if (nboot < 1) 
        stop("\n*** <nboot> must be at least 1.\n")
    if ((plot != TRUE) & (plot != FALSE)) 
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
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
    chall <- samp$capture
    seen <- apply(chall, 1, sum) > 0
    ch <- chall[seen, ]
    groupsize <- samp$population$groupsize[seen]
    n.occ <- ncol(ch)
    nall <- nrow(ch)
    res <- point.est.crMt(samp, init.N)
    N.hat <- round(res$Nhat.grp)
    p.hat <- res$phat
    maxest <- 100 * N.hat
    if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
        b.Nhat.grp <- rep(0, nboot)
        b.Nhat.ind <- rep(0, nboot)
#        b.phat <- matrix(0, (nboot * n.occ), nrow = nboot, ncol = n.occ, 
#            dimnames = list(replicate = 1:nboot, occasion = 1:n.occ))
###				comma problem fixed, ER June 2007
   b.phat <- matrix(rep(0, nboot), nrow=nboot, ncol=n.occ, dimnames=list(replicate=1:nboot, paste("occasion",1:n.occ)))
        b.Es <- rep(0, nboot)
        b.samp <- samp
        b.reg <- generate.region()
        b.dens <- generate.density(b.reg)
        b.poppars <- setpars.population(b.dens, number.groups = N.hat)
    }
    if (ci.type == "boot.nonpar") {
        b.pop <- generate.population(b.poppars)
        b.des <- generate.design.cr(b.reg, n.occ = n.occ)
        b.survpars <- setpars.survey.cr(b.pop, b.des, pmin.unmarked = 0.5)
        b.samp <- generate.sample.cr(b.survpars)
        n0 <- N.hat - nall
        cap <- matrix(0, N.hat, n.occ)
        cap[1:nall, ] <- ch
        grpsize <- rep(0, N.hat)
        grpsize[1:nall] <- groupsize
        for (i in 1:nboot) {
            repeat {
                index <- sample(1:N.hat, N.hat, replace = TRUE)
                b.samp$capture <- cap[index, ]
                b.samp$population$groupsize <- grpsize[index]
                est <- point.est.crMt(b.samp, init.N)
                b.Nhat.grp[i] <- est$Nhat.grp
                b.Nhat.ind[i] <- est$Nhat.ind
                b.phat[i, ] <- est$phat
                b.Es[i] <- est$Es
                if (est$Nhat.grp < maxest) 
                  break
            }
            b.Nhat.grp[i] <- max(est$Nhat.grp, nall)
            b.Nhat.ind[i] <- b.Nhat.grp[i] * b.Es[i]
        }
    }
    if (ci.type == "boot.par") {
        b.pop <- generate.population(b.poppars)
        b.des <- generate.design.cr(b.reg, n.occ = n.occ, effort = log(1 - 
            p.hat)/log(1 - p.hat[1]))
        b.survpars <- setpars.survey.cr(b.pop, b.des, pmin.unmarked = p.hat[1])
        for (i in 1:nboot) {
            repeat {
                b.samp <- generate.sample.cr(b.survpars)
                est <- point.est.Mt(b.samp, init.N)
                b.Nhat.grp[i] <- est$Nhat.grp
                b.Nhat.ind[i] <- est$Nhat.ind
                b.phat[i, ] <- est$phat
                b.Es[i] <- -1
                if (est$Nhat.grp < maxest) 
                  break
            }
            b.Nhat.grp[i] <- max(est$Nhat.grp, nall)
            b.Nhat.ind[i] <- b.Nhat.grp[i] * b.Es[i]
        }
    }
    if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
        boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, 
            phat = b.phat, Es = b.Es)
   valid<-(b.Nhat.grp != Inf & boot.dbn[[1]] > 0)
        boot.mean <- list(Nhat.grp = mean(b.Nhat.grp[valid]), Nhat.ind = mean(b.Nhat.ind[valid]), 
            phat = apply(as.matrix(b.phat[valid,]),2,mean), Es = mean(b.Es[valid]))
   se <- list(Nhat.grp=sqrt(var(b.Nhat.grp[valid])), Nhat.ind=sqrt(var(b.Nhat.ind[valid])), 
            phat=sqrt(apply(as.matrix(b.phat[valid,]),2,var)), 
            Es=sqrt(var(b.Es[valid])))
   cv<-list(Nhat.grp=se$Nhat.grp/boot.mean$Nhat.grp , Nhat.ind=se$Nhat.ind/boot.mean$Nhat.ind, 
            phat=se$phat/boot.mean$phat, Es=se$Es/boot.mean$Es)
   ninvalid <- length(valid) - sum(valid)
   if (ninvalid > 0) 
     warning(paste(as.character(ninvalid), " inadmissable estimates omitted from bootstrap results."))
   civec <- rep(NA, length(vlevels))
#  careful with coding the next line depends on which estimates are vectors and which scalars
#  (in this function only phat is a vector):
   ci <- list(Nhat.grp=civec, Nhat.ind=civec, phat=matrix(rep(civec,n.occ), nrow=n.occ, ncol=length(vlevels), 
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
    intest <- list(levels=vlevels, ci=ci, boot.mean = boot.mean, boot.dbn=boot.dbn, init.N=init.N, se=se, cv=cv, 
                   ci.type=ci.type, parents=parents, created=date(), seed=seed)
 class(intest) <- "int.est.crMt"
 if(plot) plot(intest,type="hist")
    return(intest)
}

is.int.est.crMt<-function(est)
{
 inherits(est, "int.est.crMt")
}


summary.int.est.crMt<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "phat"), digits=5)
{
 if(!is.int.est.crMt(iest)) 
   stop("Argument <iest>. must be of class int.est.crMt\n")
 addtext1<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 addtext2<-paste("Initial N for numerical search    : ",iest$init.N,"\n",sep="")
 addtext<-paste(addtext1, addtext2, sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.crMt<-function(iest, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}





int.est.crMb<-function (samp, init.N= -1, ci.type="boot.nonpar", nboot=999, vlevels=c(0.025, 0.975), plot=FALSE, seed=NULL) 
{
 if(ci.type!="boot.nonpar" & ci.type!="boot.par") stop("Only bootstrap CI estimation implemented so far.\n")
    if (!is.sample.cr(samp)) 
        stop("\n*** <samp> is not an object of type 'sample.crMb'.\n")
    if (!(ci.type %in% c("boot.nonpar", "boot.par"))) 
        stop(paste("\n*** Unrecognised <ci.type>. Valid options: ", 
            "'boot.par' and 'boot.nonpar'\n"))
    if (!is.numeric(vlevels)) 
        stop("\n*** All <vlevels> values must be numeric.\n")
    if (any(vlevels < 0) | any(vlevels > 1)) 
        stop("\n*** All <vlevels> values must be between 0 and 1.\n")
    if (!is.numeric(nboot)) 
        stop("\n*** <nboot> must be numeric.\n")
    if (nboot < 1) 
        stop("\n*** <nboot> must be at least 1.\n")
    if ((plot != TRUE) & (plot != FALSE)) 
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
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
    chall <- samp$capture
    seen <- apply(chall, 1, sum) > 0
    ch <- chall[seen, ]
    groupsize <- samp$population$groupsize[seen]
    n.occ <- ncol(ch)
    nall <- nrow(ch)
    res <- point.est.crMb(samp, init.N)
    N.hat <- round(res$Nhat.grp)
    p.hat <- res$phat
    maxest <- 100 * N.hat
    if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
        b.Nhat.grp <- rep(0, nboot)
        b.Nhat.ind <- rep(0, nboot)
#        b.phat <- matrix(0, (nboot * n.occ), nrow = nboot, ncol = n.occ, 
#            dimnames = list(replicate = 1:nboot, occasion = 1:n.occ))
   b.phat <- matrix(rep(0, nboot), nrow=nboot, ncol=2, dimnames=list(replicate=1:nboot, c("unmarked","marked")))
        b.Es <- rep(0, nboot)
        b.samp <- samp
        b.reg <- generate.region()
        b.dens <- generate.density(b.reg)
        b.poppars <- setpars.population(b.dens, number.groups = N.hat)
    }
    if (ci.type == "boot.nonpar") {
        b.pop <- generate.population(b.poppars)
        b.des <- generate.design.cr(b.reg, n.occ = n.occ)
        b.survpars <- setpars.survey.cr(b.pop, b.des, pmin.unmarked = 0.5)
        b.samp <- generate.sample.cr(b.survpars)
        n0 <- N.hat - nall
        cap <- matrix(0, N.hat, n.occ)
        cap[1:nall, ] <- ch
        grpsize <- rep(0, N.hat)
        grpsize[1:nall] <- groupsize
        for (i in 1:nboot) {
            repeat {
                index <- sample(1:N.hat, N.hat, replace = TRUE)
                b.samp$capture <- cap[index, ]
                b.samp$population$groupsize <- grpsize[index]
                est <- point.est.crMb(b.samp, init.N)
                b.Nhat.grp[i] <- est$Nhat.grp
                b.Nhat.ind[i] <- est$Nhat.ind
                b.phat[i, ] <- est$phat
                b.Es[i] <- est$Es
                if (est$Nhat.grp < maxest) 
                  break
            }
            b.Nhat.grp[i] <- max(est$Nhat.grp, nall)
            b.Nhat.ind[i] <- b.Nhat.grp[i] * b.Es[i]
        }
    }
    if (ci.type == "boot.par") {
        b.pop <- generate.population(b.poppars)
        b.des <- generate.design.cr(b.reg, n.occ = n.occ, effort = log(1 - 
            p.hat)/log(1 - p.hat[1]))
        b.survpars <- setpars.survey.cr(b.pop, b.des, pmin.unmarked = p.hat[1])
        for (i in 1:nboot) {
            repeat {
                b.samp <- generate.sample.cr(b.survpars)
###				incorrect function name spotted by Mike Merridith WSC                
                est <- point.est.crMt(b.samp, init.N)
                b.Nhat.grp[i] <- est$Nhat.grp
                b.Nhat.ind[i] <- est$Nhat.ind
                b.phat[i, ] <- est$phat
                b.Es[i] <- -1
                if (est$Nhat.grp < maxest) 
                  break
            }
            b.Nhat.grp[i] <- max(est$Nhat.grp, nall)
            b.Nhat.ind[i] <- b.Nhat.grp[i] * b.Es[i]
        }
    }
    if (ci.type == "boot.nonpar" | ci.type == "boot.par") {
        boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, phat = b.phat, Es = b.Es)
   valid<-(b.Nhat.grp != Inf & boot.dbn[[1]] > 0)
        boot.mean <- list(Nhat.grp = mean(b.Nhat.grp[valid]), Nhat.ind = mean(b.Nhat.ind[valid]), 
            phat = apply(as.matrix(b.phat[valid,]),2,mean), Es = mean(b.Es[valid]))
   se <- list(Nhat.grp=sqrt(var(b.Nhat.grp[valid])), Nhat.ind=sqrt(var(b.Nhat.ind[valid])), 
            phat=sqrt(apply(as.matrix(b.phat[valid,]),2,var)), 
            Es=sqrt(var(b.Es[valid])))
   cv<-list(Nhat.grp=se$Nhat.grp/boot.mean$Nhat.grp , Nhat.ind=se$Nhat.ind/boot.mean$Nhat.ind, 
            phat=se$phat/boot.mean$phat, Es=se$Es/boot.mean$Es)
   ninvalid <- length(valid) - sum(valid)
   if (ninvalid > 0) 
     warning(paste(as.character(ninvalid), " inadmissable estimates omitted from bootstrap results."))
   civec <- rep(NA, length(vlevels))
#  careful with coding the next line depends on which estimates are vectors and which scalars
#  (in this function only phat is a vector):
   ci <- list(Nhat.grp=civec, Nhat.ind=civec, phat=matrix(rep(civec,2), nrow=2, ncol=length(vlevels), 
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
    intest <- list(levels=vlevels, ci=ci, boot.mean=boot.mean, boot.dbn=boot.dbn, init.N=init.N, se=se, cv=cv, 
                   ci.type=ci.type, parents=parents, created=date(), seed=seed)
    class(intest) <- "int.est.crMb"
    if(plot) plot(intest, type="hist")
    return(intest)
}

is.int.est.crMb<-function(est)
{
 inherits(est, "int.est.crMb")
}


summary.int.est.crMb<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "phat"), digits=5)
{
 if(!is.int.est.crMb(iest)) 
   stop("Argument <iest>. must be of class int.est.crMb\n")
 addtext1<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 addtext2<-paste("Initial N for numerical search    : ",iest$init.N,"\n",sep="")
 addtext<-paste(addtext1, addtext2, sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.crMb<-function(iest, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}


int.est.crMh<-function (samp, num.mix=2, init.N= -1, ci.type="boot.nonpar", 
    nboot=999, vlevels=c(0.025, 0.975), plot=FALSE, seed=NULL) 
{
 if(ci.type!="boot.nonpar" & ci.type!="boot.par") stop("Only bootstrap CI estimation implemented so far.\n")
    if (!is.sample.cr(samp)) 
        stop("\n*** <samp> is not an object of type 'sample.cr'.\n")
    if (num.mix < 2) 
        stop("\n*** <num.mix> must be at least 2.\n")
    if (!is.numeric(init.N)) 
        stop("\n*** <init.N> must be numeric.\n")
    if (!(ci.type %in% c("boot.nonpar"))) 
        stop(paste("\n*** Unrecognised <ci.type>. These are valid:", 
            "'boot.nonpar'\n"))
    if (!is.numeric(vlevels)) 
        stop("\n*** All <vlevels> values must be numeric.\n")
    if (any(vlevels < 0) | any(vlevels > 1)) 
        stop("\n*** All <vlevels> values must be between 0 and 1.\n")
    if (!is.numeric(nboot)) 
        stop("\n*** <nboot> must be numeric.\n")
    if (nboot < 2) 
        stop("\n*** <nboot> must be at least 2.\n")
    if ((plot != TRUE) & (plot != FALSE)) 
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
 parents<-list(wisp.id(samp,newname=as.character(substitute(samp))))
    chall <- samp$capture
    seen <- apply(chall, 1, sum) > 0
    ch <- chall[seen, ]
    groupsize <- samp$population$groupsize[seen]
    n.occ <- ncol(ch)
    nall <- nrow(ch)
    res <- point.est.crMh(samp, num.mix, init.N)
    N.hat <- round(res$Nhat.grp)
    p.hat <- res$phat
    maxest <- 100 * N.hat
    if (ci.type == "boot.nonpar") {
        b.Nhat.grp <- rep(0, nboot)
        b.Nhat.ind <- rep(0, nboot)
#        b.phat <- matrix(0, nrow = nboot, ncol = num.mix, dimnames = list(replicate = 1:nboot, 
#            group = 1:num.mix))
   b.phat <- matrix(0, nrow=nboot, ncol=num.mix, dimnames=list(replicate=1:nboot, paste("mixing p ",1:num.mix,sep="")))
        b.Es <- rep(0, nboot)
        b.samp <- samp
        n0 <- N.hat - nall
        cap <- matrix(0, N.hat, n.occ)
        cap[1:nall, ] <- ch
        grpsize <- rep(0, N.hat)
        grpsize[1:nall] <- groupsize
        for (i in 1:nboot) {
            repeat {
                index <- sample(1:N.hat, N.hat, replace = TRUE)
                b.samp$capture <- cap[index, ]
                b.samp$population$groupsize <- grpsize[index]
                est <- point.est.crMh(b.samp, num.mix, init.N)
                b.Nhat.grp[i] <- est$Nhat.grp
                b.Nhat.ind[i] <- est$Nhat.ind
                b.phat[i, ] <- est$phat
                b.Es[i] <- est$Es
                if (est$Nhat.grp < maxest) 
                  break
                b.Nhat.grp[i] <- est$Nhat.grp
                b.Nhat.ind[i] <- est$Nhat.ind
                b.phat[i, ] <- est$phat
                b.Es[i] <- est$Es
                if (est$Nhat.grp < maxest) 
                  break
            }
            b.Nhat.grp[i] <- max(est$Nhat.grp, nall)
            b.Nhat.ind[i] <- b.Nhat.grp[i] * b.Es[i]
        }
    }
    if (ci.type == "boot.nonpar") {
        boot.dbn <- list(Nhat.grp = b.Nhat.grp, Nhat.ind = b.Nhat.ind, phat = b.phat, Es = b.Es)
   valid<-(b.Nhat.grp != Inf & boot.dbn[[1]] > 0)
        boot.mean <- list(Nhat.grp = mean(b.Nhat.grp[valid]), Nhat.ind = mean(b.Nhat.ind[valid]), 
            phat = apply(as.matrix(b.phat[valid,]),2,mean), Es = mean(b.Es[valid]))
   se <- list(Nhat.grp=sqrt(var(b.Nhat.grp[valid])), Nhat.ind=sqrt(var(b.Nhat.ind[valid])), 
            phat=sqrt(apply(as.matrix(b.phat[valid,]),2,var)), 
            Es=sqrt(var(b.Es[valid])))
   cv<-list(Nhat.grp=se$Nhat.grp/boot.mean$Nhat.grp , Nhat.ind=se$Nhat.ind/boot.mean$Nhat.ind, 
            phat=se$phat/boot.mean$phat, Es=se$Es/boot.mean$Es)
   ninvalid <- length(valid) - sum(valid)
   if (ninvalid > 0) 
     warning(paste(as.character(ninvalid), " inadmissable estimates omitted from bootstrap results."))
   civec <- rep(NA, length(vlevels))
#  careful with coding the next line depends on which estimates are vectors and which scalars
#  (in this function only phat is a vector):
   ci <- list(Nhat.grp=civec, Nhat.ind=civec, phat=matrix(rep(civec,num.mix), nrow=num.mix, ncol=length(vlevels), 
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
    intest <- list(levels=vlevels, ci=ci, boot.mean=boot.mean, boot.dbn=boot.dbn, init.N=init.N, se=se, cv=cv, 
                   ci.type=ci.type, parents=parents, created=date(), seed=seed)
    class(intest) <- "int.est.crMh"
    if(plot) plot(intest, type="hist")
    return(intest)
}

is.int.est.crMh<-function (est) 
{
    inherits(est, "int.est.crMh")
}


summary.int.est.crMh<-function(iest, est=c("Nhat.grp","Nhat.ind","Es", "phat"), digits=5)
{
 if(!is.int.est.crMh(iest)) 
   stop("Argument <iest>. must be of class int.est.crMh\n")
 addtext1<-paste("Interval estimation method        : ",iest$ci.type,"\n",sep="")
 addtext2<-paste("Initial N for numerical search    : ",iest$init.N,"\n",sep="")
 addtext<-paste(addtext1, addtext2, sep="")
 summary.int.est(iest, est=est, add.text=addtext, digits=digits)
}


plot.int.est.crMh<-function(iest, est="Nhat.grp", breaks="Sturges", type="both", ...)
{
 plot.int.est(iest, est=est, breaks=breaks, type=type, ...)
}

point.sim.crM0<- function (pop.spec, survey.spec, design.spec, B = 99, init.N = -1, seed=NULL, Chapmod = FALSE, numerical=TRUE, plot=FALSE) 
{
   if (!is.pars.survey.cr(survey.spec)) {
       stop("\nsurvey.spec must be of class 'pars.survey.cr'.\n")
   }
   if (!is.design.cr(design.spec)) {
       stop("\ndesign.spec must be of class 'design.cr'.\n")
   }
   if (!is.population(pop.spec) & !is.pars.population(pop.spec)) {
       stop("pop.spec must be of class 'population' or 'pars.population'.\n")
   }
   parents<-list(wisp.id(pop.spec,newname=as.character(substitute(pop.spec))), 
   wisp.id(survey.spec,newname=as.character(substitute(survey.spec))), 
   wisp.id(design.spec,newname=as.character(substitute(design.spec))))
   if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) stop("\nThe parameter <seed> must be a number    or a WiSP object.\n")
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
#  design.spec <- survey.spec$design
#  len <- 7
#  res <- matrix(0, nrow = B, ncol = len)
#  res <- as.data.frame(res)
#  out.est <- NULL
for (i in 1:B) {
#  if (is.population(pop.spec)) mypop <- pop.spec
#  if (is.pars.population(pop.spec)) mypop <- generate.population(pop.spec)
#  if (is.sample.cr(survey.spec)) mysamp <- survey.spec
#  if (is.pars.survey.cr(survey.spec)) {
#     survey.spec$population <- mypop
#     survey.spec$design <- design.spec
#     mysamp <- generate.sample.cr(survey.spec)
#  }
#  out.est <- point.est.crM0(mysamp, init.N = init.N)
#  for (j in 1:7) {
#     res[i, j] <- out.est[[j]]
#        }
   if (is.population(pop.spec)) mypop <- pop.spec
   if (is.pars.population(pop.spec)) {
      mypop <- generate.population(pop.spec)
      random.pop<-TRUE
   }
   if (is.design.cr(design.spec)) mydes <- design.spec
#  if (is.pars.design.rm(design.spec)) {
#     mydes <- generate.design.rm(design.spec)
#     random.design<-TRUE
#  }
   survey.spec$population <- mypop
   survey.spec$design <- mydes
   mysamp <- generate.sample.cr(survey.spec,seed=seed)
   out.est <- point.est.crM0(mysamp , init.N = init.N, Chapmod = Chapmod, numerical=numerical)
   res[i, stats] <- out.est[stats]
   }
#  colnames(res) <- c("Nhat.grp", "Nhat.ind", "phat", "Es", 
#        "log.Likelihood", "res.Deviance", "AIC")
#  true.N.grp <- length(mypop$groupsize)
#  sim.plot(res$Nhat.grp, true.N.grp, xlim=xlim, ...)
#  rlist<-list(est=res, parents=parents, created=date(), seed=seed)
#  class(rlist) <- "point.sim.crM0"
   sim<-list(est=res, true=true, numerical=numerical, init.N=init.N, Chapmod=Chapmod,
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
   class(sim) <- "point.sim.crM0"
   if(plot) plot(sim, est="Nhat.grp")
   return(sim)
}

is.point.sim.crM0<-function(sim)
{
   inherits(sim, "point.sim.crM0")
}

#plot.point.sim.crM0<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), breaks="Sturges", type="both", ...)
plot.point.sim.crM0<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
   if(length(est)>1) breaks="Sturges"
   for(i in 1:length(est)) {
      if(i>1) windows(height=5, width=4)
      sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
   }
}


summary.point.sim.crM0<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), digits=5)
{
   if(!is.point.sim.crM0(sim)) 
   stop("Argument <sim>. must be of class point.sim.crM0\n")
   addtext1<-paste("Numerical estimator?            = ",sim$numerical,"\n", sep="")
   addtext2<-paste("Use Chapman's Estimator?        = ",sim$Chapmod,"\n", sep="")
   addtext3<-paste("Starting N for numerical search = ",sim$init.N,"\n", sep="")
   addtext<-paste(addtext1, addtext2, addtext3, sep="")
   summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}




point.sim.crMb<- function (pop.spec, survey.spec, design.spec, B = 9, init.N = -1, seed=NULL, plot=FALSE) 
{
   if (!is.pars.survey.cr(survey.spec)) {
      stop("\nsurvey.spec must be of class 'pars.survey.cr'.\n")
   }
   if (!is.design.cr(design.spec)) {
      stop("\ndesign.spec must be of class 'design.cr'.\n")
   }
   if (!is.population(pop.spec) & !is.pars.population(pop.spec)) {
      stop("pop.spec must be of class 'population' or 'pars.population'.\n")
   }
   parents<-list(wisp.id(pop.spec,newname=as.character(substitute(pop.spec))), 
   wisp.id(survey.spec,newname=as.character(substitute(survey.spec))), 
   wisp.id(design.spec,newname=as.character(substitute(design.spec))))
   if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) stop("\nThe parameter <seed> must be a number  or a WiSP object.\n")
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
#  design.spec <- survey.spec$design
#  len <- 7
#  res <- matrix(0, nrow = B, ncol = len)
#  res <- as.data.frame(res)
#  out.est <- NULL
   for (i in 1:B) {
#     if (is.population(pop.spec)) mypop <- pop.spec
#     if (is.pars.population(pop.spec)) mypop <- generate.population(pop.spec)
#     if (is.sample.cr(survey.spec)) mysamp <- survey.spec
#     if (is.pars.survey.cr(survey.spec)) {
#        survey.spec$population <- mypop
#        survey.spec$design <- design.spec
#        mysamp <- generate.sample.cr(survey.spec)
#     }
#     out.est <- point.est.crMb(mysamp, init.N = init.N)
#     for (j in 1:7) {
#        res[i, j] <- out.est[[j]]
#     }
      if (is.population(pop.spec)) mypop <- pop.spec
      if (is.pars.population(pop.spec)) {
         mypop <- generate.population(pop.spec)
         random.pop<-TRUE
      }
      if (is.design.cr(design.spec)) mydes <- design.spec
#     if (is.pars.design.rm(design.spec)) {
#        mydes <- generate.design.rm(design.spec)
#     random.design<-TRUE
#     }
      survey.spec$population <- mypop
      survey.spec$design <- mydes
      mysamp <- generate.sample.cr(survey.spec,seed=seed)
      out.est <- point.est.crMb(mysamp , init.N = init.N)
      res[i, stats] <- out.est[stats]
   }  
#  colnames(res) <- c("Nhat.grp", "Nhat.ind", "phat", "Es", 
#        "log.Likelihood", "res.Deviance", "AIC")
#  true.N.grp <- length(mypop$groupsize)
#  sim.plot(res$Nhat.grp, true.N.grp, xlim=xlim, ...)
#  rlist<-list(est=res, parents=parents, created=date(), seed=seed)
#  class(rlist) <- "point.sim.crMb"
   sim<-list(est=res, true=true, init.N=init.N, 
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
   class(sim) <- "point.sim.crMb"
   if(plot) plot(sim, est="Nhat.grp")
   return(sim)
}  

is.point.sim.crMb<-function(sim)
{
   inherits(sim, "point.sim.crMb")
}

#plot.point.sim.crMb<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), breaks="Sturges", type="both", ...)
plot.point.sim.crMb<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
   if(length(est)>1) breaks="Sturges"
   for(i in 1:length(est)) {
      if(i>1) windows(height=5, width=4)
      sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
   }
}

summary.point.sim.crMb<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), digits=5)
{
   if(!is.point.sim.crMb(sim)) 
      stop("Argument <sim>. must be of class point.sim.crMb\n")
   addtext<-paste("Starting N for numerical search = ",sim$init.N,"\n", sep="")
   summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}



point.sim.crMt<- function (pop.spec, survey.spec, design.spec, B = 99, init.N = -1, seed=NULL, plot=FALSE) 
{
   if (!is.pars.survey.cr(survey.spec)) {
       stop("\nsurvey.spec must be of class 'pars.survey.cr'.\n")
   }
    if (!is.design.cr(design.spec)) {
       stop("\ndesign.spec must be of class 'design.cr'.\n")
   }
   if (!is.population(pop.spec) & !is.pars.population(pop.spec)) {
       stop("pop.spec must be of class 'population' or 'pars.population'.\n")
   }
   parents<-list(wisp.id(pop.spec,newname=as.character(substitute(pop.spec))), 
   wisp.id(survey.spec,newname=as.character(substitute(survey.spec))), 
   wisp.id(design.spec,newname=as.character(substitute(design.spec))))
   if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) stop("\nThe parameter <seed> must be a number       or a WiSP object.\n")
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
#  design.spec <- survey.spec$design
#  len <- 7
#  res <- matrix(0, nrow = B, ncol = len)
#  res <- as.data.frame(res)
#  out.est <- NULL
   for (i in 1:B) {
#     if (is.population(pop.spec)) mypop <- pop.spec
#     if (is.pars.population(pop.spec)) mypop <- generate.population(pop.spec)
#     if (is.sample.cr(survey.spec)) mysamp <- survey.spec
#     if (is.pars.survey.cr(survey.spec)) {
#        survey.spec$population <- mypop
#        survey.spec$design <- design.spec
#        mysamp <- generate.sample.cr(survey.spec)
#     }
#     out.est <- point.est.crMt(mysamp, init.N = init.N)
#     for (j in 1:7) {
#        res[i, j] <- out.est[[j]]
#     }
      if (is.population(pop.spec)) mypop <- pop.spec
      if (is.pars.population(pop.spec)) {
         mypop <- generate.population(pop.spec)
         random.pop<-TRUE
      }
      if (is.design.cr(design.spec)) mydes <- design.spec
#     if (is.pars.design.rm(design.spec)) {
#        mydes <- generate.design.rm(design.spec)
#        random.design<-TRUE
#     }
      survey.spec$population <- mypop
      survey.spec$design <- mydes
      mysamp <- generate.sample.cr(survey.spec,seed=seed)
      out.est <- point.est.crMt(mysamp , init.N = init.N)
      res[i, stats] <- out.est[stats]
   }  
#  colnames(res) <- c("Nhat.grp", "Nhat.ind", "phat", "Es", 
#        "log.Likelihood", "res.Deviance", "AIC")
#  true.N.grp <- length(mypop$groupsize)
#  sim.plot(res$Nhat.grp, true.N.grp, xlim=xlim, ...)
#  rlist<-list(est=res, parents=parents, created=date(), seed=seed)
#  class(rlist) <- "point.sim.crMt"
   sim<-list(est=res, true=true, init.N=init.N,
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
   class(sim) <- "point.sim.crMt"
   if(plot) plot(sim, est="Nhat.grp")
   return(sim)
}


is.point.sim.crMt<-function(sim)
{
   inherits(sim, "point.sim.crMt")
}



#plot.point.sim.crMt<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), breaks="Sturges", type="both", ...)
plot.point.sim.crMt<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
   if(length(est)>1) breaks="Sturges"
   for(i in 1:length(est)) {
      if(i>1) windows(height=5, width=4)
      sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
   }
}

summary.point.sim.crMt<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), digits=5)
{
   if(!is.point.sim.crMt(sim)) 
   stop("Argument <sim>. must be of class point.sim.crMt\n")
   addtext<-paste("Starting N for numerical search = ",sim$init.N,"\n", sep="")
   summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
} 


point.sim.crMh<- function (pop.spec, survey.spec, design.spec, B = 99, init.N = -1, seed=NULL, num.mix = 2, plot=FALSE) 
{
   if (!is.pars.survey.cr(survey.spec)) {
       stop("\nsurvey.spec must be of class 'pars.survey.cr'.\n")
   }
   if (!is.design.cr(design.spec)) {
       stop("\ndesign.spec must be of class 'design.cr'.\n")
   }
   if (!is.population(pop.spec) & !is.pars.population(pop.spec)) {
       stop("pop.spec must be of class 'population' or 'pars.population'.\n")
   }
   parents<-list(wisp.id(pop.spec,newname=as.character(substitute(pop.spec))), 
   wisp.id(survey.spec,newname=as.character(substitute(survey.spec))), 
   wisp.id(design.spec,newname=as.character(substitute(design.spec))))
   if (!is.null(seed) & !is.numeric(seed) & !is.wisp.class(seed)) stop("\nThe parameter <seed> must be a number    or a WiSP object.\n")
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
#  design.spec <- survey.spec$design
#  len <- 7
#  res <- matrix(0, nrow = B, ncol = len)
#  res <- as.data.frame(res)
#  out.est <- NULL
   for (i in 1:B) {
#     if (is.population(pop.spec)) mypop <- pop.spec
#     if (is.pars.population(pop.spec)) mypop <- generate.population(pop.spec)
#     if (is.sample.cr(survey.spec)) mysamp <- survey.spec
#     if (is.pars.survey.cr(survey.spec)) {
#        survey.spec$population <- mypop
#        survey.spec$design <- design.spec
#        mysamp <- generate.sample.cr(survey.spec)
#     }
#     out.est <- point.est.crMt(mysamp, init.N = init.N)
#     for (j in 1:7) {
#        res[i, j] <- out.est[[j]]
#     }
      if (is.population(pop.spec)) mypop <- pop.spec
      if (is.pars.population(pop.spec)) {
         mypop <- generate.population(pop.spec)
         random.pop<-TRUE
      }
      if (is.design.cr(design.spec)) mydes <- design.spec
#     if (is.pars.design.rm(design.spec)) {
#        mydes <- generate.design.rm(design.spec)
#        random.design<-TRUE
#     }
      survey.spec$population <- mypop
      survey.spec$design <- mydes
      mysamp <- generate.sample.cr(survey.spec,seed=seed)
      out.est <- point.est.crMh(mysamp , init.N = init.N, num.mix=num.mix)
      res[i, stats] <- out.est[stats]
   }  
#  colnames(res) <- c("Nhat.grp", "Nhat.ind", "phat", "Es", 
#        "log.Likelihood", "res.Deviance", "AIC")
#  true.N.grp <- length(mypop$groupsize)
#  sim.plot(res$Nhat.grp, true.N.grp, xlim=xlim, ...)
#  rlist<-list(est=res, parents=parents, created=date(), seed=seed)
#  class(rlist) <- "point.sim.crMh"
   sim<-list(est=res, true=true, init.N=init.N, num.mix=num.mix,
           random.pop=random.pop, random.design=random.design, parents=parents, created=date(), seed=seed)
   class(sim) <- "point.sim.crMh"
   if(plot) plot(sim, est="Nhat.grp")
   return(sim)
}

is.point.sim.crMh<-function(sim)
{
   inherits(sim, "point.sim.crMh")
}


#plot.point.sim.crMh<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), breaks="Sturges", type="both", ...)
plot.point.sim.crMh<-function(sim, est=c("Nhat.grp"), breaks="Sturges", type="both", ...)
{
   if(length(est)>1) breaks="Sturges"
   for(i in 1:length(est)) {
      if(i>1) windows(height=5, width=4)
      sim.plot(sim, est=est[i], breaks=breaks, type=type, ...)
   }
}

summary.point.sim.crMh<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), digits=5)
{
   if(!is.point.sim.crMh(sim)) 
      stop("Argument <sim>. must be of class point.sim.crMh\n")
   addtext1<-paste("Number of mixtures              = ",sim$num.mix,"\n", sep="")
   addtext2<-paste("Starting N for numerical search = ",sim$init.N,"\n", sep="")
   addtext<-paste(addtext1, addtext2,  sep="")
   summary.point.sim(sim, est=est, add.text=addtext, digits=digits)
}



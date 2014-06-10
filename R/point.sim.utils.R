get.true.state<-function(pars)
#-------------------------------------------------------------------------------
# Returns true values of Nhat.grp, Nhat.ind, Es from population or population 
# parameter object. (Called Nhat.* because then can get true and estimated
# values out of their respective lists using the same character variable.)
# In due course should probably expand this to include various other parameters.
#-------------------------------------------------------------------------------
{
 if (!is.pars.population(pars) & !is.population(pars)) stop("\nThe parameter <pars> must be of class 'pars.population' of 'population'.\n")
 if (is.pars.population(pars)) {
   N.grp <- pars$number.groups
   if(pars$size.method == "poisson") {
     lambda<-pars$size.lambda
     size<-seq(pars$size.min,pars$size.max,length=(pars$size.max-pars$size.min+1))
     Es<-sum(dpois(size,lambda)*size)/sum(dpois(size,lambda))
   }else {
     Es<-sum(pars$size.prob*pars$size.values)
   }
   N.ind<-round(N.grp*Es)
 }
 if (is.population(pars)) {
   N.grp<-length(pars$groupsize)
   N.ind<-sum(pars$groupsize)
   Es<-N.ind/N.grp
 }
 list(Nhat.grp=N.grp, Nhat.ind=N.ind, Es=Es)
}




sim.plot<-function(sim, est="Nhat.grp", type="both", breaks="Sturges", ...)
#----------------------------------------------------------
# Inputs: 
#       x: estimates of true.x from simulation
#  true.x: true value of x in simulation
#     ...: other graphical parameters to pass to hist()
#
# Outputs: histogram and boxplot of x, showing true.x
#----------------------------------------------------------
{
 if(type!="box" & type!="hist" & type!="both") stop("Parameter type must be 'box' or 'hist' or 'both'\n.")
 if(length(est)>1) stop("Only one estimate can be plotted at a time.")
# stat is also in summary.point.sim() any changes here should occur there too
 stat<-c("Group Abundance",
         "Animal Abundance",
         "Mean Group size",
         "Effective half-width",
         "Effective area",
         "Encounter rate",
         "Encounter rate",
         "Capture probabilities",
         "Log-likelihood"
 )
 names(stat)<-c("Nhat.grp","Nhat.ind","Es", "mu", "esa", "nL", "nbar", "phat", "log.likelihood")
 if(!is.element(est,names(sim$est))) stop(paste("Simulation does not contain estimates named",est))
 else {
#  extract estimates and truth:
   if(is.element(est,names(sim$true))) true.x<-sim$true[[est]]
   else true.x<-NA
   x<-sim$est[[est]]
 }
 name.x<-stat[est]
   if(is.numeric(breaks)) {
     if(length(breaks)==1) breaks<-seq(min(x),max(x),length=(breaks+1))
     if(breaks[1]>min(x)) x<-x[breaks[1]<=x]
     if(breaks[length(breaks)]<max(x)) x<-x[x<=breaks[length(breaks)]]
#    use Sturges' rule for number intervals if have only max & min
     if(length(breaks)==2) breaks<-seq(breaks[1],breaks[2],length=(n.sturges(x)+1))
   }
# set scaling factor for labels, axes and text to be 90% (plot window height)/5
 pars.old<-par(no.readonly=TRUE)
 cex<-0.9*par()$din[2]/5
 par(cex=cex, cex.lab=cex, cex.axis=cex, cex.main=cex, xpd=TRUE)
 if(type=="both") par(mfrow=c(2,1))
 mean.x<-mean(x)
 hst<-hist(x, breaks=breaks, plot=FALSE, ...)
 hist.ymax <- max(hst$counts)
 hist.xmax<-max(hst$breaks)
 hist.xmin<-min(hst$breaks)
 breaks<-hst$breaks
 box.stats<-boxplot(x,horizontal=TRUE,plot=FALSE)$stats
 box.xmax<-max(box.stats)
 box.xmin<-min(box.stats)
 # histogram
 if(type=="hist" || type=="both") {
   hist(x, breaks=breaks, main="", xlab="Estimate value (red line is mean)", ...)
   if(!is.na(true.x)) {
     title(main=paste("Simulated sampling distribution of",name.x,"estimates\n(", length(x), 
           "replicates; blue=truth)"))
     lines(c(true.x, true.x), c(0,hist.ymax), lwd=2, col="blue")
   } else title(main=paste("Simulated sampling distribution of",name.x,"estimates\n(", length(x), "replicates)"))
   lines(c(mean.x, mean.x), c(0,hist.ymax), lwd=2, col="red", lty="dashed")
 }
 if(type=="box" || type=="both") {
   plot(c(min(x),max(x)),c(0.5,1.5),type="n", xlab="", ylab="", yaxt="n")
   boxplot(x,horizontal=TRUE,add=TRUE)
   if(!is.na(true.x)) points(true.x,1,col="blue",pch=19)
   if(type=="box") 
     title(main=paste("Simulated sampling distribution of",name.x,"estimates\n(", length(x), "replicates)"), 
          xlab="Estimate value")
   else 
     title(xlab="Estimate value")
 }
 par(pars.old)
}


summary.point.sim<-function(sim, est=c("Nhat.grp","Nhat.ind","Es"), add.text=NULL, digits=5) 
{
 cat("\n")
 cat("POINT SIMULATION SUMMARY\n")
 cat("------------------------\n")
 cat("creation date   :", sim$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(sim$parents)) {
   cat("      ",paste("(",sim$parents[[i]]$class,", ",sim$parents[[i]]$name,", ",sim$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(sim$seed)) cat("random number seed used: ",sim$seed,"\n")
 cat("\n")
 cat("Design randomization?    :",sim$random.design,"\n")
 cat("Population randomization?:",sim$random.pop,"\n")
 cat("Number of simulations    :",length(sim$est$Nhat.grp),"\n")
 if(!is.null(add.text)) cat(add.text,"\n\n")
     cat("Variable name               Summary statistics\n")
     cat("----------------------    --------------------------------------------------------\n")
# stat is also in sim.plot() any changes here should occur there too
 stat<-c("Group Abundance      :",
         "Animal Abundance     :",
         "Mean Group size      :",
         "Effective half-width :",
         "Effective area       :",
         "Encounter rate       :",
         "Encounter rate       :",
         "Capture probabilities:",
         "Log-likelihood       :"
 )
 names(stat)<-c("Nhat.grp","Nhat.ind","Es", "mu", "esa", "nL", "nbar", "phat", "log.likelihood")
 for(i in 1:length(est)) {
   if(!is.element(est[i],names(sim$est))) warning(paste("Simulation does not contain estimates named",est[i]))
   else {
     cat(stat[est[i]])
     n<-length(sim$est[[est[i]]])
     mn<-mean(sim$est[[est[i]]])
     se<-sqrt(var(sim$est[[est[i]]])/n)
     if(is.element(est[i],names(sim$true))) cat("    True value  =",signif(sim$true[[est[i]]],digits),"\n")
     else cat("    True value  = (Not available)\n")
     cat("                          Mean        =",signif(mn,digits),"\n")
     cat("                          s.e. of mean=",signif(se,digits),
         paste("  (%CV of mean =",signif(100*se/mn,digits),")",sep=""),"\n\n")
   }
 }
}





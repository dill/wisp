summary.int.est<-function(iest, est=c("Nhat.grp","Nhat.ind","Es"), add.text=NULL, digits=5) 
{
 cls<-switch(class(iest),
        int.est.pl = "PLOT SAMPLING METHOD",
        int.est.lt = "LINE TRANSECT METHOD",
        int.est.pt = "POINT TRANSECT METHOD",
        int.est.no = "NEAREST OBJECT METHOD",
        int.est.rm = "SIMPLE REMOVAL METHOD",
        int.est.ce = "CATCH-EFFORT METHOD",
        int.est.cir = "CHANGE-IN-RATIO METHOD",
        int.est.crM0 = "MARK RECAPTURE MODEL M0",
        int.est.crMt = "MARK RECAPTURE MODEL Mt",
        int.est.crMb = "MARK RECAPTURE MODEL Mb",
        int.est.crMh = "MARK RECAPTURE MODEL Mh",
        int.est.dp = "DOUBLE PLATFORM METHOD"
 )
 cat("\n")
 cat(paste("INTERVAL ESTIMATION SUMMARY FOR ",cls,"\n"),sep="")
 cat("--------------------------------------------------------\n")
 cat("creation date   :", iest$created,"\n")
 cat("parent object(s) (class, name, creation date):\n")
 for(i in 1:length(iest$parents)) {
   cat("      ",paste("(",iest$parents[[i]]$class,", ",iest$parents[[i]]$name,", ",iest$parents[[i]]$created,")",sep=""),"\n")
 }
 if(is.numeric(iest$seed)) cat("random number seed used: ",iest$seed,"\n")
 cat("\n")
 if(!is.null(iest$boot.dbn)) {
   cat("Number of bootstraps              :",length(iest$boot.dbn[[1]]),"\n")
   valid.Nhat <- iest$boot.dbn[["Nhat.grp"]][iest$boot.dbn[["Nhat.grp"]] != Inf]
   nvalid <- length(valid.Nhat)
   cat("Number of admissable bootstraps   :",nvalid,"\n")
 }
 if(!is.null(add.text)) cat(add.text,"\n")
 cat("\n")
 cat("Confidence interval percentiles      :",iest$levels,"\n") 
 cat("\n")
     cat("Variable name               Summary statistics\n")
     cat("----------------------    -----------------------------------------------\n")
# stat is also in plot.int.est() any changes here should occur there too
 stat<-c("Group Abundance      :",
         "Animal Abundance     :",
         "Mean Group size      :",
         "Effective half-width :",
         "Effective area       :",
         "Encounter rate       :",
         "Encounter rate       :",
         "Capture probability  :",
         "Obs. model par(s)    :",
         "g(0):"
 )
 names(stat)<-c("Nhat.grp","Nhat.ind","Es", "mu", "esa", "nL", "nbar", "phat", "theta", "average.g0")
 for(i in 1:length(est)) {
   if(!is.element(est[i],names(iest$ci))) 
     warning(paste("The interval estimation object does not contain estimates named",est[i]))
   else {
#     if(est[i]=="theta" | est[i]=="phat") {
     n.elements<-length(iest$se[[est[i]]])
     if((est[i]=="theta" | est[i]=="phat" | est[i]=="average.g0") & n.elements>1) {
       for(j in 1:n.elements) {
         cat(stat[est[i]],"\n")
         cat(names(iest$se[[est[i]]])[j])
         se<-iest$se[[est[i]]][j]
         cv<-iest$cv[[est[i]]][j]
         spaces<-"                           "
         padding<-paste(rep(" ",(22-nchar(names(iest$se[[est[i]]])[j]))),collapse="")
         cat(padding)
         if(!is.null(iest$boot.dbn)) {
           cat("          Bootstrap mean =",signif(mean(iest$boot.mean[[est[i]]][j]),digits),"\n")
           cat(spaces)
         }
         cat("               s.e. =",signif(se,digits),
             paste("  (%CV = ",signif(100*cv,digits),")",sep=""),"\n")
         cat(spaces)
         cat(paste("Confidence interval = (",signif(iest$ci[[est[i]]][j,1],digits),";",
             signif(iest$ci[[est[i]]][j,2],digits),")\n\n",sep=""))
       }
     } else {
       cat(stat[est[i]])
       se<-iest$se[[est[i]]]
       cv<-iest$cv[[est[i]]]
       spaces<-"                           "
       if(!is.null(iest$boot.dbn)) {
         cat("          Bootstrap mean =",signif(mean(iest$boot.mean[[est[i]]]),digits),"\n")
         cat(spaces)
       }
       cat("               s.e. =",signif(se,digits),
           paste("  (%CV = ",signif(100*cv,digits),")",sep=""),"\n")
       cat(spaces)
       cat(paste("Confidence interval = (",signif(iest$ci[[est[i]]][1],digits),";", 
           signif(iest$ci[[est[i]]][2],digits),")\n\n",sep=""))
     }
   }
 }
}



plot.int.est<-function(iest, est="Nhat.grp", type="both", breaks="Sturges", ...)
#--------------------------------------------------------------------------------
# Plots histogram of element [[est]] of class int.est.* object.
# <type> can be "hist", "bar" or "both"
# If <breaks> is numeric of length 1, it is treated as number of cutpoints to use.
# If <breaks> has length 2, they are treated as max and min of histogram & 
#             Sturges rule is used to calculate histogram cutpoints.
# If <breaks> has length >2, they are treated as cutpoints.
#--------------------------------------------------------------------------------
{
 if(is.null(iest$boot.dbn)) {
   plot.text("Plots are only implemented for bootstrap CI estimation methods",col="black")
   stop("Plots are only implemented for bootstrap CI estimation methods")
 }
 if(is.null(iest$boot.dbn) & iest$ci.type!="profile") {
   plot.text("Plots are only produced for bootstrap or profile likelihood CI estimation methods",col="black")
   stop("Plots are only produced for bootstrap or profile likelihood CI estimation methods")
 }
 if(iest$ci.type=="profile" & est!="Nhat.grp") {
   plot.text("Profile likelihood plot only implemented for group Abundance estimate",col="black")
   stop("Profile likelihood plot only implemented for group Abundance estimate")
 }
 if(iest$ci.type=="boot.par" | iest$ci.type=="boot.nonpar") {
   if(type!="box" & type!="hist" & type!="both") stop("Parameter type must be 'box' or 'hist' or 'both'\n.")
   if(length(est)>1) stop("Only one estimate can be plotted at a time.")
#   stat is also in summary.int.est() any changes here should occur there too
    stat<-c("Group Abundance",
           "Animal Abundance",
           "Mean Group size",
           "Effective half-width",
           "Effective area",
           "Encounter rate",
           "Encounter rate",
           "Capture probabilities:",
           "Obs. model parameters:",
           "g(0):"
   )
#   names(stat)<-c("Nhat.grp","Nhat.ind","Es", "mu", "esa", "nL", "nbar", "phat", "theta")
#   theta and phat vectors not yet implemented (need for loop and multiple plots)
   names(stat)<-c("Nhat.grp","Nhat.ind","Es", "mu", "esa", "nL", "nbar") 
   if(!is.element(est,names(stat)))
     stop(paste("Plotting not implemented for estimate named",est))
   if(!is.element(est,names(iest$ci))) 
     stop(paste("The interval estimation object does not contain estimates named",est))
   else {
     x<-iest$boot.dbn[[est]]
   }
   name.x<-stat[est]
   if(is.numeric(breaks)) {
     if(length(breaks)==1) breaks<-seq(min(x),max(x),length=(breaks+1))
     if(breaks[1]>min(x)) x<-x[breaks[1]<=x]
     if(breaks[length(breaks)]<max(x)) x<-x[x<=breaks[length(breaks)]]
#    use Sturges' rule for number intervals if have only max & min
     if(length(breaks)==2) breaks<-seq(breaks[1],breaks[2],length=(n.sturges(x)+1))
   }
#   set scaling factor for labels, axes and text to be 90% (plot window height)/5
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
     hst<-hist(x, breaks=breaks, main="", xlab="Estimate value", ...)
     if (iest$ci.type == "boot.par") 
       title(main=paste("Parametric b'strap CI for ",name.x," estimates\n(",length(x)," replicates)", sep=""), cex=cex)
     if (iest$ci.type == "boot.nonpar") 
       title(main=paste("Nonparametric b'strap CI for ",name.x," estimates\n(",length(x)," replicates)", sep=""), cex=cex)
     ymax<-max(hst$counts)
     k<-length(iest$levels)
     lower<-iest$ci[[est]][1]
     upper<-iest$ci[[est]][k]
     est<-iest$boot.mean[[est]]
     lines(c(lower,lower),c(-ymax*0.05,ymax*0.1),col="blue",lty=2, lwd=2)
     lines(c(upper,upper),c(-ymax*0.05,ymax*0.1),col="blue",lty=2, lwd=2)
     lines(c(est,est),c(-ymax*0.05,ymax),lty=3, lwd=2)
     CI<-round(c(lower,upper))
     vlevels<-c(iest$levels[1], iest$levels[k])
     text(CI, -ymax*0.05, paste(CI), adj=c(0.5,1), col="blue", xpd=T, cex=cex)
     text(CI, ymax*0.1, paste(round(100*vlevels,1), "%"), adj=c(0.5,0), col="blue", xpd=T, cex=cex)
     text(est, -ymax*0.05, paste(round(est)), adj=c(0.5,1), xpd=T, cex=cex)
     text(est, ymax*0.1, paste("bootstrap mean"), adj=c(0.5,0), xpd=T, cex=cex)
#     lines(c(mean.x, mean.x), c(0,hist.ymax), lwd=2, col="red", lty="dashed")
   }
   if(type=="box" || type=="both") {
     plot(c(min(x),max(x)),c(0.5,1.5),type="n", xlab="", ylab="", yaxt="n")
     boxplot(x,horizontal=TRUE,add=TRUE)
     if(type=="box") 
       title(main=paste("Simulated sampling distribution of",name.x,"estimates\n(", length(x), "replicates)"), 
          xlab="Estimate value")
     else 
       title(xlab="Estimate value")
   }
 }
 if(iest$ci.type=="profile") {
   N<-iest$proflik$N
   Nleft<-iest$proflik$Nleft
   Nright<-iest$proflik$Nright
   W<-iest$proflik$W
   q<-iest$proflik$q
   plot(N, -W, type = "l", xlab = "N", ylab = "W(N)/2")
   lines(c(Nleft, Nright), c(crit, crit), lty = 4)
   yshift <- (max(W[W < Inf]) - min(W[W < Inf])) * 0.05
   xshift <- (Nright - Nleft) * 0.025
   text((ci$Nhat.grp[1] - xshift), (crit + yshift), labels = as.character(ci$Nhat.grp[1]))
   text((ci$Nhat.grp[length(vlevels)] + xshift), (crit + yshift), labels = as.character(ci$Nhat.grp[length(vlevels)]))
 }
 par(pars.old)
}






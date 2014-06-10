point.sim.pt<-function (pop.spec,survey.spec,design.spec,
B = 99,plot=F,conditional=T,model="half.normal",...) 
{

#***********************************  INFO ************************************************************
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
#*******************************************************************************************************

#  Error trap to ensure there is a random element involved in the replicate simulations
if (!is.pars.survey.pt(survey.spec)) {
            stop("\nsurvey.spec must be of class 'pars.survey.pt'.\n")}        
if (!is.design.pt(design.spec) & !is.pars.design.pt(design.spec)) {
            stop("\ndesign.spec must be of class 'design.pt' or 'pars.design.pt'.\n")}        
if (!is.population(pop.spec) & !is.pars.population(pop.spec)) {
            stop("\npop.spec must be of class 'population' or 'pars.population'.\n")}        


#  Set us the results matrix to store the output from the point estimate function for each replicate.

res<-matrix(0,nrow=B,ncol=8)
res<-as.data.frame(res)
out.est <- NULL
#  Produces point estimates for each replicate of the randomised population / survey sample. 
for(i in 1:B){
  if(is.population(pop.spec)){
    mypop <- pop.spec
    }
  if(is.pars.population(pop.spec)){
    mypop <- generate.population(pop.spec)
    }
  if(is.design.pt(design.spec)){
    mydes <- design.spec
    }
  if(is.pars.design.pt(design.spec)){
    mydes <- generate.design.pt(design.spec)
    }
  if(is.sample.pt(survey.spec)){
    mysamp <- survey.spec
    }
  if(is.pars.survey.pt(survey.spec)){
  survey.spec$population <- mypop
  survey.spec$design <- mydes
  mysamp <- generate.sample.pt(survey.spec)
  }
# Records output for the replicate
  out.est <- point.est.pt(mysamp,plot=plot,conditional=conditional,model=model)
  par.len <- length(out.est[[3]])
#  Selects required output and places it into the results matrix
  for(j in c(1,2,4,5,6,7,8)){
    res[i,j]<-out.est[[j]]
    }
   res[i,3]<-out.est[[3]][[1]]  
}

#  Command to label the columns of the results matrix
colnames(res)<-c("Nhat.grp","Nhat.ind","theta.sigma2","esa","nbar","Es","log.Likelihood","AIC")

#  Records the true population abundance and the mean of the replicate abundance estimates
true.N.grp <- length(mypop$groupsize)
mean.N.grp <- mean(res$Nhat.grp)

#  Commands to produce histogram of the replicate abundance estimates
#low.lim <- min(true.N.grp,min(hist(res$Nhat.grp,plot=F,...)$breaks))
#upp.lim <- max(true.N.grp,max(hist(res$Nhat.grp,plot=F,...)$breaks))
max.true <- max(hist(res$Nhat.grp,plot=F,...)$counts)
hist(res$Nhat.grp,main = paste("Histogram of Nhat.grp with",B,"replicates"),...)
#  Add lines to histogram denoting mean abundance from the replicates (dashed red line) and
#  true population group abundance (solid blue line)
lines(c(mean.N.grp,mean.N.grp),c(0,max.true),lwd=2,col="red",lty="dashed")
lines(c(true.N.grp,true.N.grp),c(0,max.true),lwd=2,col="blue")

#  Return results matrix
class(res)<-"sim.matrix"
return(res)

}

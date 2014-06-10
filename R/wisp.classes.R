# functons to return the name, class(es) and creation date (if it exists) of WiSP objects

wisp.id<-function(x, newname=NULL)
#---------------------------------------------------------------
# Returns list with elements $name, $class and $created, giving 
# the name, class and creation date (if it exists) of the WiSP 
# object x.
# Argument "newname" lets you overwrite the parent object's name
# - put this in to deal with calling function from inside another
# function because I can't figure out how to access the object 
# passed to the original function in a way that lets it be passed
# appropriately to wisp.id(). I deal with it by using:
#
# wisp.id(x,newname=as.character(substitute(x)))
#
#---------------------------------------------------------------
{
 if (is.null(newname)) newname<-as.character(deparse(substitute(x)))
 created<-"NA"
 if(is.element("created",names(x))) created<-x$created 
 list(class=class(x),name=newname,created=created)
}

# functions to return all objects of given class and all objects with WiSP class:

is.class<-function(obj.name,target.class)
#------------------------------------------------------------------------------
# Returns TRUE if the class of an object from a character variable (a scalar) 
# containing its name, is of class <target.class>.
#------------------------------------------------------------------------------
{
  is.element(target.class,class(eval(parse(text=obj.name))))
}

ls.class<-function(name.vector,target.class)
#------------------------------------------------------------------------------
# Lists all objects with names given in <name.vector> that are of class 
# <target.class> (a scalar or vector).
#------------------------------------------------------------------------------
{
 name.vector[apply(matrix(name.vector,nrow=length(name.vector)),1,FUN=is.class,target.class)]
}


# The commented-out function below are the non-apply version of ls.class 
# (It seems very marginally slower than the apply version)
#ls.class<-function(name.vector,target.class)
#------------------------------------------------------------------------------
# Lists all objects with names given in <name.vector> that are of class 
# <target.class> (a scalar or vector).
#------------------------------------------------------------------------------
#{
# n<-length(name.vector); keep<-rep(0,n)
# for (i in 1:n) keep[i]<-is.element(target.class,class(eval(parse(text=name.vector[i]))))
# name.vector[keep==1]
#}


wisp.object.list<-function(pos=1,outfile="")
#---------------------------------------------------------------
# Outputs list of all objects with WiSP classes in the environment 
# in position <pos> of the search lis, to <outfile>.
#
# Note: Should probably allow environment name instead of pos?
#---------------------------------------------------------------
{
 missing<-"NA"
 wisp.classes<-c(
"region","density.population","pars.population","population",
"pars.design.pl","pars.design.lt","pars.design.pt","pars.design.dp",
"design.pl","design.lt","design.pt","design.cr","design.rm","design.no",
"pars.survey.lt","pars.survey.pt","pars.survey.rm","pars.survey.ce","pars.survey.cir","pars.survey.cr","pars.survey.dp",
"sample.pl","sample.lt","sample.pt","sample.rm","sample.ce","sample.cir","sample.cr","sample.dp",
"point.est.pl","point.est.lt","point.est.pt","point.est.rm","point.est.ce","point.est.cir","point.est.crM0","point.est.crMt","point.est.crMb","point.est.crMh","point.est.no",
"int.est.pl","int.est.lt","int.est.pt","int.est.rm","int.est.ce","int.est.cir","int.est.crM0","int.est.crMt","int.est.crMb","int.est.crMh","int.est.no",
"point.sim.pl","point.sim.lt","point.sim.pt","point.sim.rm","point.sim.ce","point.sim.cir","point.sim.crM0","point.sim.crMt","point.sim.crMb","point.sim.crMh","point.sim.no")
 for(i in 1:length(wisp.classes)) {
   objects<-ls(pos=pos)
   objects<-objects[substr(objects,1,1)!="*"] # I once found object "*tmp*" which screws things up
   clist<-ls.class(objects,wisp.classes[i])
   if(length(clist)==0) clist<-missing
   else{
     for(j in 1:length(clist)) {
       cat(file=outfile,paste("child",wisp.classes[i],clist[j],cdate(clist[j]),sep="\t"),"\n", append=TRUE)
       parents<-list(list(class=missing, name=missing, created=missing))
       child<-eval(parse(text=clist[j]))
       if(is.element("parents",names(child))) parents<-child$parents
       for(k in 1:length(parents)) cat(file=outfile, 
         paste("parent",parents[[k]]$class,parents[[k]]$name,parents[[k]]$created,sep="\t"),"\n", append=TRUE)
     }
   }
 }
}

# test code:
#wisp.object.list() # prints to screen
#wisp.object.list(outfile="C:/temp/wisp.objects.txt") # prints to file C:/temp/wisp.objects.txt
                       # (import above file into Excel or similar to see it neatly formatted)

is.wisp.class<-function(obj)
{
 inherits(obj,c("region","density.population","pars.population","population",
"pars.design.pl","pars.design.lt","pars.design.pt","pars.design.dp",
"design.pl","design.lt","design.pt","design.cr","design.rm","design.no",
"pars.survey.lt","pars.survey.pt","pars.survey.rm","pars.survey.ce","pars.survey.cir","pars.survey.cr","pars.survey.dp",
"sample.pl","sample.lt","sample.pt","sample.rm","sample.ce","sample.cir","sample.cr","sample.dp",
"point.est.pl","point.est.lt","point.est.pt","point.est.rm","point.est.ce","point.est.cir","point.est.crM0","point.est.crMt","point.est.crMb","point.est.crMh","point.est.no",
"int.est.pl","int.est.lt","int.est.pt","int.est.rm","int.est.ce","int.est.cir","int.est.crM0","int.est.crMt","int.est.crMb","int.est.crMh","int.est.no",
"point.sim.pl","point.sim.lt","point.sim.pt","point.sim.rm","point.sim.ce","point.sim.cir","point.sim.crM0","point.sim.crMt","point.sim.crMb","point.sim.crMh","point.sim.no"))
}


cdate<-function(obj.name)
#------------------------------------------------------------------------------
# Returns creation date of an object from a character variable (a scalar) 
# containing its name if the object is a list with component $created, 
# else returns "NA".
#------------------------------------------------------------------------------
{
 created<-"NA"
 obj<-eval(parse(text=obj.name))
 if(!is.list(obj)) stop("Object ",obj.name," is not a list -- function cdate requires a list.")
 else if(is.element("created",names(obj))) created<-obj$created
 created
}



#
#    Script of functions that generate a gui for wisp
#       Rexstad:  17-19 February 2007
#				versioning modification June 2007
#				Fshake and genden.data for density surface added July 2008
#

#  ------------------   Functions associated with density  ---------------------

.First.lib <- function(lib, pkg) {
packageStartupMessage("Welcome to WiSP version 1.2.6-2 January 2013\n")
if (.Platform$OS.type=="windows" & interactive() & .Platform$GUI=="Rgui") {
winMenuAdd("Interactive density")
winMenuAddItem("Interactive density","Region", "my.region <- eval(parse(text=gen.reg()))")
winMenuAddItem("Interactive density","Run F-Shake3D", "launch.FShake3D()")
winMenuAddItem("Interactive density","Run F-Shake3D","disable")
winMenuAddItem("Interactive density","Sample Density", "my.density <- densityDialog.FShake3D()")



winMenuAdd("Generate density")
winMenuAddItem("Generate density","Region", "my.region <- eval(parse(text=gen.reg()))")
winMenuAddItem("Generate density", "From x-y data", "my.density <- eval(parse(text=gen.den.gendendata()))")
winMenuAddItem("Generate density", "From x-y data", "disable")
winMenuAddItem("Generate density","Generate","my.density <- eval(parse(text=gen.den()))")
winMenuAddItem("Generate density","Generate","disable")
winMenuAddItem("Generate density","Add hotspot", "my.density <- eval(parse(text=gen.den.hotspot()))")
winMenuAddItem("Generate density","Add hotspot", "disable")
winMenuAddItem("Generate density","Set stripe", "my.density <- eval(parse(text=gen.den.addstripe()))")
winMenuAddItem("Generate density","Set stripe", "disable")
winMenuAdd("Generate density/Plot")
winMenuAddItem("Generate density/Plot","Heatmap","eval(parse(text=plot.dens.wireheat(heat=TRUE)))")
winMenuAddItem("Generate density/Plot","Heatmap","disable")
winMenuAddItem("Generate density/Plot","Wireframe","eval(parse(text=plot.dens.wireheat(heat=FALSE)))")
winMenuAddItem("Generate density/Plot","Wireframe","disable")
winMenuAddItem("Generate density/Plot","Rotating surface","eval(parse(text=visualize(double=FALSE)))")
winMenuAddItem("Generate density/Plot","Rotating surface","disable")
winMenuAddItem("Generate density","Summary","summary(eval(parse(text=of.class('density.population'))))")
winMenuAddItem("Generate density","Summary","disable")

#  ----------------   Functions to do with generating populations  -----------------------
winMenuAdd("Generate population")
winMenuAddItem("Generate population","Set parameters", "my.pop.pars <- eval(parse(text=pars.popn()))")
winMenuAddItem("Generate population","Set parameters", "disable")
winMenuAddItem("Generate population","Generate","my.population <- eval(parse(text=gen.popn()))")
winMenuAddItem("Generate population","Generate","disable")
winMenuAddItem("Generate population","Plot", "eval(parse(text=popn.plot()))")
winMenuAddItem("Generate population","Plot", "disable")
winMenuAddItem("Generate population","Summary","summary(eval(parse(text=of.class('population'))))")
winMenuAddItem("Generate population","Summary","disable")

#  ----------------   Functions to do with generating designs  -----------------------
winMenuAdd("Survey design")
winMenuAdd("Survey design/Plot sampling")
winMenuAddItem("Survey design/Plot sampling","Set parameters", "my.plot.design.pars <- eval(parse(text=design.pars.pl()))")
winMenuAddItem("Survey design/Plot sampling","Set parameters", "disable")
winMenuAddItem("Survey design/Plot sampling","Generate","my.plot.design <- eval(parse(text=design.gen.pl()))")
winMenuAddItem("Survey design/Plot sampling","Generate","disable")

winMenuAdd("Survey design/Line transect")
winMenuAddItem("Survey design/Line transect","Set parameters", "my.lt.design.pars <- eval(parse(text=design.pars.transect('line')))")
winMenuAddItem("Survey design/Line transect","Set parameters", "disable")
winMenuAddItem("Survey design/Line transect","Generate","my.lt.design <- eval(parse(text=design.gen.lt()))")
winMenuAddItem("Survey design/Line transect","Generate","disable")
winMenuAddItem("Survey design/Line transect","Plot", "plot(eval(parse(text=of.class('design.lt'))))")
winMenuAddItem("Survey design/Line transect","Plot", "disable")

winMenuAdd("Survey design/Point transect")
winMenuAddItem("Survey design/Point transect","Set parameters", "my.pt.design.pars <- eval(parse(text=design.pars.transect('point')))")
winMenuAddItem("Survey design/Point transect","Set parameters", "disable")
winMenuAddItem("Survey design/Point transect","Generate","my.pt.design <- eval(parse(text=design.gen.pt()))")
winMenuAddItem("Survey design/Point transect","Generate","disable")
winMenuAddItem("Survey design/Point transect","Plot", "plot(eval(parse(text=of.class('design.pt'))))")
winMenuAddItem("Survey design/Point transect","Plot", "disable")

winMenuAdd("Survey design/Mark-recapture")
winMenuAddItem("Survey design/Mark-recapture","Generate", "my.cr.design.pars <- eval(parse(text=design.gen.cr()))")
winMenuAddItem("Survey design/Mark-recapture","Generate", "disable")

# -----------------   Functions to do with existing datasets  ---------------------
winMenuAdd("Existing samples")
winMenuAdd("Existing samples/Line transect")
winMenuAddItem("Existing samples/Line transect", "samp1.lt", "data(samp1.lt);wakeup.lt()")
winMenuAddItem("Existing samples/Line transect", "samp2.lt", "data(samp2.lt);wakeup.lt()")
winMenuAddItem("Existing samples/Line transect", "tortoise.samp.lt", "data(tortoise.samp.lt);wakeup.lt()")
winMenuAdd("Existing samples/Removal method")
winMenuAddItem("Existing samples/Removal method", "seal.samp.rm", "data(seal.samp.rm)")
winMenuAddItem("Existing samples/Removal method", "skink.samp.rm", "data(skink.samp.rm)")
winMenuAddItem("Existing samples/Removal method", "skink.samp.rm2", "data(skink.samp.rm2)")
winMenuAdd("Existing samples/Mark-recapture")
winMenuAddItem("Existing samples/Mark-recapture", "birds1997.samp.cr", "data(birds1997.samp.cr);wakeup.cr()")
winMenuAddItem("Existing samples/Mark-recapture", "chips.samp.cr", "data(chips.samp.cr);wakeup.cr()")
winMenuAddItem("Existing samples/Mark-recapture", "hare.samp.cr", "data(hare.samp.cr);wakeup.cr()")
winMenuAddItem("Existing samples/Mark-recapture", "skink.samp.cr", "data(skink.samp.cr);wakeup.cr()")
winMenuAddItem("Existing samples/Mark-recapture", "standrews.samp.cr", "data(standrews.samp.cr);wakeup.cr()")
winMenuAddItem("Existing samples/Mark-recapture", "taxi.samp.cr", "data(taxi.samp.cr);wakeup.cr()")
winMenuAddItem("Existing samples/Mark-recapture", "samp1.cr", "data(samp1.cr);wakeup.cr()")
winMenuAddItem("Existing samples/Mark-recapture", "samp2.cr", "data(samp2.cr);wakeup.cr()")
winMenuAdd("Existing samples/Double platform")
winMenuAddItem("Existing samples/Double platform", "harb.samp.dp", "data(harb.samp.dp)")

#  ----------------   Functions to do with generating surveys  -----------------------
winMenuAdd("Sampling")
winMenuAdd("Sampling/Plot sampling")
winMenuAddItem("Sampling/Plot sampling","Generate", "my.plot.sample <- eval(parse(text=sample.gen.pl()))")
winMenuAddItem("Sampling/Plot sampling","Generate", "disable")
winMenuAddItem("Sampling/Plot sampling","Summary", "summary(eval(parse(text=of.class('sample.pl'))))")
winMenuAddItem("Sampling/Plot sampling","Summary", "disable")
winMenuAddItem("Sampling/Plot sampling","Plot", "eval(parse(text=sample.plot('plot')))")
winMenuAddItem("Sampling/Plot sampling","Plot", "disable")

winMenuAdd("Sampling/Line transect")
winMenuAddItem("Sampling/Line transect","Set parameters", "my.sample.lt.pars <- eval(parse(text=survey.pars.transect('line')))")
winMenuAddItem("Sampling/Line transect","Set parameters", "disable")
winMenuAddItem("Sampling/Line transect","Generate","my.lt.sample <- eval(parse(text=sample.gen.transect('line')))")
winMenuAddItem("Sampling/Line transect","Generate","disable")
winMenuAddItem("Sampling/Line transect","Summary","summary(eval(parse(text=of.class('sample.lt'))))")
winMenuAddItem("Sampling/Line transect","Summary","disable")
winMenuAddItem("Sampling/Line transect","Plot","eval(parse(text=sample.plot('line')))")
winMenuAddItem("Sampling/Line transect","Plot","disable")

winMenuAdd("Sampling/Point transect")
winMenuAddItem("Sampling/Point transect","Set parameters", "my.sample.pt.pars <- eval(parse(text=survey.pars.transect('point')))")
winMenuAddItem("Sampling/Point transect","Set parameters", "disable")
winMenuAddItem("Sampling/Point transect","Generate","my.pt.sample <- eval(parse(text=sample.gen.transect('point')))")
winMenuAddItem("Sampling/Point transect","Generate","disable")
winMenuAddItem("Sampling/Point transect","Summary","summary(eval(parse(text=of.class('sample.pt'))))")
winMenuAddItem("Sampling/Point transect","Summary","disable")
winMenuAddItem("Sampling/Point transect","Plot","eval(parse(text=sample.plot('point')))")
winMenuAddItem("Sampling/Point transect","Plot","disable")

winMenuAdd("Sampling/Mark-recapture")
winMenuAddItem("Sampling/Mark-recapture","Set parameters", "my.sample.cr.pars <- eval(parse(text=survey.pars.cr()))")
winMenuAddItem("Sampling/Mark-recapture","Set parameters", "disable")
winMenuAddItem("Sampling/Mark-recapture","Generate", "my.cr.sample <- eval(parse(text=sample.gen.cr()))")
winMenuAddItem("Sampling/Mark-recapture","Generate", "disable")
winMenuAddItem("Sampling/Mark-recapture","Summary","summary(eval(parse(text=of.class('sample.cr'))))")
winMenuAddItem("Sampling/Mark-recapture","Summary","disable")
winMenuAddItem("Sampling/Mark-recapture","Plot","eval(parse(text=sample.plot('cr')))")
winMenuAddItem("Sampling/Mark-recapture","Plot","disable")

#  ----------------   Functions to do with point estimates  -----------------------
winMenuAdd("Estimation")
winMenuAdd("Estimation/Plot sampling")
winMenuAddItem("Estimation/Plot sampling","Point estimate", "summary(my.point.est.plot <- eval(parse(text=est.point.plot())))")
winMenuAddItem("Estimation/Plot sampling","Point estimate", "disable")
winMenuAddItem("Estimation/Plot sampling","Interval estimate", "my.interval.est.plotsam <- eval(parse(text=est.int.plot()))")
winMenuAddItem("Estimation/Plot sampling","Interval estimate", "disable")

winMenuAdd("Estimation/Line transect")
winMenuAddItem("Estimation/Line transect","Point estimate","my.point.est.line <- eval(parse(text=est.point.transect('line')));plot(my.point.est.line)")
winMenuAddItem("Estimation/Line transect","Point estimate","disable")
winMenuAddItem("Estimation/Line transect","Interval estimate","my.interval.est.lt <- eval(parse(text=est.int.transect('line')))")
winMenuAddItem("Estimation/Line transect","Interval estimate","disable")

winMenuAdd("Estimation/Point transect")
winMenuAddItem("Estimation/Point transect","Point estimate", "my.point.est.point <- eval(parse(text=est.point.transect('point')));plot(my.point.est.point)")
winMenuAddItem("Estimation/Point transect","Point estimate", "disable")
winMenuAddItem("Estimation/Point transect","Interval estimate", "my.interval.est.pt <- eval(parse(text=est.int.transect('point')))")
winMenuAddItem("Estimation/Point transect","Interval estimate", "disable")

winMenuAdd("Estimation/Mark-recapture")
winMenuAdd("Estimation/Mark-recapture/Point estimate")
winMenuAdd("Estimation/Mark-recapture/Interval estimate")
winMenuAddItem("Estimation/Mark-recapture/Point estimate","M0", "summary(my.point.est.crM0 <- eval(parse(text=est.point.cr('M0'))))")
winMenuAddItem("Estimation/Mark-recapture/Point estimate","M0", "disable")
winMenuAddItem("Estimation/Mark-recapture/Interval estimate","M0", "my.interval.est.crM0 <- eval(parse(text=est.int.cr('M0')))")
winMenuAddItem("Estimation/Mark-recapture/Interval estimate","M0", "disable")
winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mt", "summary(my.point.est.crMt <- eval(parse(text=est.point.cr('Mt'))))")
winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mt", "disable")
winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mt", "my.interval.est.crMt <- eval(parse(text=est.int.cr('Mt')))")
winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mt", "disable")
winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mb", "summary(my.point.est.crMb <- eval(parse(text=est.point.cr('Mb'))))")
winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mb", "disable")
winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mb", "my.interval.est.crMb <- eval(parse(text=est.int.cr('Mb')))")
winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mb", "disable")
winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mh", "summary(my.point.est.crMh <- eval(parse(text=est.point.cr('Mh'))))")
winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mh", "disable")
winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mh", "my.interval.est.crMh <- eval(parse(text=est.int.cr('Mh')))")
winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mh", "disable")

#  ----------------   Functions to do with simulations  -----------------------
winMenuAdd("Simulation")
winMenuAddItem("Simulation", "Plot sampling", "my.sim.plotsamp <- eval(parse(text=sim.plotsam()))")
winMenuAddItem("Simulation", "Plot sampling", "disable")
winMenuAddItem("Simulation", "Line transect", "my.sim.ltsamp <- eval(parse(text=sim.transect('line')))")
winMenuAddItem("Simulation", "Line transect", "disable")
winMenuAddItem("Simulation", "Point transect", "my.sim.ptsamp <- eval(parse(text=sim.transect('point')))")
winMenuAddItem("Simulation", "Point transect", "disable")

winMenuAdd("Simulation/Mark-recapture")
winMenuAddItem("Simulation/Mark-recapture", "M0", "my.sim.cr.M0 <- eval(parse(text=sim.cr('M0')))")
winMenuAddItem("Simulation/Mark-recapture", "M0", "disable")
winMenuAddItem("Simulation/Mark-recapture", "Mt", "my.sim.cr.Mt <- eval(parse(text=sim.cr('Mt')))")
winMenuAddItem("Simulation/Mark-recapture", "Mt", "disable")
winMenuAddItem("Simulation/Mark-recapture", "Mb", "my.sim.cr.Mb <- eval(parse(text=sim.cr('Mb')))")
winMenuAddItem("Simulation/Mark-recapture", "Mb", "disable")
winMenuAddItem("Simulation/Mark-recapture", "Mh", "my.sim.cr.Mh <- eval(parse(text=sim.cr('Mh')))")
winMenuAddItem("Simulation/Mark-recapture", "Mh", "disable")

#  ----------------   Functions to do with advanced features (rgl, mrds, MARK)  -----------------------
winMenuAdd("Advanced")
winMenuAddItem("Advanced", "Visualizations", "eval(parse(text=visualize(double=TRUE)))")
winMenuAddItem("Advanced", "Visualizations", "disable")
winMenuAddItem("Advanced", "Covariate distance sampling", "none")
winMenuAddItem("Advanced", "Covariate distance sampling", "disable")
winMenuAddItem("Advanced", "Mark-recapture with MARK", "none")
winMenuAddItem("Advanced", "Mark-recapture with MARK", "disable")

#  ----------------   Functions to do with housekeeping  -----------------------
winMenuAdd("Clean-up")
winMenuAddItem("Clean-up", "Remove created objects", "rmls.my()")
winMenuAddItem("Clean-up","Erase menus","menu.erase()")
} # of OS test
}  #  of .First.lib function

rmls.my <- function() {
    mylist <- select.list(ls(envir = parent.frame(), pattern="^my."), multiple = TRUE, title="Objects you have created")
    if (length(mylist) != 0) {
        OK <- winDialog("yesno", "Do you want to remove these objects?")
        if (OK == "YES") 
            rm(list = mylist, envir = parent.frame())
    }
}
menu.erase <- function() {
#     awkward because menus need to be removed from 'bottom up'
#     I accomplished this by sorting menus so that longest names (containing slashes) 
#     would rise to the top of a dataframe (zapper), hence the counting of characters with 'nchar'
#     and the call to 'order' to sort by longest number of characters
  rid.these <- winMenuNames()
  tex <- data.frame(menus=rid.these, len=rep(0))
  for (i in 1:length(rid.these)) { tex$len[i] <- nchar(as.character(tex$menus[i])) }
  zapper <- tex[order(-tex$len),]
  for(i in 1:length(rid.these)) {
    winMenuDel(as.character(zapper[i,1]))  
    Sys.sleep(0.05)  # pausing between destructions seems necessary (?)
     }
  }

of.class <- function(the.class, class2=NULL) {
  objects.this.class <- sapply(ls(name=1), function(x) is(eval(parse(text=x)), the.class))
  all.objects <- ls(name=1)
  object.list <- all.objects[objects.this.class]
  if (!is.null(class2)) {
    second.objects <-  sapply(ls(name=1), function(x) is(eval(parse(text=x)), class2))
    object.list.2 <- all.objects[second.objects]
    object.list <- c(object.list, object.list.2)
    }
  select.title <- paste("Choose a", the.class, sep=" ")
  if (length(object.list) == 0) stop("There are no objects of type '", the.class, "'. Perhaps you have not yet created them.")
  choice <- select.list(object.list, title=select.title)
  return(choice)
  }

gen.reg <- function() {
  query <- winDialogString("Study area:\n   Length & width study area", "100,100")
  x <- as.numeric(strsplit(query, ",")[[1]][1])
  y <- as.numeric(strsplit(query, ",")[[1]][2])
  com <- paste("generate.region(", x, ",", y, ")", sep="")
  winMenuAddItem("Generate density","Generate","enable")
  winMenuAddItem("Generate density","From x-y data", "enable")
  winMenuAddItem("Interactive density","Run F-Shake3D","enable")
  print(com)
  return(com)
  }

  gen.den <- function() {
  query <- winDialogString("Study area:\n   Rel. density SW, SE, NW of area", "1,1,1")
  sw <- as.numeric(strsplit(query, ",")[[1]][1])
  se <- as.numeric(strsplit(query, ",")[[1]][2])
  nw <- as.numeric(strsplit(query, ",")[[1]][3])
  reg <- of.class("region")
  com <- paste("generate.density(", reg,", southwest=", sw, ", southeast=", se, ", northwest=", nw, ")", sep="")
  winMenuAddItem("Generate density","Add hotspot","enable")
  winMenuAddItem("Generate density","Set stripe","enable")
  winMenuAddItem("Generate density/Plot","Wireframe","enable")
  winMenuAddItem("Generate density/Plot","Heatmap","enable")
  winMenuAddItem("Generate density/Plot","Rotating surface","enable")
  winMenuAddItem("Generate density","Summary","enable")
  winMenuAddItem("Generate population","Set parameters","enable")
  print(com)
  return(com)
  }

  gen.den.hotspot <- function() {
  den <- of.class("density.population")
  query <- winDialogString("Add hotspot:\n   Location of hotspot centre", "50,50")
  x <- as.numeric(strsplit(query, ",")[[1]][1])
  y <- as.numeric(strsplit(query, ",")[[1]][2])
  query <- winDialogString("Add hotspot:\n   Height and width of hotspot", "10,30")
  altitude <- as.numeric(strsplit(query, ",")[[1]][1])
  sigma <- as.numeric(strsplit(query, ",")[[1]][2])
  com <- paste("add.hotspot(", den, ", x=", x, ", y=", y, ", altitude=", 
                altitude, ", sigma=", sigma, ")", sep="")
  print(com)
  return(com)
  }
  
  gen.den.addstripe <- function() {
  den <- of.class("density.population")
  query <- winDialogString("Set stripe:\n   Starting coordinate of stripe", "10,10")
  x1 <- as.numeric(strsplit(query, ",")[[1]][1])
  y1 <- as.numeric(strsplit(query, ",")[[1]][2])
  query <- winDialogString("Set stripe:\n   Ending coordinate of stripe", "90,90")
  x2 <- as.numeric(strsplit(query, ",")[[1]][1])
  y2 <- as.numeric(strsplit(query, ",")[[1]][2])
  query <- winDialogString("Set stripe:\n   Density inside stripe and stripe width", "0, 5")
  value <- as.numeric(strsplit(query, ",")[[1]][1])
  width <- as.numeric(strsplit(query, ",")[[1]][2])
  com <- paste("set.stripe(", den, ", x1=", x1, ", y1=", y1, 
               ", x2=", x2, ", y2=", y2, ", value=", 
                value, ", width=", width, ")", sep="")
  print(com)
  return(com)
  }
  
  gen.den.gendendata <- function() {
  reg <- of.class("region")
  animal.data <- of.class("data.frame")
  query <- winDialogString("Generate density from data:\n   Number of incements in x and y direction of region", "100,100")
  xint <- as.integer(strsplit(query, ",")[[1]][1])
  yint <- as.integer(strsplit(query, ",")[[1]][2])
  query <- winDialogString("Generate density from data:\n  Smoothing parameter (<1 smooth, >1 non-smooth) ", "1")
  gam.gamma <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("genden.data(reg=" , reg, ", xylocations=", animal.data,
	             ", nint.x=", xint, ",nint.y=", yint,",gam.param=", gam.gamma, ")", sep="")
  winMenuAddItem("Generate density","Add hotspot","enable")
  winMenuAddItem("Generate density","Set stripe","enable")
  winMenuAddItem("Generate density/Plot","Rotating surface","enable")
  winMenuAddItem("Generate density/Plot","Wireframe","enable")
  winMenuAddItem("Generate density/Plot","Heatmap","enable")
  winMenuAddItem("Generate density","Summary","enable")
  winMenuAddItem("Generate population","Set parameters","enable")
  print(com)
  return(com)
  }

	plot.dens.wireheat <- function(heat=FALSE) {
		den <- of.class("density.population")
		more <- NULL
		ifelse(heat, more<-",method='image'",more<-"")
		com <- paste("plot.density.population(", den, more, ")",sep="")
		print(com)
		return(com)
	}
	
	pars.popn <- function() {
  den <- of.class("density.population")
    query <- winDialogString("Population:\n  Number groups ", "100")
    num.groups <- as.numeric(strsplit(query, ",")[[1]][1])
    query <- winDialogString("Population:\n  Min, Max, Mean of group size", "1,10,5")
    size.min <- as.numeric(strsplit(query, ",")[[1]][1])
    size.max <- as.numeric(strsplit(query, ",")[[1]][2])
    size.mean <- as.numeric(strsplit(query, ",")[[1]][3])
    size.method <- "poisson"
    exposure.method <- "beta"
    exposure.min <- 0
    exposure.max <- 1
    exposure.mean <- 0.5
    exposure.shape <- 0.1
    com <- paste("setpars.population(", den, ", number.groups=", num.groups, 
                 ", size.method='", size.method, "', size.min=", size.min,
                 ", size.max=", size.max, ", size.mean=", size.mean, 
                 ", exposure.method='", exposure.method, "', exposure.min=", exposure.min, ",exposure.max=", exposure.max,
                 ", exposure.mean=", exposure.mean, ", exposure.shape=", exposure.shape, ", adjust.interactive='TRUE')", sep="")
  winMenuAddItem("Generate population","Generate","enable")
  print(com)
  return(com)
  }
  
  gen.popn <- function() {
  popn <- of.class("pars.population")
  query <- winDialogString("Population:\n  random number seed ", "123123")
  the.seed <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("generate.population(", popn, ", seed=", the.seed, ")",sep=" ")
  winMenuAddItem("Generate population","Plot","enable")
  winMenuAddItem("Generate population","Summary","enable")
  winMenuAddItem("Survey design/Plot sampling","Set parameters","enable")
  winMenuAddItem("Survey design/Line transect","Set parameters","enable")
  winMenuAddItem("Survey design/Point transect","Set parameters","enable")
  winMenuAddItem("Survey design/Mark-recapture","Generate", "enable")
  print(com)
  return(com)
  }
  
  popn.plot <- function() {
  popn <- of.class("population")
  query <- winDialogString("Population:\n  Plot type (details or location)", "details")
  plottype <- strsplit(query, ",")[[1]][1]
  query <- winDialogString("Population:\n  Show group sizes (yes/no)", "yes")
  size <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Population:\n  Show exposure shades (yes/no) ", "yes")
  exposure <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Population:\n  Dot enlargement factor", "0.75")
  enlarge <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Population:\n  Title on plot", "my.population")
  plottitle <- strsplit(query, ",")[[1]][1]
  com <- paste("plot.population(", popn, ", type='", plottype, 
               "', show.sizes=", size, ", show.exp=", exposure,
               ", dsf=", enlarge, ", title='", plottitle, "')",sep="")
  print(com)
  return(com)
  }
  
  design.pars.pl <- function() {
  reg <- of.class("region")
  query <- winDialogString("Plot design:\n  Selection (random or regular)", "random")
  method <- strsplit(query, ",")[[1]][1]
  query <- winDialogString("Plot design:\n  Proportion of area surveyed", "0.05")
  covered <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("setpars.design.pl(", reg, ", method='", method, 
               "', area.covered=", covered, ")",sep="")
  winMenuAddItem("Survey design/Plot sampling","Generate","enable")
  print(com)
  return(com)
  }
  
  design.gen.pl <- function() {
  pars.pl <- of.class("pars.design.pl")
  query <- winDialogString("Design plot:\n  random number seed ", "123123")
  the.seed <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("generate.design.pl(", pars.pl, ", seed=", the.seed, ")",sep=" ")
  winMenuAddItem("Sampling/Plot sampling","Generate","enable")
  print(com)
  return(com)
  }
  
  design.gen.cr <- function() {
  reg <- of.class("region")
  query <- winDialogString("Design capture-recapture:\n  number of sampling occasions ", "5")
  n.occ <- as.integer(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Design capture-recapture:\n  relative effort allocated each occasion ", "1,1,1,1,1")
  com <- paste("generate.design.cr(", reg, ", n.occ=", n.occ, ", effort=c(", sep="")
  for (i in 1:n.occ) {
    com <- paste(com, as.integer(strsplit(query, ",")[[1]][i]), sep="")
    if (i < n.occ) { com <- paste(com, ",", sep="")
    }
    }
  com <- paste(com, "))", sep="")
  winMenuAddItem("Sampling/Mark-recapture","Set parameters", "enable")
  print(com)
  return(com)
  }
  
  design.pars.transect <- function(line.or.point) {
  reg <- of.class("region")
  query <- winDialogString("Transect design:\n  Number of north/south transects", "10")
  n.transects <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Transect design:\n  Total number of transects (multiple of n.transects)", "10")
  n.units <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Transect design:\n  Maximum distance from transect searched", "2")
  visual.range <- as.numeric(strsplit(query, ",")[[1]][1])
  if (line.or.point == 'line' ) {
    query <- winDialogString("Transect design:\n  Proportion of transect searched", "1")
    percent.on.effort <- as.numeric(strsplit(query, ",")[[1]][1])
    com <- paste("setpars.design.lt(", reg, ", n.transects=", n.transects, 
               ", n.units=", n.units, ", visual.range=", visual.range, 
               ", percent.on.effort=", percent.on.effort, ")",sep="")
    winMenuAddItem("Survey design/Line transect","Generate","enable")
      } else {
    com <- paste("setpars.design.pt(", reg, ", n.transects=", n.transects, 
               ", n.units=", n.units, ", visual.range=", visual.range, ")",sep="")
    winMenuAddItem("Survey design/Point transect","Generate","enable")
    }
  print(com)
  return(com)
  }

  design.gen.lt <- function() {
  popn <- of.class("pars.design.lt")
  query <- winDialogString("Line transect design:\n  random number seed ", "123123")
  the.seed <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("generate.design.lt(", popn, ", seed=", the.seed, ")",sep=" ")
  winMenuAddItem("Survey design/Line transect","Plot","enable")
  winMenuAddItem("Sampling/Line transect","Set parameters","enable")
  print(com)
  return(com)
  }

  design.gen.pt <- function() {
  popn <- of.class("pars.design.pt")
  query <- winDialogString("Point transect design:\n  random number seed ", "123123")
  the.seed <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("generate.design.pt(", popn, ", seed=", the.seed, ")",sep=" ")
  winMenuAddItem("Survey design/Point transect","Plot","enable")
  winMenuAddItem("Sampling/Point transect","Set parameters","enable")
  print(com)
  return(com)
  }

  sample.gen.pl <- function() {
  popn <- of.class("population")
  design <- of.class("design.pl")
  query <- winDialogString("Plot sample generation:\n  random number seed ", "123123")
  the.seed <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("generate.sample.pl(", popn, ",", design, ", seed=", the.seed, ")",sep=" ")
  winMenuAddItem("Sampling/Plot sampling","Summary","enable")
  winMenuAddItem("Sampling/Plot sampling","Plot","enable")
  winMenuAddItem("Estimation/Plot sampling","Point estimate","enable")
  winMenuAddItem("Estimation/Plot sampling","Interval estimate","enable")
  winMenuAddItem("Simulation","Plot sampling","enable")
  print(com)
  return(com)
  }
  
  survey.pars.transect <- function(line.or.point) {
  popn <- of.class("population")
  if (line.or.point=='line') {
      design <- of.class("design.lt")
      winMenuAddItem("Sampling/Line transect","Generate","enable")      
      } else {
      design <- of.class("design.pt")
      winMenuAddItem("Sampling/Point transect","Generate","enable")
    }
  query <- winDialogString("Transect survey params:\n  Distance Pr(detect)=0.5 for min. exposure ", "1")
  disthalf.min <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Transect survey params:\n  Distance Pr(detect)=0.5 for max. exposure ", "2")
  disthalf.max <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste(ifelse(line.or.point=='line',"setpars.survey.lt(", "setpars.survey.pt("), popn, ",", design, 
               ", disthalf.min=", disthalf.min, ", disthalf.max=", disthalf.max, ")",sep=" ")
  print(com)
  return(com)
  }
  
  survey.pars.cr <- function() {
  popn <- of.class("population")
  design <- of.class("design.cr")
  query <- winDialogString("Capture-recapture survey parameters:\n  pmin unmk, pmax unmk, pmin mk, pmax mk", "0.01, 0.50, 0.25, 0.75")
  pmin.unmarked <- as.numeric(strsplit(query, ",")[[1]][1])
  pmax.unmarked <- as.numeric(strsplit(query, ",")[[1]][2])
  pmin.marked <- as.numeric(strsplit(query, ",")[[1]][3])
  pmax.marked <- as.numeric(strsplit(query, ",")[[1]][4])
  query <- winDialogString("Capture-recapture survey parameters:\n  Proportion improvement in detectability between occasions ", "0")
  improvement <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("setpars.survey.cr(", popn, ",", design, 
               ", pmin.unmarked=", pmin.unmarked, ", pmax.unmarked=", pmax.unmarked,
               ", pmin.marked=", pmin.marked, ", pmax.marked=", pmax.marked,
               ", improvement=", improvement, ")",sep="")
  winMenuAddItem("Sampling/Mark-recapture","Generate", "enable")  
  print(com)
  return(com)
  }
  
  sample.gen.transect <- function(line.or.point) {
  if (line.or.point=='line') {
    survey <- of.class("pars.survey.lt")
    winMenuAddItem("Sampling/Line transect","Summary","enable")
    winMenuAddItem("Sampling/Line transect","Plot","enable")
    winMenuAddItem("Advanced", "Visualizations", "enable")
    winMenuAddItem("Estimation/Line transect","Point estimate","enable")
    winMenuAddItem("Estimation/Line transect","Interval estimate","enable")
    winMenuAddItem("Simulation","Line transect","enable")
      } else {
      survey <- of.class("pars.survey.pt")
      winMenuAddItem("Sampling/Point transect","Summary","enable")
      winMenuAddItem("Sampling/Point transect","Plot","enable")
      winMenuAddItem("Advanced", "Visualizations", "enable")
      winMenuAddItem("Estimation/Point transect","Point estimate","enable")
      winMenuAddItem("Estimation/Point transect","Interval estimate","enable")
      winMenuAddItem("Simulation","Point transect","enable")
    }
  query <- winDialogString("Transect sample generation:\n  random number seed ", "123123")
  the.seed <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste(ifelse(line.or.point=='line',"generate.sample.lt(", "generate.sample.pt("),
               survey, ", seed=", the.seed, ")",sep=" ")
  print(com)
  return(com)
  }
  
  sample.gen.cr <- function() {
  survey <- of.class("pars.survey.cr")
  query <- winDialogString("Capture-recapture sample generation:\n  random number seed ", "123123")
  the.seed <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("generate.sample.cr(", survey, ", seed=", the.seed, ")",sep=" ")
  winMenuAddItem("Sampling/Mark-recapture","Summary","enable")  
  winMenuAddItem("Sampling/Mark-recapture","Plot","enable") 
  winMenuAddItem("Advanced", "Visualizations", "enable") 
  winMenuAddItem("Estimation/Mark-recapture/Point estimate","M0", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Interval estimate","M0", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mt", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mt", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mb", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mb", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mh", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mh", "enable")
  winMenuAddItem("Simulation/Mark-recapture", "M0", "enable")  
  winMenuAddItem("Simulation/Mark-recapture", "Mt", "enable")  
  winMenuAddItem("Simulation/Mark-recapture", "Mb", "enable")  
  winMenuAddItem("Simulation/Mark-recapture", "Mh", "enable")  
  print(com)
  return(com)
  }
  
  sample.plot <- function(samp.type) {
  if (samp.type=="plot") {samp.object <- of.class("sample.pl")
    } else {
    if (samp.type=="line") { 
      samp.object <- of.class("sample.lt")
        query <- winDialogString("Transect sample generation:\n  Plot of 'locations' or 'hist' ", "hist")
        plot.type <- strsplit(query, ",")[[1]][1]
    } else {
    if (samp.type=="point") { samp.object <- of.class("sample.pt")
    } else {
      samp.object <- of.class("sample.cr")
      query <- winDialogString("Capture-recapture sample generation:\n  Plot of 'locations' or 'freq' ", "freq")
      plot.type <- strsplit(query, ",")[[1]][1]
    }
    }}
  query <- winDialogString("Plot of sample:\n  Show group sizes (yes/no)", "yes")
  size <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Plot of sample:\n  Show exposure shades (yes/no) ", "yes")
  exposure <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Plot of sample:\n  Dot enlargement factor", "0.75")
  enlarge <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Plot of sample:\n  Whole population shown", "yes")
  entire <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  com <- paste("plot(", samp.object, sep="")
  if (samp.type=="line" || samp.type=="cr") com <- paste(com, ", type='", plot.type, "'", sep="") 
  com <- paste(com, ", show.sizes=", size, ", show.exp=", exposure,
               ", dsf=", enlarge, ", whole.population=", entire, ")",sep="")
  print(com)
  return(com)
  }
  
  wakeup.lt <- function() {
  winMenuAddItem("Sampling/Line transect","Summary","enable")
  winMenuAddItem("Sampling/Line transect","Plot","enable")
  winMenuAddItem("Advanced", "Visualizations", "enable")
  winMenuAddItem("Estimation/Line transect","Point estimate","enable")
  winMenuAddItem("Estimation/Line transect","Interval estimate","enable")
  winMenuAddItem("Simulation","Line transect","enable")
  return()
  }

  wakeup.cr <- function() {
  winMenuAddItem("Sampling/Mark-recapture","Summary","enable")  
  winMenuAddItem("Sampling/Mark-recapture","Plot","enable") 
  winMenuAddItem("Estimation/Mark-recapture/Point estimate","M0", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Interval estimate","M0", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mt", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mt", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mb", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mb", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Point estimate","Mh", "enable")
  winMenuAddItem("Estimation/Mark-recapture/Interval estimate","Mh", "enable")
  winMenuAddItem("Simulation/Mark-recapture", "M0", "enable")  
  winMenuAddItem("Simulation/Mark-recapture", "Mt", "enable")  
  winMenuAddItem("Simulation/Mark-recapture", "Mb", "enable")  
  winMenuAddItem("Simulation/Mark-recapture", "Mh", "enable")  
  return()
  }
  
  est.point.plot <- function() {
  samp <- of.class("sample.pl")
  query <- winDialogString("Point estimation plot sampling:\n  Use MLE estimate (yes/no) ", "yes")
  HT <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", FALSE, TRUE)
  com <- paste("summary(point.est.pl(", samp, 
               ", HT=", HT, "))",sep="")
  print(com)
  return(com)
  }
  
  est.int.plot <- function() {
  samp <- of.class("sample.pl")
  query <- winDialogString("Interval estimation plot sampling:\n  Use MLE estimate (yes/no) ", "yes")
  HT <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Interval estimation plot sampling:\n  Method (normal, boot.par, boot.nonpar", "boot.par")
  ci.type <- strsplit(query, ",")[[1]][1]
  if (ci.type != "normal") {
    query <- winDialogString("Interval estimation plot sampling:\n  number of bootstrap reps", "99")
    nboot <- as.numeric(strsplit(query, ",")[[1]][1])
  }
  query <- winDialogString("Point estimation plot sampling:\n  Plot distribution of N estimator (yes/no) ", "yes")
  plot.distrib <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Interval estimation plot sampling:\n  Random seed for bootstrapping", "123123")
  seed <- as.numeric(strsplit(query, ",")[[1]][1])
  com <- paste("int.est.pl(", samp, 
               ", HT=", HT, ", ci.type='", ci.type, "', nboot=", nboot, 
               ", plot=", plot.distrib, ", seed=", seed, ")",sep="")
  print(com)
  return(com)
  }
  
  est.point.transect <- function(line.or.point) {
  if (line.or.point=='line') {
    samp <- of.class("sample.lt")
    query <- winDialogString("Point estimation transect sampling:\n  Model (half.normal, hazard.rate)", "half.normal")
    model <- strsplit(query, ",")[[1]][1]
    } else {
    samp <- of.class("sample.pt")
    model <- "half.normal"
    }
  query <- winDialogString("Point estimation transect sampling:\n  Plot detection distances and fitted fn. (yes/no) ", "yes")
  plot.distrib <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Point estimation transect sampling:\n  Conditional likelihood (yes/no) ", "yes")
  conditional <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  com <- paste(ifelse(line.or.point=='line',"point.est.lt(", "point.est.pt("), samp, 
               ", conditional=", conditional, ", plot=", plot.distrib, ", model='", model, "')",sep="")
  print(com)
  return(com)
  }
  
  est.int.transect <- function(line.or.point) {
  if (line.or.point=='line') {
    samp <- of.class("sample.lt")
    } else {
    samp <- of.class("sample.pt")
    }
  query <- winDialogString("Interval estimation transect sampling:\n  number of bootstrap reps", "99")
  nboot <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Interval estimation transect sampling:\n  Conditional likelihood (yes/no) ", "yes")
  mod.name <- winDialogString("Interval estimation transect sampling:\n  Detection function model (half.normal/hazard.rate) ", "half.normal")
  conditional <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Interval estimation transect sampling:\n  Plot distribution of N estimator (yes/no) ", "yes")
  plot.distrib <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  com <- paste(ifelse(line.or.point=='line',"int.est.lt(", "int.est.pt("), samp, 
               ", nboot=", nboot, ", conditional=", conditional, 
               ",model='", mod.name, "', plot=", plot.distrib, ")",sep="")
  print(com)
  return(com)
  }

  est.point.cr <- function(estimator="M0") {
  root.class <-  "sample.cr"
  samp <- of.class("sample.cr")
  chapmod <- FALSE
  numerical <- TRUE
  init.N <- -1
  com <- paste("point.est.cr", estimator, "(", samp,",", sep="")
  if ( estimator == "Mh" ) {
    query <- winDialogString("Point estimation capture-recapture sampling:\n  Number of mixtures in population", "2")
    num.mix <- as.integer(strsplit(query, ",")[[1]][1])
    com <- paste(com, "num.mix =", num.mix, ",", sep="")
  }
  com <- paste(com, "init.N =", init.N, sep="")
  if (estimator=="M0") com <- paste(com, ", numerical=", numerical,  sep="")
  com <- paste(com, ")", sep="")
  print(com)
  return(com)
  }

  est.int.cr <- function(estimator="M0") {
  root.class <-  "sample.cr"
  samp <- of.class("sample.cr")
  numerical <- TRUE
  init.N <- -1
  query <- winDialogString("Interval estimation capture-recapture sampling:\n  Method (normal, boot.par, boot.nonpar", "boot.nonpar")
  ci.type <- strsplit(query, ",")[[1]][1]
  query <- winDialogString("Interval estimation capture-recapture sampling:\n  number of bootstrap reps", "99")
  nboot <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Interval estimation capture-recapture sampling:\n  Plot distribution of N estimator (yes/no) ", "yes")
  plot.distrib <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  com <- paste("int.est.cr", estimator, "(", samp,",", sep="")
  if ( estimator == "Mh" ) {
    query <- winDialogString("Interval estimation capture-recapture:\n  Number of mixtures in M(h) ", "2")
    n.mix <- as.integer(strsplit(query, ",")[[1]][1])
    com <- paste(com, "num.mix =", n.mix, ",", sep="")
  }
  com <- paste(com, "init.N =", init.N, ", ci.type='", ci.type, "', nboot=", nboot,
               ", plot=", plot.distrib, ")", sep="")
  print(com)
  return(com)
  }
  
  sim.plotsam <- function() {
  popn <- of.class("population", "pars.population")  
  design <- of.class("design.pl", "pars.design.pl")  
  query <- winDialogString("Plot sampling simulation:\n  number of simulations", "99")
  nsim <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Plot sampling simulation:\n  Use MLE estimate (yes/no) ", "yes")
  HT <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", FALSE, TRUE)
  query <- winDialogString("Plot sampling simulation:\n  Random seed for simulation", "123123")
  seed <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Plot sampling simulation:\n  Plot distribution of N estimates (yes/no) ", "yes")
  plot.distrib <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  com <- paste("point.sim.pl(pop.spec=", popn, ", design.spec=", design, 
               ", B=", nsim, ", HT=", HT, ", seed=", seed, ", show=FALSE, plot=", plot.distrib, ")",sep="")
  print(com)
  return(com)
  }
  
  sim.transect <- function(line.or.point) {
  popn <- of.class("population", "pars.population")  
  if (line.or.point=="line") {
    survey <- of.class("pars.survey.lt")  
    design <- of.class("design.lt", "pars.design.lt")  
  } else {
    survey <- of.class("pars.survey.pt")  
    design <- of.class("design.pt", "pars.design.pt")  
  }
  query <- winDialogString("Transect sampling simulation:\n  number of simulations", "99")
  nsim <- as.numeric(strsplit(query, ",")[[1]][1])
  if (line.or.point=="line") {
    query <- winDialogString("Transect sampling simulation:\n  Detection function form ('half.normal' or 'hazard.rate'", "half.normal")
    model <- strsplit(query, ",")[[1]][1]
    query <- winDialogString("Transect sampling simulation:\n  Use model selection on each simulation? (yes/no) ", "no")
    model.sel <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
    } else {
      model <- "half.normal"
  }
  query <- winDialogString("Transect sampling simulation:\n  Conditional likelihood (yes/no) ", "no")
  conditional <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  query <- winDialogString("Transect sampling simulation:\n  Random seed for simulation", "123123")
  seed <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Transect sampling simulation:\n  Plot distribution of N estimates (yes/no) ", "yes")
  plot.distrib <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  show <- FALSE
  com <- paste(ifelse(line.or.point=='line',"point.sim.lt(", "point.sim.pt("), sep="") 
  com <- paste(com, "pop.spec=", popn, ", survey.spec=", survey, ", design.spec=", design, sep="")
  if (line.or.point=="line") com <- paste(com, ", model.sel=", model.sel, sep="")
   com <- paste(com, ", model='", model, "', conditional=", conditional, 
               ", B=", nsim,  ", seed=", seed, ", show=", show, ", plot=", plot.distrib, ")",sep="")
  print(com)
  return(com)
  }
  
  sim.cr <- function(estimator="M0") {
  popn <- of.class("population", "pars.population")  
  design <- of.class("design.cr")  
  survey <- of.class("pars.survey.cr")
  query <- winDialogString("Capture-recapture sampling simulation:\n  number of simulations", "99")
  nsim <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Capture-recapture sampling simulation:\n  Random seed for simulation", "123123")
  seed <- as.numeric(strsplit(query, ",")[[1]][1])
  query <- winDialogString("Capture-recapture sampling simulation:\n  Plot distribution of N estimates (yes/no) ", "yes")
  plot.distrib <- ifelse(tolower(strsplit(query, ",")[[1]][1])=="yes", TRUE, FALSE)
  chapmod <- FALSE
  numerical <- TRUE
  init.N <- -1
  com <- paste("point.sim.cr", estimator, "(", popn, ",", survey, ",", design, ",",
               ", B=", nsim, "seed=", seed, ", init.N=", init.N, ", plot=",  plot,
               ")", sep="")
  print(com)
  return(com)
  }
  
  visualize <- function(double=FALSE) {
  require(rgl)
  dens <- of.class("density.population")
  if (!double) {
    com <- paste("plot.density.sample.3d(", dens, ")", sep="")
  } else {
    query <- winDialogString("Visualization:\n  What type of sample to visualize (lt, pt, cr)", "lt")
    sample.type <- paste("sample", strsplit(query, ",")[[1]][1], sep=".")
    sampled <- of.class(sample.type)
    query <- winDialogString("Visualization:\n  Point scaling factor", "0.75")
    scale <- as.numeric(strsplit(query, ",")[[1]][1])
    com <- paste("plot.density.sample.3d(", dens, ", sampled=", sampled, ", scale.fact=", scale, ")", sep="")
    }
  print(com)
  return(com)  
  }


#		Print methods for various classes suggested
#			by Mike Meridith, WCS, June 2007
print.region <- summary.region
print.density.population <- summary.density.population
print.population <- summary.population

print.pars.survey.lt <- summary.pars.survey.lt
print.pars.survey.pt <- summary.pars.survey.pt
print.sample.cr <- summary.sample.cr
print.sample.dp <- summary.sample.dp
print.sample.lt <- summary.sample.lt
print.sample.no <- summary.sample.no
print.sample.pl <- summary.sample.pl
print.sample.pt <- summary.sample.pt
print.sample.rm <- summary.sample.rm

print.point.est.ce <- summary.point.est.ce
print.point.est.cir <- summary.point.est.cir
print.point.est.crM0 <- summary.point.est.crM0
print.point.est.crMb <- summary.point.est.crMb
print.point.est.crMh <- summary.point.est.crMh
print.point.est.crMt <- summary.point.est.crMt
print.point.est.dp <- summary.point.est.dp
print.point.est.lt <- summary.point.est.lt
# print.point.est.no <- summary.point.est.no # summary doesn't exist?
print.point.est.pl <- summary.point.est.pl
print.point.est.rm <- summary.point.est.rm

print.point.sim.crM0 <- summary.point.sim.crM0
print.point.sim.crMb <- summary.point.sim.crMb
print.point.sim.crMh <- summary.point.sim.crMh
print.point.sim.crMt <- summary.point.sim.crMt
  

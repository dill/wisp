#
# fmix file format:
# csv file with whitespace separator
#
# one bivar normal distribution per row with columns:
#  
#  column
#    #1     x-axis norm: mean
#    #2     x-axis norm: sd
#    #3     y-axis norm: mean
#    #4     y-axis norm: sd
#    #5     alpha mixes the bivar function
#

run.FShake3D <- function(xlim=c(-1,1),ylim=c(-1,1),nx=10,ny=10)
{
  limits <- paste("-l",xlim[1],xlim[2],ylim[1],ylim[2])
  size   <- paste("-s",nx,ny)
  command <- system.file("exec/FShake3D.exe", package="wisp")
  cmdline <- paste(command,"-l",xlim[1],xlim[2],ylim[1],ylim[2],"-s",nx,ny)
  shell(cmdline,wait=FALSE)
}

launch.FShake3D <- function()
{
  reg <- of.class("region")  
  regobj <- get(reg)
  xlim <- c(0, regobj$width)
  ylim <- c(0, regobj$length)
  run.FShake3D(xlim=xlim,ylim=ylim,nx=41,ny=41)
}
  
Filters.FShake3D <- matrix( c("FMix file (*.fmix)","*.fmix") ,1,2)
loadFMix.FShake3D <- function(filename) as.matrix(read.table(filename))
openFMix.FShake3D <- function() choose.files(filters = Filters.FShake3D)

dnorm2d.FShake3D <- function(x,y,param) 
{
  dnorm(x, mean=param[1],sd=param[2]) * dnorm(y, mean=param[3],sd=param[4]) * param[5]
}

sample.FShake3D <- function(fmix,nx=100,ny=100,xlim=c(-1,1),ylim=c(-1,1))
{
  nfmix<-nrow(fmix)
  x<-seq(xlim[1],xlim[2], len=nx)
  y<-seq(ylim[1],xlim[2], len=ny)
  z<-matrix(0,nx,ny)
  for (i in 1:nfmix){z<-z+outer(x,y,FUN=dnorm2d.FShake3D, param= fmix[i,])}
  z[z < 0] <- 0
  z
}

sampleDialog.FShake3D <- function(nx=100,ny=100,xlim=c(-1,1),ylim=c(-1,1))
{
  file <- openFMix.FShake3D()
  if (length(file) == 0) return(NULL)
  fmix <- loadFMix.FShake3D(file)
  return( sample.FShake3D(fmix,nx,ny,xlim,ylim) )
}
  
densityDialog.FShake3D <- function()
{
  reg <- of.class("region")
  
  regobj <- get(reg)
  
  dm <- sampleDialog.FShake3D( xlim=c(1,regobj$width), ylim=c(1,regobj$height) )
  if ( is.null(dm) ) return(NULL)
    
  nint.x <- ncol(dm)
  nint.y <- nrow(dm)

  parents <- list(wisp.id(reg, newname = as.character(substitute(reg))))

  density <- list(reg = regobj, n.interval.x = nint.x, n.interval.y = nint.y, 
        matrix = dm, parents = parents, created = date())
  class(density) <- "density.population"
  
  winMenuAddItem("Generate density","Add hotspot","enable")
  winMenuAddItem("Generate density","Set stripe","enable")
  winMenuAddItem("Generate density/Plot","Rotating surface","enable")
  winMenuAddItem("Generate density","Summary","enable")
  winMenuAddItem("Generate population","Set parameters","enable")
  
  return(density) 
}

"draw.hist2" <-
function (df=df, my.max=my.max)
{
hist(df[[2]][,1][is.finite(df[[2]][,1]) & df[[2]][,1]<my.max], main=df[1], xlab="Point estimate of total abundance")
}

"make.2d.cir" <-
function(pop, top.samp=0.5, samp.incr=0.1, male.harv=0.5, fem.harv=0.05, reps=10)
{
 num.rows <- (top.samp-.1)/samp.incr * (top.samp-.1)/samp.incr
 twod.table <- matrix(0, nrow=num.rows, ncol=5)
 twod.table <- as.data.frame(twod.table)
 names(twod.table) <- c("Pre-p","Post-p","point","se","cv")
 k <- 0
  for (i in seq(0.1, top.samp, by=samp.incr)) {
      for (j in seq(0.1, top.samp, by=samp.incr)) {
           k <- k + 1
           tex <- sim.cir.2.summary(pop, i,j, male.harv, fem.harv, reps)
           twod.table[k, 1] <- i
           twod.table[k, 2] <- j
           twod.table[k, 3] <- tex[1]
           twod.table[k,4] <- tex[2]
           twod.table[k,5] <- tex[3]

      }
  }
  twod.table

}

"make.twosex.pop" <-
function(abund=100, prop.male=0.50)
{
rm.reg<-generate.region(x.length=100, y.width=100)
rm.dens <- generate.density(rm.reg)
rm.poppars<-setpars.population(density.pop = rm.dens, number.groups = abund, size.method = "user",
             size.min = 1, size.max = 1, size.mean = 1, exposure.method = "user", exposure.min = 1,
             exposure.max = 1, exposure.mean = 1, type.values=c("Male","Female"),
             type.prob=c(prop.male,1-prop.male))
rm.pop<-generate.population(rm.poppars)

}

"run.sexratio.sims" <-
function(abund=100, ratio=0.40, nreps=500)
{
  popn <- make.twosex.pop(abund, ratio)

  d05 <- sim.cir.2(popn, .3, .3, .6, .55, nreps)
  d10 <- sim.cir.2(popn, .3, .3, .6, .50, nreps)
  d15 <- sim.cir.2(popn, .3, .3, .6, .45, nreps)
  d20 <- sim.cir.2(popn, .3, .3, .6, .40, nreps)
  d25 <- sim.cir.2(popn, .3, .3, .6, .35, nreps)
  d30 <- sim.cir.2(popn, .3, .3, .6, .30, nreps)
  d35 <- sim.cir.2(popn, .3, .3, .6, .25, nreps)
  d40 <- sim.cir.2(popn, .3, .3, .6, .20, nreps)
  d50 <- sim.cir.2(popn, .3, .3, .6, .10, nreps)
  d60 <- sim.cir.2(popn, .3, .3, .6, .00, nreps)

  ans <- numeric(0)
  ans <- rbind(ans, quantile(data.frame(d60[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d50[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d40[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d35[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d30[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d25[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d20[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d15[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d10[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- rbind(ans, quantile(data.frame(d05[[2]])$point.cir, probs=c(.05, .5, .95), na.rm=TRUE))
  ans <- cbind(c(.6,.5,.4,.35,.3,.25,.2,.15,.1,.05), ans)

  plot(ans[,1], ans[,3], xlab="Delta p", ylab="Estimated abundance and CIs",
      main=paste("Proportion of popn. male=", ratio, sep=" "),
      sub=paste("True abundance=", abund, sep=" "),
      ylim=c(min(ans[,2]),max(ans[,4])))
  segments(ans[,1], ans[,2], ans[,1], ans[,4])
  abline(h=abund, col="blue")
  text(.05, quantile(data.frame(d05[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d05[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.1, quantile(data.frame(d10[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d10[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.15, quantile(data.frame(d15[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d15[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.2, quantile(data.frame(d20[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d20[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.25, quantile(data.frame(d25[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d25[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.3, quantile(data.frame(d30[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d30[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.35, quantile(data.frame(d35[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d35[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.4, quantile(data.frame(d40[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d40[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.5, quantile(data.frame(d50[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d50[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.6, quantile(data.frame(d60[[2]])$point.cir, probs=c(.95),na.rm=TRUE),
          round(sum(is.finite(data.frame(d60[[2]])$point.cir))/nreps,3), cex=.8, pos=3)
  text(.45, 0.95*(max(ans[,4])), "Values are proportion of replicates with legitimate estmates", cex=0.75, col="blue")
}

"sim.cir.2" <-
function(popn, frac.prehunt, frac.posthunt, frac.harv.male, frac.harv.fem, B) {
call <- match.call()
stats <- c("point.cir","se.cir","coef.var")
 len <- length(stats)
    res <- matrix(0, nrow = B, ncol = len)
    res <- as.data.frame(res)
    names(res) <- stats
  
for (i in 1:B) {out.est <- two.samp.cir(popn, frac.prehunt, frac.posthunt, frac.harv.male, frac.harv.fem) 

res[i, stats] <- out.est[stats]
}
return(list(call, res))
}

"sim.cir.2.summary" <-
function(popn, frac.prehunt, frac.posthunt, frac.harv.male, frac.harv.fem, B) {
call <- match.call()
stats <- c("point.cir","se.cir","coef.var")
 len <- length(stats)
    res <- matrix(0, nrow = B, ncol = len)
    res <- as.data.frame(res)
    names(res) <- stats

for (i in 1:B) {out.est <- two.samp.cir(popn, frac.prehunt, frac.posthunt, frac.harv.male, frac.harv.fem)

res[i, stats] <- out.est[stats]
}
return(data.frame(point.mean=mean(res$point.cir, na.rm=TRUE), se.mean=mean(res$se.cir, na.rm=TRUE), cv.mean=mean(res$coef.var, na.rm=TRUE)))
}

"sim.cir.3" <-
function(popn, frac.prehunt, frac.midhunt, frac.posthunt, frac.harv.male.1, frac.harv.fem.1, frac.harv.male.2, frac.harv.fem.2, B) {
  call <- match.call()
  stats <- c("point.cir","se.cir","coef.var", "lambda.est")
	len <- length(stats)
	res <- matrix(0, nrow = B, ncol = len)
	res <- as.data.frame(res)
	names(res) <- stats
	for (i in 1:B) {
		out.est <- three.samp.cir(popn, frac.prehunt, frac.midhunt, frac.posthunt, frac.harv.male.1, frac.harv.fem.1, frac.harv.male.2, frac.harv.fem.2) 
		res[i, stats] <- out.est[stats]
	}
	return(res=res)
}

"three.samp.cir" <-
function(rm.pop, frac.prehunt, frac.midhunt, frac.posthunt, frac.harv.male.1, frac.harv.fem.1, frac.harv.male.2, frac.harv.fem.2) {

prehunt.sample.size <- frac.prehunt * length(rm.pop$groupID)
prehunt.ratio <- sum(rm.pop$types[sample(rm.pop$groupID, prehunt.sample.size)]=="Male") / prehunt.sample.size

rm.pop$harvest.1 <- logical(length(rm.pop$groupID))

harvest.male.1 <- frac.harv.male.1 * (length(rm.pop$groupID[rm.pop$types=="Male"]))
rm.pop$harvest.1[sample(rm.pop$groupID[rm.pop$types=="Male"], harvest.male.1)] <- TRUE

harvest.female.1 <- frac.harv.fem.1 * (length(rm.pop$groupID[rm.pop$types=="Female"]))
rm.pop$harvest.1[sample(rm.pop$groupID[rm.pop$types=="Female"], harvest.female.1)] <- TRUE
#--------------
midhunt.sample.size <- frac.midhunt * (length(rm.pop$groupID)-harvest.male.1-harvest.female.1)
midhunt.ratio <- sum(rm.pop$types[sample(rm.pop$groupID, midhunt.sample.size)]=="Male") / midhunt.sample.size

rm.pop$harvest.2 <- logical(length(rm.pop$groupID))

harvest.male.2 <- frac.harv.male.2 * (length(rm.pop$groupID[rm.pop$types=="Male"])-harvest.male.1)
rm.pop$harvest.2[sample(rm.pop$groupID[rm.pop$types=="Male"], harvest.male.2)] <- TRUE

harvest.female.2 <- frac.harv.fem.2 * (length(rm.pop$groupID[rm.pop$types=="Female"])-harvest.female.1)
rm.pop$harvest.2[sample(rm.pop$groupID[rm.pop$types=="Female"], harvest.female.2)] <- TRUE

#--------------
posthunt.sample.size <- frac.posthunt * (length(rm.pop$groupID)-harvest.male.1-harvest.female.1-harvest.male.2-harvest.female.2)
posthunt.ratio <- sum(rm.pop$types[sample(rm.pop$groupID[!rm.pop$harvest.1 & !rm.pop$harvest.2],posthunt.sample.size)]=="Male") / posthunt.sample.size

cat(prehunt.ratio, midhunt.ratio, posthunt.ratio, "\n")
male.abund.est <- (harvest.male.1+harvest.male.2) * prehunt.ratio * (1-midhunt.ratio) / (prehunt.ratio - midhunt.ratio)
female.abund.est <- (harvest.female.1+harvest.female.2) * posthunt.ratio * (1-midhunt.ratio) / (midhunt.ratio - posthunt.ratio)
lambda.est <- (harvest.male.1+harvest.male.2) * (1-prehunt.ratio) * (midhunt.ratio - posthunt.ratio) / 
			(harvest.female.1+harvest.female.2) * posthunt.ratio * (prehunt.ratio - midhunt.ratio)

total.abund.est <- male.abund.est + female.abund.est

if (total.abund.est > 0 ) {
	var.prehunt <- (prehunt.ratio * (1-prehunt.ratio)/prehunt.sample.size) *(total.abund.est - prehunt.sample.size)/(total.abund.est-1)
	var.midhunt <- (midhunt.ratio * (1-midhunt.ratio)/midhunt.sample.size) *
					(total.abund.est-harvest.male.1-harvest.female.1 - midhunt.sample.size)/
					(total.abund.est-harvest.male.1-harvest.female.1-1)
	var.posthunt <- (posthunt.ratio * (1-posthunt.ratio)/posthunt.sample.size) *
					(total.abund.est-harvest.male.1-harvest.female.1-harvest.male.2-harvest.female.2 - posthunt.sample.size)/
					(total.abund.est-harvest.male.1-harvest.female.1-harvest.male.2-harvest.female.2-1)
	
	male.abund.var <- ((male.abund.est + lambda.est * female.abund.est)^2 * midhunt.ratio^2 * var.prehunt +
					(male.abund.est - (harvest.male.1+harvest.male.2) + lambda.est*female.abund.est)^2 * midhunt.ratio^2*var.midhunt) /
					(prehunt.ratio - midhunt.ratio)^2
	
	female.abund.var <- ((male.abund.est + harvest.male.1+harvest.male.2+lambda.est*female.abund.est)^2 * (1-posthunt.ratio)^2*var.midhunt +
					  (male.abund.est - harvest.male.1-harvest.male.2 + lambda.est*(female.abund.est-harvest.female.1-harvest.female.2))^2 *
					  (1-midhunt.ratio)^2 * var.posthunt) / (midhunt.ratio - posthunt.ratio)^2
	covar.male.fem <- ((harvest.male.1+harvest.male.2)*(harvest.female.1+harvest.female.2)*prehunt.ratio*posthunt.ratio) /
					(prehunt.ratio - midhunt.ratio) * (posthunt.ratio - midhunt.ratio) * var.midhunt
        total.abund.var <- male.abund.var + female.abund.var + 2 * covar.male.fem
	coef.var <- sqrt(total.abund.var) / total.abund.est
}
else {
	total.abund.est = NaN
	total.abund.var = NaN
	var.total = NaN
	coef.var = NaN
}
answer <- list(point.cir=total.abund.est, se.cir=sqrt(total.abund.var), coef.var=coef.var, lambda.est=lambda.est)
return(answer)
}

"two.samp.cir" <-
function(rm.pop, frac.prehunt, frac.posthunt, frac.harv.male, frac.harv.fem) {

prehunt.sample.size <- frac.prehunt * length(rm.pop$groupID)
prehunt.ratio <- sum(rm.pop$types[sample(rm.pop$groupID, prehunt.sample.size)]=="Male") / prehunt.sample.size

rm.pop$harvest <- logical(length(rm.pop$groupID))

harvest.male <- frac.harv.male * length(rm.pop$groupID[rm.pop$types=="Male"])
rm.pop$harvest[sample(rm.pop$groupID[rm.pop$types=="Male"], harvest.male)] <- TRUE

harvest.female <- frac.harv.fem * length(rm.pop$groupID[rm.pop$types=="Female"])
rm.pop$harvest[sample(rm.pop$groupID[rm.pop$types=="Female"], harvest.female)] <- TRUE

posthunt.sample.size <- frac.posthunt * (length(rm.pop$groupID) - harvest.male - harvest.female)
posthunt.ratio <- sum(rm.pop$types[sample(rm.pop$groupID[!rm.pop$harvest],posthunt.sample.size)]=="Male") / posthunt.sample.size

estimator <- (harvest.male - (harvest.male+harvest.female) * posthunt.ratio)/(prehunt.ratio - posthunt.ratio)
if (estimator > 0 ) {
	var.prehunt <- (prehunt.ratio * (1-prehunt.ratio)/prehunt.sample.size) *(estimator - prehunt.sample.size)/(estimator-1)
	var.posthunt <- (posthunt.ratio * (1-posthunt.ratio)/posthunt.sample.size) *(estimator-harvest.male-harvest.female - posthunt.sample.size)/
					(estimator-harvest.male-harvest.female-1)
	var.total.num <- estimator^2 *var.prehunt + (estimator-harvest.male-harvest.female)^2*var.posthunt
	var.total.denom <- ( prehunt.ratio - posthunt.ratio)^2
	var.total <- var.total.num /var.total.denom
	coef.var <- sqrt(var.total) / estimator
}
else {
	estimator = NaN
	var.total = NaN
	coef.var = NaN
}
answer <- list(point.cir=estimator, se.cir=sqrt(var.total), coef.var=coef.var)
return(answer)

}



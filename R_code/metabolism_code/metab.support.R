# modified LM function for running metabolism and returning uncertainty
# JAZ; 2015-06-03



my.metab.mle <- function(do.obs, do.sat, k.gas, z.mix, irr, wtr, irr_day, wtr_day, error.type='OE', logged=T,
                         bootstrap=F, n.boot=1000, guesses, optim_method, daysSim=daysSim, ...){

  complete.inputs(do.obs=do.obs, do.sat=do.sat, k.gas=k.gas,
                  z.mix=z.mix, irr=irr, wtr=wtr, error=TRUE)

  match.arg(error.type,choices=c('OE','PE'))

  nobs <- length(do.obs)

  mm.args <- list(...)

  if(any(z.mix <= 0)){
    stop("z.mix must be greater than zero.")
  }
  if(any(wtr <= 0)){
    stop("all wtr must be positive.")
  }

  if("datetime"%in%names(mm.args)){ # check to see if datetime is in the ... args
    datetime <- mm.args$datetime # extract datetime
    freq <- calc.freq(datetime)*length(daysSim) # calculate sampling frequency from datetime
    if(nobs!=freq){ # nobs and freq should agree, if they don't issue a warning
      bad.date <- format.Date(datetime[1], format="%Y-%m-%d")
      warning("number of observations on ", bad.date, " (", nobs, ") ", "does not equal estimated sampling frequency", " (", freq, ")", sep="")
    }
  }else{ # if datetime is *not* in the ... args
    warning("datetime not found, inferring sampling frequency from # of observations") # issue a warning (note checks in addNAs)
    # NOTE: because of the checks in addNA's, it is unlikely a user would receive this warning via metab()
    # warning will only be seen through direct use of metab.bookkeep when datettime is not supplied
    freq <- nobs
  }

  chk.list <- list(do.obs, irr, do.sat, z.mix, k.gas, wtr)
  if(!all(sapply(chk.list, is.numeric)) || !all(sapply(chk.list, is.vector))){
    stop('All metab.mle inputs must be numeric vectors.')
  }

  if(!all(nobs==sapply(chk.list, length))){
    stop('All input data to metab.mle must be the same length')
  }

  Q0 <- ((diff(range(do.obs,na.rm=TRUE)) - mean(do.obs,na.rm=TRUE))^2 / length(do.obs))
  # guesses <- c(1E-3, 1E-3, log(Q0))
  guesses <- c(guesses, log(Q0))

  #We have a different number of fitted parameters depending on error type of the model
  if(error.type=='OE'){
    if(logged==T){
      guesses <- c(log(guesses[1:2]), guesses[3], log(do.obs[1]))  # exponentiating R and GPP coeff to constrain positive / negative
    }else{
      guesses <- c(guesses, do.obs[1])
    }
    fit <- optim(guesses, fn=mleNllOE, method=optim_method, do.obs=do.obs, do.sat=do.sat, k.gas=(k.gas/freq), z.mix=z.mix, irr=irr, wtr=wtr,logged=logged)
    # if(fit$convergence==1){
    #   fit <- optim(guesses/2, fn=mleNllOE, do.obs=do.obs, do.sat=do.sat, k.gas=(k.gas/freq), z.mix=z.mix, irr=irr, wtr=wtr,logged=logged)
    # }

    pars0 <- fit$par
    if(logged==T){
      pars <- c("gppCoeff"=exp(pars0[1]), "rCoeff"=-exp(pars0[2]), "Q"=exp(pars0[3]), "nll"=fit$value, "doInit"=exp(pars0[4]),"converge"=fit$convergence)
    }else{
      pars <- c("gppCoeff"=pars0[1], "rCoeff"=pars0[2], "Q"=exp(pars0[3]), "nll"=fit$value, "doInit"=pars0[4],"converge"=fit$convergence)
    }

    # bootstrapping if True
    if(bootstrap){
      bootResults<-bootstrap.metab(guesses=guesses,pars=pars0,fn=mleNllOE,do.obs=do.obs, do.sat=do.sat, error.type=error.type,freq=freq,
                                   k.gas=(k.gas/freq), z.mix=z.mix, irr=irr, wtr=wtr,logged=logged, n.boot=n.boot, ar1.resids = ar1.resids)
      gppCoeff_sd<-sd(bootResults$gppCoeff,na.rm=T)
      rCoeff_sd<-sd(bootResults$rCoeff,na.rm=T)
      doInit_sd<-sd(bootResults$doInit,na.rm=T)
      GPP_sd<-sd(bootResults$GPP,na.rm=T)
      R_sd<-sd(bootResults$R,na.rm=T)
    }

  }else if(error.type=='PE'){
    if(logged==T){
      guesses <- c(log(guesses[1:2]), guesses[3])
    }else{
      guesses <- c(guesses)
    }
    fit <- optim(guesses, fn=mleNllPE, do.obs=do.obs, do.sat=do.sat, k.gas=(k.gas/freq), z.mix=z.mix, irr=irr, wtr=wtr,logged=logged)

    pars0 <- fit$par
    if(logged==T){
      pars <- c("gppCoeff"=exp(pars0[1]), "rCoeff"=-exp(pars0[2]), "Q"=exp(pars0[3]), "nll"=fit$value,'converge'=fit$convergence)
    }else{
      pars <- c("gppCoeff"=pars0[1], "rCoeff"=pars0[2], "Q"=exp(pars0[3]), "nll"=fit$value,'converge'=fit$convergence)
    }

    # bootstrapping if True
    if(bootstrap){
      bootResults<-bootstrap.metab(guesses=guesses,pars=pars0,fn=mleNllPE,do.obs=do.obs, do.sat=do.sat, error.type=error.type,freq=freq,
                                   k.gas=(k.gas/freq), z.mix=z.mix, irr=irr, wtr=wtr,logged=logged, n.boot=n.boot, ar1.resids = ar1.resids)
      gppCoeff_sd<-sd(bootResults$gppCoeff,na.rm=T)
      rCoeff_sd<-sd(bootResults$rCoeff,na.rm=T)
      doInit_sd<-sd(bootResults$doInit,na.rm=T)
      GPP_sd<-sd(bootResults$GPP,na.rm=T)
      R_sd<-sd(bootResults$R,na.rm=T)
    }

  }else{
    stop("error.type must be either 'OE' or 'PE', Observation Error or Process Error respectively.")
  }


  #standard error based on the hessian - for pars
  # stderr=sqrt(abs(diag(solve(fit$hessian))))


  # ====================================
  # = Use fits to calculate metabolism =
  # ====================================
  GPP <- mean(pars[1]*irr_day, na.rm=TRUE) * freq/length(daysSim)
  R <- mean(pars[2]*log(wtr_day), na.rm=TRUE) * freq/length(daysSim)
  # GPP_SE<-mean(stderr[1]*irr,na.rm=T)*freq
  # R_SE<-mean(stderr[2]*log(wtr),na.rm=T)*freq
  nll<-pars[4]
  if(error.type=='OE'){
    converge<-pars[6]
  }else{
    converge<-pars[5]
  }

  if(bootstrap){
    return(list("params"=pars, "metab"=c("GPP"=GPP,"R"=R,"NEP"=GPP+R,"GPP_SD"=GPP_sd,"R_SD"=R_sd,
                                         "gppCoeff_SD"=gppCoeff_sd, "rCoeff_SD"=rCoeff_sd, "doInit_SD"=doInit_sd,"nll"=nll,"converge"=converge)))
  }else{
    return(list("params"=pars, "metab"=c("GPP"=GPP,"R"=R,"NEP"=GPP+R,"nll"=nll,"converge"=converge)))
  }
}


# =============================================
# = Bootstrapping function for MLE metabolism =
# =============================================
bootstrap.metab <- function(guesses, pars, fn, do.obs, do.sat, k.gas, z.mix, irr, wtr, error.type='OE', logged=T, n.boot=1000, ar1.resids, freq, ...){
  #Calculate DOHat given max likelihood parameter estimates
  predix <- predictDO(par=pars,do.obs=do.obs, do.sat=do.sat, k.gas=k.gas,
                      z.mix=z.mix, irr=irr, wtr=wtr,logged=logged,error.type = error.type) # returns MLE DO
  DOHat<-predix$DOHat
  res <- predix$res

  n.obs = length(do.obs)

  #If we are maintaining the ar1 component of the residuals,
  # we must estimate ar1 coeff and the ar1 residual standard deviation
  if(ar1.resids){
    ar1.lm    = lm(res[1:n.obs-1] ~ res[2:n.obs]-1)
    ar1.coeff = ar1.lm$coefficients
    ar1.sd    = sd(ar1.lm$residuals)
  }

  #Pre-allocate the result data frame
  result <- data.frame(boot.iter = 1:n.boot,gppCoeff = rep(NA,n.boot),rCoeff = rep(NA,n.boot),doInit = rep(NA,n.boot),nll = rep(NA,n.boot),
                       GPP=rep(NA,n.boot),R=rep(NA,n.boot),NEP=rep(NA,n.boot))

  for(i in 1:n.boot){
    #Randomize the residuals using one of two methods
    if(ar1.resids){ #residual randomization keeping the ar1 data structure
      simRes = rep(NA, n.obs)
      simRes[1] = sample(res[!is.na(res)],1)
      for(j in 2:n.obs){
        simRes[j] = ar1.coeff*simRes[j-1] + rnorm(n=1, sd=ar1.sd)
      }
    }else{ #Raw residual randomization
      #Randomize residuals without replacement
      simRes = sample(res[!is.na(res)], length(res), replace=FALSE)
    }

    do.sim = DOHat + simRes

    #Run metab model again with new simulated DO signal
    if(error.type=='OE'){
      boot.temp <- optim(guesses, fn=mleNllOE, do.obs=do.sim, do.sat=do.sat, k.gas=k.gas, z.mix=z.mix, irr=irr, wtr=wtr,logged=logged)
      pars0 <- boot.temp$par

      if(logged){
        pars <- c("gppCoeff"=exp(pars0[1]), "rCoeff"=-exp(pars0[2]), "Q"=exp(pars0[3]), "nll"=boot.temp$value, "doInit"=exp(pars0[4]))
      }else{
        pars <- c("gppCoeff"=pars0[1], "rCoeff"=pars0[2], "Q"=exp(pars0[3]), "nll"=boot.temp$value, "doInit"=pars0[4])
      }

      GPP <- mean(pars[1]*irr, na.rm=TRUE) * freq
      R <- mean(pars[2]*log(wtr), na.rm=TRUE) * freq
      NEP<-GPP+R
      # store result of current iteration in output data frame
      result[i,2:3]<-pars[1:2]
      result[i,4]<-pars[5]
      result[i,5]<-pars[4]
      result[i,6]<-GPP
      result[i,7]<-R
      result[i,8]<-NEP

    }else if(error.type=='PE'){
      boot.temp <- optim(guesses, fn=mleNllPE, do.obs=do.sim, do.sat=do.sat, k.gas=k.gas, z.mix=z.mix, irr=irr, wtr=wtr,logged=logged)
      pars0 <- boot.temp$par

      if(logged){
        pars <- c("gppCoeff"=exp(pars0[1]), "rCoeff"=-exp(pars0[2]), "Q"=exp(pars0[3]), "nll"=boot.temp$value)
      }else{
        pars <- c("gppCoeff"=pars0[1], "rCoeff"=pars0[2], "Q"=exp(pars0[3]), "nll"=boot.temp$value)
      }

      GPP <- mean(pars[1]*irr, na.rm=TRUE) * freq
      R <- mean(pars[2]*log(wtr), na.rm=TRUE) * freq
      NEP<-GPP+R
      # store result of current iteration in output data frame
      result[i,2:3]<-pars[1:2]
      result[i,4]<-do.sim[1]
      result[i,5]<-pars[4]
      result[i,6]<-GPP
      result[i,7]<-R
      result[i,8]<-NEP
    }
  }
  return(result)
}


# ============================================
# = The R loop for Observation Error mle NLL =
# ============================================
mleLoopOE <- function(alpha, doobs, c1, c2, beta, irr, wtr, kz, dosat){
  nobs <- length(doobs)
  a.loop <- .C("mleLoopCoe", alpha=as.double(alpha), as.double(doobs), as.double(c1), as.double(c2), as.double(beta), as.double(irr), as.double(wtr), as.double(kz), as.double(dosat), as.integer(nobs), PACKAGE="LakeMetabolizer")
  return(a.loop[["alpha"]])
}

# ============================================
# = The R loop for Process Error mle NLL =
# ============================================
mleLoopPE <- function(alpha, doobs, c1, c2, beta, irr, wtr, kz, dosat){
  nobs <- length(doobs)
  a.loop <- .C("mleLoopCpe", alpha=as.double(alpha), as.double(doobs), as.double(c1), as.double(c2), as.double(beta), as.double(irr), as.double(wtr), as.double(kz), as.double(dosat), as.integer(nobs), PACKAGE="LakeMetabolizer")
  return(a.loop[["alpha"]])
}

# ====================
# = mle NLL function =
# ====================
mleNllPE <- function(Params, do.obs, do.sat, k.gas, z.mix, irr, wtr,logged){
  if(logged==T){ #exponentiating if logged ==T
    c1 <- exp(Params[1]) #PAR coeff
    c2 <- -exp(Params[2]) #log(Temp) coeff; constraining negative
  }else{
    c1 <- Params[1] #PAR coeff
    c2 <- Params[2] #log(Temp) coeff
  }
  Q <- exp(Params[3]) # Variance of the process error

  # See KalmanDO_smooth.R comments for explanation of beta
  kz <- k.gas/z.mix # K and Zmix are both vector of length nobs
  beta <- exp(-kz) # This beta is for using the differential equation form

  # Set first true value equal to first observation
  alpha <- rep(0, length(do.obs))
  alpha[1] <- do.obs[1]#Let's give this model some starting values

  #R version of C loop
  #for(i in 2:length(do.obs)){
  #	a1 <- c1*irr[i-1] + c2*log(wtr[i-1]) + kz[i-1]*do.sat[i-1]
  #	alpha[i] <- a1/kz[i-1] + -exp(-kz[i-1])*a1/kz[i-1] + beta[i-1]*alpha[i-1] # NOTE: beta==exp(-kz); kz=K/Zmix
  #}
  alpha <- mleLoopPE(alpha=alpha, doobs=do.obs, c1=c1, c2=c2, beta=beta, irr=irr, wtr=wtr, kz=kz, dosat=do.sat)

  return(-sum(dnorm(do.obs, alpha, sd=sqrt(Q), log=TRUE), na.rm=TRUE))
}#End function

# ====================
# = mle NLL function =
# ====================
mleNllOE <- function(Params, do.obs, do.sat, k.gas, z.mix, irr, wtr, logged){
  if(logged==T){ #exponentiating if logged ==T
    c1 <- exp(Params[1]) #PAR coeff
    c2 <- -exp(Params[2]) #log(Temp) coeff; constraining negative
    # Set first true value equal to first observation
    alpha <- rep(0, length(do.obs))
    alpha[1] <- exp(Params[4]) #Free varying initial DO value
  }else{
    c1 <- Params[1] #PAR coeff
    c2 <- Params[2] #log(Temp) coeff
    # Set first true value equal to first observation
    alpha <- rep(0, length(do.obs))
    alpha[1] <- Params[4] #Free varying initial DO value
  }
  Q <- exp(Params[3]) # Variance of the process error

  # See KalmanDO_smooth.R comments for explanation of beta
  kz <- k.gas/z.mix # K and Zmix are both vector of length nobs
  beta <- exp(-kz) # This beta is for using the differential equation form

  alpha <- mleLoopOE(alpha=alpha, doobs=do.obs, c1=c1, c2=c2, beta=beta, irr=irr, wtr=wtr, kz=kz, dosat=do.sat)

  return(-sum(dnorm(do.obs, alpha, sd=sqrt(Q), log=TRUE), na.rm=TRUE))
}#End function


# ================================================
# = prediction function given modeled parameters =
# = returns DO predictions =
# ================================================
predictDO<-function(par, do.obs, do.sat, k.gas, z.mix, irr, wtr, error.type='OE',logged=T ,...){
  match.arg(error.type,choices = c('OE','PE'))
  if(error.type=='PE'){
    if(logged==T){
      c1 <- exp(par[1]) #PAR coeff
      c2 <- -exp(par[2]) #log(Temp) coeff
    }else{
      c1 <- par[1] #PAR coeff
      c2 <- par[2] #log(Temp) coeff
    }
    # Set first true value equal to first observation
    alpha <- rep(0, length(do.obs))
    alpha[1] <- do.obs[1]#Let's give this model some starting values
  }
  if(error.type=='OE'){
    if(logged==T){
      c1 <- exp(par[1]) #PAR coeff
      c2 <- -exp(par[2]) #log(Temp) coeff
      # Set first true value equal to first observation
      alpha <- rep(0, length(do.obs))
      alpha[1] <- exp(par[3])#Let's give this model some starting values
    }else{
      c1 <- par[1] #PAR coeff
      c2 <- par[2] #log(Temp) coeff
      # Set first true value equal to first observation
      alpha <- rep(0, length(do.obs))
      alpha[1] <- par[3]#Let's give this model some starting values
    }
  }

  # See KalmanDO_smooth.R comments for explanation of beta
  kz <- k.gas/z.mix # K and Zmix are both vector of length nobs
  beta <- exp(-kz) # This beta is for using the differential equation form


  # R version of C loop
  for(i in 2:length(do.obs)){
   if(kz[i-1]!=0){
     a1 <- c1*irr[i-1] + c2*log(wtr[i-1]) + kz[i-1]*do.sat[i-1]
     alpha[i] <- a1/kz[i-1] + -exp(-kz[i-1])*a1/kz[i-1] + beta[i-1]*alpha[i-1] # NOTE: beta==exp(-kz); kz=K/Zmix
   }else{
     alpha[i]<-alpha[i-1] + c1*irr[i-1]+c2*log(wtr[i-1])
   }
  }

  #Compare observed and predicted DO; calculate residuals
  #Exclude from calculation any cases where DOObs=NA
  if (any(is.na(do.obs)))
  {
    NAObs <- which(is.na(do.obs))
    res <- do.obs[-NAObs] - alpha[-NAObs]
  } else

  {
    res <- do.obs - alpha
  }

  nRes <- length(res)
  SSE <- sum(res^2)
  sigma2 <- SSE/nRes

  #Set up output structure
  dataOut <- list(DOHat=alpha,res=res)

  #Return dataOut
  return(dataOut)

}


my.metab <- function(data, method = NULL, wtr.name="wtr", irr.name="irr", do.obs.name="do.obs", nDaysSim, sunrise, percentReqd = .80, ...){

  m.args <- list(...)

  #Rename the WTR column to be used (must be wtr to easily be passed to meta.* functions)
  if(wtr.name != "wtr"){
    if(!"wtr"%in%names(data)){
      names(data)[names(data)==wtr.name] <- "wtr"
    }else{
      data[,"wtr"] <- data[,wtr.name]
    }
  }

  if(irr.name != "irr"){
    if(!"irr"%in%names(data)){
      names(data)[names(data)==irr.name] <- "irr"
    }else{
      data[,"irr"] <- data[,irr.name]
    }
  }

  if(do.obs.name != "do.obs"){
    if(!"do.obs"%in%names(data)){
      names(data)[names(data)==do.obs.name] <- "do.obs"
    }else{
      data[,"do.obs"] <- data[,do.obs.name]
    }
  }


  # ===================
  # = Identify method =
  # ===================
  possibleMethods <- c("bookkeep", "bayesian",  "kalman", "ols", "mle")
  mtd <- match.arg(method, possibleMethods)

  mtdCall <- paste("my.metab",mtd, sep=".")

  # ==============
  # = Groom data =
  # ==============
  # Removes days with many NA's:
  data1 <- addNAs(data[complete.cases(data),], percentReqd = percentReqd) # note that addNAs ALSO checks for POSIXct datetime, and adds year/doy
  data2 <- data1[complete.cases(data1),]

  # ==================================
  # = Prepare to apply metab to data =
  # ==================================
  ids <- id(list(data2[,"year"],trunc(data2[,"doy"]))) # ID chunks to be analyzed
  ids <- as.integer(ids - (min(ids)-1))
  nid <- length(unique(ids))
  if(sunrise){
    ids<-rep(NA,length(ids))
    days<-unique(as.Date(data2$datetime))
    for(j in 1:(length(days)-1)){
      ids[data2$datetime>data2$sunrise[min(which(as.Date(data2$sunrise)==days[j]))]&data2$datetime<data2$sunrise[min(which(as.Date(data2$sunrise)==days[j+1]))]]<-j
    }
    data2<-data2[!is.na(ids),]
    ids<-ids[!is.na(ids)]
  }
  nid <- length(unique(ids))
  results <- vector("list", nid)
  nDaysSim<-trunc(nDaysSim/2) # cutting in half the days over which to simulate (centered simulation)

  # ==================================
  # = Apply metab to subsets of data =
  # ==================================
  for(i in unique(ids)){

    daysSim<-c(i-nDaysSim,i+nDaysSim) #
    if(daysSim[1]<unique(ids)[1]){
      daysSim[1]<-unique(ids)[1]
    }
    if(daysSim[2]>unique(ids)[nid]){
      daysSim[2]<-unique(ids)[nid]
    }
    daysSim<-seq(daysSim[1],daysSim[2]) #sequence of days over which to simulate

    poss.args <- c("do.obs","do.sat","k.gas","z.mix", "irr", "wtr", "datetime") # data2 columns that could correspond to arguments
    used.args <- poss.args[poss.args%in%names(data2)] # assuming arguments are used if they are in data2
    largs0 <- as.list(data2[ids%in%daysSim, used.args]) # subsetting columns of data2 to used.args (just a safety check, probably not needed)
    largs0 <- c(largs0,list(daysSim=daysSim),list(irr_day=data2[i==ids,'irr']),list(wtr_day=data2[i==ids,'wtr']))
    largs <- c(largs0, m.args[!names(m.args)%in%names(largs0)]) # adding on any other arguments supplied via ...
    # note that in largs, argument supplied through data/data2/poss.args take precedent over arguments from ...

    # print(paste("Analyzing day #", i)); flush.console(); # Is this annoying? I'm commenting-out
    results[[i]] <- do.call(mtdCall, largs) # this is where all of the work happens
  }
  answer0 <- conquerList(results, naming=data.frame("year"=data2[!duplicated(ids),"year"], "doy"=trunc(data2[!duplicated(ids),"doy"])))


  a0.names <- names(results[[1]])

  # =======================================================
  # = Add non-metab list elements as attributes to output =
  # =======================================================
  # only need to add attributes if it's a list (more than 1 element, not a data frame)
  if(length(a0.names)>1 & is.list(answer0) & !is.data.frame(answer0)){

    names(answer0) <- a0.names
    answer <- answer0$metab
    for(i in 1:length(a0.names)){
      if(a0.names[i]=="metab"){next}
      if(a0.names[i]=="smoothDO"){ # do a little extra attribute work if smoothDO
        t.sDO <- answer0[[a0.names[i]]] # grab the element of the list that contains smoothed DO concs
        t.sDO <- t.sDO[,!names(t.sDO)%in%c("doy","year")] # remove the columns that are the year/ doy
        attr(answer, "smoothDO.vec") <- c(t(t.sDO)) # provide the smoothed DO as a long vector, instead of each row containing nobs+2 columns of smoothed DO from a given day
      }
      attr(answer, a0.names[i]) <- answer0[[a0.names[i]]] # assign non "metab" list element as attribute
    }

  }else{ # if the list only has one element, or if the list is really a data.frame, then answer0 is what we want
    answer <- answer0
  }

  return(answer)
}

#rdb
byeShort <- function(X, percentReqd=0.80, Expected=288, ToCount="doy", TruncToCount=TRUE, By=c("year","doy")){
  #there is almost certainly a better way to do this
  #in addition to checking for missing values, also checks for duplicates (checks duplicates first, removes those, then checks for missing)
  # NOTE: see notes in addNAs about the presence of "doy" and "year" columns in the input to byeShort
  require(plyr)
  dups <- function(x) x[!duplicated(round(x[,ToCount],9)),]
  X <- ddply(X, setdiff(By, ToCount), dups)
  ByInd <- data.frame(X[,By], "IND"=1:nrow(X))
  which_nrow <- function(x){ c("Size"=nrow(x), "Start"=min(x[,"IND"]), "Stop"=max(x[,"IND"]))}
  Sizes <- ddply(trunc(ByInd), By, which_nrow)
  TooShort <- Sizes[which(Sizes[,"Size"] < Expected*percentReqd), c("Start","Stop")]
  Start2Stop <- function(x) x[1]:x[2] #these last two steps could probably be combined into an is.element() approach that would be simpler
  WaveTo <- unlist(apply(TooShort, MARGIN=1, FUN=Start2Stop), use.names=FALSE)
  print(paste("Points removed due to incomplete day or duplicated time step:",length(WaveTo), sep=" "))
  flush.console()
  if(length(WaveTo)!=0){
    Xr <- X[-WaveTo,]
  }else{
    Xr <- X
  }
  return(Xr)
}

addNAs <- function(x, percentReqd = 0.8, ...){
  dateL <- grepl(".?date?.", names(x), ignore.case=TRUE) # matches anything with "date" in it, regardless of what else is or is not there
  dL <- grepl("^[dD][oO][yY]$", names(x)) # matches doy, regardless of case
  yL <- grepl("^[yY]ear4?$", names(x))# matches Year, year, year4, Year4

  # =============================================================
  # = The datetime checks result in error if conditions not met =
  # =============================================================
  if(any(dateL)){ # look for the date column
    names(x)[dateL] <- "datetime"
  }else{
    # warning("No 'date' column found")
    stop("No 'datetime' column found")
  }
  if(!"POSIXct"%in%class(x[,"datetime"])){ # make sure the date column is POSIXct (note that if date column is not found, you get this error)
    stop("date column must be POSIXct")
  }

  # ===============================================
  # = If doy/ year aren't found, they'll be added =
  # ===============================================
  # Because we are requiring POSIXct datetime, these values can be generated if they're missing
  if(any(dL)){ # look for "day of year column"
    names(x)[dL] <- "doy"
  }else{
    # NOTE: if a "doy" column is not created, byeShort will not work.
    # NOTE: if a "doy" column is not created here, it will have to be created in byeShort anyway
    x[,"doy"] <- date2doy(x[,"datetime"])
    # warning("No 'doy' column found")

  }
  if(any(yL)){ # look for "year" column
    names(x)[yL] <- "year"
  }else{
    # NOTE: if a "year" column is not created, byeShort will not work.
    # NOTE: if a "year" column is not created here, it will have to be created in byeShort anyway
    x[,"year"] <- as.integer(format.Date(x[,"datetime"], format="%Y"))
    # warning("No 'year' column found")
  }

  rdig <- 4
  Mode <- function(x){ # note that this function is now in the helper.functions.R file
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  ex <- round(Mode(1/diff(x[,"doy"]))) # note that this is the freq that's calculated in metab.xx()
  mins <- 1/ex*24*60
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){abs(x - round(x)) < tol}
  if(!is.wholenumber(mins)){warning("Time between samples not whole number")}
  x1 <- byeShort(X=x, percentReqd = percentReqd, Expected=ex, ToCount="doy", TruncToCount=TRUE)
  if(nrow(x1)==0){
    return(x1)
  }
  Range <- range(x1[,"datetime"]) #intentionally not truncating (having already used byeShort, I don't have to worry about starting and stopping at a specific time each day)
  Ideal <- data.frame("datetime"=seq(Range[1], Range[2], by=paste(mins, "mins")))

  print(paste("NA's added to fill in time series:",dim(Ideal)[1]-dim(x1)[1], sep=" "))
  flush.console()
  x2 <- merge(x1, Ideal, all.x=TRUE, all.y=TRUE)
  if(any(yL)){x2[,"year"] <- approx(x2[,"datetime"], x2[,"year"], xout=Ideal[,1])$y}
  if(any(dL)){x2[,"doy"] <- approx(x2[,"datetime"], x2[,"doy"], xout=Ideal[,1])$y}

  return(x2)
}

has.vars <- function(data, var.names){

  if(!is(data, 'data.frame')){
    stop('Data must be of class data.frame')
  }

  header = names(data)

  has.var = rep(FALSE, times=length(var.names))

  for(i in 1:length(var.names)){
    has.var[i] = any(grepl(paste(var.names[i], '[\\w_]?', sep=''), header, ignore.case=TRUE))
  }

  return(has.var)
}

#'@title subsets data.frame according to header names
#'@description
#'subsets \code{data} according to header names
#'
#'@usage
#'get.vars(data, var.names)
#'@param data Object of class data.frame
#'@param var.names A character vector of names to get from \code{data}
#'
#'@return An object of class data.frame
#'
#'@keywords methods

#'@author
#'Luke A. Winslow
#'@seealso
#'\link{has.vars}
#'\link{rmv.vars}
#'@export
get.vars <- function(data, var.names){

  if(!is(data, 'data.frame')){
    stop('Data must be of class data.frame')
  }

  header = names(data)

  datetimeI = grepl('datetime', header, ignore.case=TRUE)

  if(!any(datetimeI)){
    stop("Can't find datetime column in supplied data.")
  }

  varI = rep(FALSE, times=length(var.names))

  for(i in 1:length(var.names)){
    varI = varI | grepl(paste(var.names[i], '[\\w_]?', sep=''), header, ignore.case=TRUE)
  }

  if(!any(varI)){
    stop("No variable pattern matches:", paste(var.names, collapse=' '))
  }

  return(data[, varI | datetimeI])
}

#'@title gets surface water temperatures
#'@description
#'grabs best available data for surface water temperature
#'
#'@param data Object of class data.frame
#'@param s.range a numeric vector of length=2 with the range for depth measurements to still be considered 'surface'
#'@return An object of class data.frame
#'
#'@keywords methods

#'@author
#'Jordan S. Read
#'@seealso
#'\link{has.vars}
#'\link{get.vars}
#'\link{rmv.vars}
#'@export
get.Ts <- function(data,s.range=c(0,1)){
  wtr <- get.vars(data,'wtr')
  datetimeI <- var.indx(wtr,'datetime')
  depths <- c(NA, get.offsets(wtr)) # append NA for datetime col
  depths[depths > s.range[2] | depths < s.range[1]] <- NA
  varI <- which.min(depths)

  if (length(varI)==0){
    stop(paste0('no matches for water temperatures within depth range ',s.range[1],' to ',s.range[2]))
  }

  # want to use this here, but it returns wtr_0 and wtr_0.5 etc.
  #Ts <- get.vars(wtr,head.nm[varI])

  Ts <- wtr[, c(datetimeI,varI)]
  return(Ts)
}

#'@title subsets data.frame according to header names
#'@description
#'subsets \code{data} according to header names. Excludes all matches to \code{var.name}
#'
#'@usage
#'rmv.vars(data, var.name, ignore.missing=TRUE, ignore.offset=FALSE)
#'@param data Object of class data.frame
#'@param var.name A character vector of names to remove from \code{data}
#'@param ignore.missing Boolean, should an error be thrown if no matching data found
#'@param ignore.offset Should the numerical offset be ignored in the match, (e.g. all \code{wtr} columns removed, or \code{wtr_0} specifically)
#'
#'@return An object of class data.frame
#'
#'@keywords methods

#'@author
#'Luke A. Winslow
#'@seealso
#'\link{has.vars}
#'\link{get.vars}
#'@export
rmv.vars <- function(data, var.name, ignore.missing=TRUE, ignore.offset=FALSE){
  if(ignore.offset){
    varI = var.indx(data, var.name)
  }else{
    varI = grep(var.name, names(data), ignore.case=TRUE)
  }

  if(length(varI) > 0){
    varI = varI * -1
    return(data[, varI])
  }else{
    if(!ignore.missing){
      stop('No variable by that name found')
    }
  }

}

#'@title finds matching column names in data.frame
#'@description
#'returns index of column matches for \code{data} according to header names matches with \code{var.names}.
#'
#'@usage
#'var.indx(data, var.name)
#'@param data Object of class data.frame
#'@param var.name A character vector of names to find matches with \code{data}
#'
#'@return a boolean vector with same length as \code{var.names}
#'
#'@keywords methods

#'@author
#'Luke A. Winslow
#'@seealso
#'\link{has.vars}
#'\link{get.vars}
#'\link{rmv.vars}
#'@export
var.indx <- function(data, var.name){
  if(length(var.name) != 1){
    stop('var.indx only operates on one variable at a time')
  }

  header = names(data)

  indx = grep(paste(var.name, '[\\w_]?', sep=''), header, ignore.case=TRUE)

  return(indx)
}



# test edit
# =============================================
# = Function to predict dimensions of merge() =
# =============================================
# RDB 07-May-2014
# Predict merge() dimensions
# Both desired (no duplicates) and expected (how merge will behave) dimensions
# 'all' argument corresponds to the argument of the same name in merge()
pred.merge <- function(x1, x2, all=FALSE){
  common.names <- intersect(names(x1), names(x2))

  if(length(common.names)>1){
    fact1 <- do.call(paste, as.list(x1[,common.names])) #'factors' from x1
    fact2 <- do.call(paste, as.list(x2[,common.names])) # factors from x2
  }else{
    fact1 <- x1[,common.names]
    fact2 <- x2[,common.names]
  }


  fInt <- intersect(fact1, fact2) # common elements of fact1 and fact2, same as desired.aF (see below)

  o1 <- table(fact1[fact1%in%fInt])
  o2 <- table(fact2[fact2%in%fInt])
  out.aF <- sum(o1*o2)

  if(all){ # if you used all=TRUE in merge()
    just1 <- sum(fact1%in%setdiff(fact1, fact2))
    just2 <- sum(fact2%in%setdiff(fact2, fact1))

    out.aT <- just1 + just2 + out.aF # This is what merge will give when all=TRUE
    desired.aT <- sum(!duplicated(c(fact1, fact2))) # non-duplicated output if all=TRUE

    list(desired=desired.aT, merge=out.aT)

  }else{
    out.aF # This is what merge *will* output
    desired.oF <- length(fInt) # Length of desired output, assuming you don't want duplicated join rows

    list(desired=desired.oF, merge=out.aF)
  }
}



# =======================
# = round.time Function =
# =======================
# RDB 16May2014
# example: round.time(x, "5 minutes")
# example: round.time(x, "90 min")
# example: round.time(x, "0.5 hours")
# x is a time format, preferably POSIXct, or can be coerced w/ as.POSIXct
# if x needs to be converted to POSIX, define input.format if x currently isn't in a 'standard unambiguous format'
# default output.format=NULL leads to output of class POSIXct, character otherwise
round.time <- function(x, units, input.format=NULL, output.format=NULL){
  # x = head(t.sonde0.na2[,"date"], 20) + 120
  # units = "df.345 min"
  # Check for invalid input classes
  stopifnot(
    is.character(units) &
      (is.null(output.format) | is.character(output.format)) &
      (is.null(input.format) | is.character(input.format))
  )

  # Determine time unit
  unit.choices <- c("sec", "min", "hour", "day")
  choices.or <- paste(unit.choices, collapse="|")
  unit.pattern <- paste(".*(", choices.or, ").*", sep="")
  unit <- gsub(unit.pattern, "\\1", units)
  if(is.na(unit)){stop("not a valid unit, use sec, min, hour, or day")}
  which.choice <- which(unit==unit.choices)

  # Determine time interval
  u.time.pattern <- "(?:[0-9]+\\.[0-9]+)|(?:[0-9]+\\.)|(?:\\.[0-9]+)|(?:[0-9]+)"
  u.time.char <- regmatches(units, regexpr(u.time.pattern, units, perl=TRUE))
  u.time <- as.numeric(u.time.char)
  u.time <- ifelse(is.na(u.time), 1, u.time)

  unit.cutoff <- switch(unit, sec=60, min=60, hour=24, day=1)

  # =========================================================================
  # = Check for invalid input (before slow [attempted] conversion to POSIX) =
  # =========================================================================
  if(sign(u.time)==-1L){
    stop("time interval must be positive")
  }
  # Deal with case where units are 1 second (or less)
  if(unit=="sec" & u.time<=1L){
    return(format.Date(x, format=output.format))
  } else

    # Fractional time intervals  convert to smaller unit
    if((trunc(u.time)-u.time)!=0){
      if(sign(u.time)==1L){
        while((trunc(u.time)-u.time)!=0){
          if(unit=="sec"){stop("time interval must be an integer when converted to units of seconds")}
          unit <- unit.choices[which.choice-1]
          which.choice <- which(unit==unit.choices)
          unit.cutoff <- switch(unit, sec=60, min=60, hour=24)
          u.time <- unit.cutoff*u.time
        }
      }else{
        stop("time interval must be positive")
      }
    } else

      # Deal with case where units are days
      if(unit=="day"){
        if(u.time==1){
          return(format.Date(trunc.POSIXt(x + 43200, units = units), format=output.format))
        }else{
          stop("units must be <= 1 day")
        }
      } else

        # Deal w/ cases where time interval is 1 unit
        if(u.time==1){
          unit <- unit.choices[which.choice-1]
          which.choice <- which(unit==unit.choices)
          unit.cutoff <- switch(unit, sec=60, min=60, hour=24)
          u.time <- unit.cutoff
        }

  # Deal with cases where time interval is > 1 of a larger unit
  # Note that this follows up on case where u.time is > 1 and is not an integer
  if(u.time>unit.cutoff){
    u.time <- u.time%%unit.cutoff
    mod.mess <- paste("Rounding to units =", u.time, unit) # may or may not want to make this a warning ...
    warning(mod.mess)
  }

  # =============================================
  # = Convert to POSIX, or if can't, give error =
  # =============================================
  if(!"POSIXct"%in%class(x)){
    if(is.null(input.format)){
      x <- as.POSIXct(x)
    }else{
      x <- as.POSIXct(x, format=input.format)
    }
  }

  # ===========================================================
  # = Matching units (e.g., min) and unit multiples (e.g., 5) =
  # ===========================================================

  which.choice <- which(unit==unit.choices)
  form.unit <- c("%S", "%M", "%H", "%d")[which.choice]
  mult <- as.integer(format.Date(x, format=form.unit))/u.time
  after <- round(mult, 0)*u.time
  # direction <- sign(after-before)

  # trunc.unit <- unit.choices[min(which.choice+1, length(unit.choices))]
  trunc.unit <- unit.choices[min(which.choice+1, length(unit.choices))]
  rounded <- trunc.POSIXt(x, trunc.unit) + switch(unit, sec = 1, min = 60, hour = 3600, day = 86400)*after
  if(!is.null(output.format)){
    return(format.Date(rounded, format=output.format))
  }else{
    return(rounded)
  }
}



# ==================
# = Conquer a List =
# ==================
#RDB
conquerList <- function(x, naming=NULL){
  # If x is not a list, don't bother
  if(!is.list(x) | is.data.frame(x)){return(x)}

  s1 <- length(x)
  s2 <- length(x[[1]])
  u1 <- unlist(x, recursive=FALSE)
  stopifnot(length(u1)==s1*s2)


  if(is.data.frame(x[[1]])){
    single.row <- nrow(x[[1]]) == 1L
  }else{
    single.row <- FALSE
  }

  # return value from ldply() if it will work (e.g., if each element of list x contains a row of a data frame)
  if(single.row & is.list(x)){ # the checking for is.list() is a bit redundant with earlier check
    return(cbind(naming, ldply(x)))
  }

  #
  s2C <- unlist(lapply(x[[1]], class))
  cqd <- vector("list", s2)
  for(i in 1:s2){
    ti <- seq(i, s1*s2, s2)
    tl <- vector("list", s1)
    for(j in 1:s1){
      tl[[j]] <- u1[[ti[j]]]
    }
    if(is.data.frame(tl[[1]])|!is.list(tl[[1]])){
      if(!is.null(naming)){
        cqd[[i]] <- cbind(naming,ldply(tl))
      }else{
        cqd[[i]] <- ldply(tl)
      }
    }else{
      cqd[[i]] <- llply(tl)
    }
  }
  return(cqd)
}

# =======================================================
# = Simple way of estimating watts entering water layer =
# =======================================================
#RDB
#'@export
watts.in <- function(top, bot, irr, z1perc){
  # top = the top of the layer in meters (e.g., 2)
  # bottom = the bottom of the layer in meters (e.g., 4)
  # irr = PAR, measured in uE
  # z1perc = depth of 1 percent surface light, measured in meters (e.g., 4)

  watts <- 0.2174*irr # convert PAR to watts/m^2 (0.2174)
  kd <- log(0.01)/-z1perc # calculate an average kd for the photic zone
  watts*exp(-kd*top) - watts*exp(-kd*bot) # Estimate watts gained as the difference between watts entering at the top and exiting at the bottom
}

# ==================
# = Calculate Mode =
# ==================
#RDB
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ================================
# = Calculate sampling frequency =
# ================================
calc.freq <- function(datetime){
  freq <- round(Mode(1/diff(date2doy(datetime))))
}

# =================================
# = Checks all inputs for NA vals =
# =================================
# if error=TRUE, will throw an error when
# NA in input is found
complete.inputs <- function(..., error=FALSE){

  inputs = list(...)
  for(i in 1:length(inputs)){
    if(!is.null(inputs[[i]]) && any(is.na(inputs[[i]]))){
      if(error){
        stop('Input ', names(inputs[i]), ' contains NA value')
      }else{
        return(FALSE)

      }
    }
  }
  return(TRUE)
}


date2doy <- function(x){
  stopifnot(any(grepl("^POSIX",class(x[1]))))
  day <- as.numeric(format(x, "%j"))
  pat <- quote("([0-9]{2}:){2}[0-9]{2}")
  midnight <- as.POSIXct(gsub(pat, "00:00:00", x), tz="GMT")
  frac <- as.numeric(difftime(x, midnight, units="days"))
  day+frac
}



ma.weighted<-function(x,cv,n=5){
  # x is the time series data mean or estimate;
  # cv is the coefficient of variation of the time series data;
  # n is the moving average window
  out<-rep(NA,length(x))
  for(i in 1:length(x)){
    # check to see if it's at the beginning or end of data frame
    halfN<-round(n/2)
    if(halfN>=i|halfN>(length(x)-i)){
      next
    }else{
      weights<-(1/(cv[(i-halfN):(i+halfN)]))/sum(1/(cv[(i-halfN):(i+halfN)])) #find cv of current ma window
      curX<-x[(i-halfN):(i+halfN)] # raw data of current ma window
      out[i]<-sum(weights*curX) # output of weight ma
    }
  }
  return(out)
}


get.drho_dz <- function(wtr, depths){
  numDepths = length(wtr)

  rhoVar = water.density(wtr)

  drho_dz = vector(mode="double", length=numDepths-1);

  #Calculate the first derivative of density
  for(i in 1:numDepths-1){
    drho_dz[i] = ( rhoVar[i+1]-rhoVar[i] )/( depths[i+1] - depths[i] );
  }
  drho_dz
}


#' @title Find and drop the datetime column from the datatable
#'
#' @description Liberally looks for a datetime column and drops it,
#'  returning a data.frame with only water temperature. Errors if datetime column is
#'  ambiguous. Warns if there is no match.
#' @param data data arg
#' @param error defaults to FALSE
#'
#' @return A data.frame with only the data, after datetime has been dropped

drop.datetime = function(data, error=FALSE){
  datetime.pattern = "(datetime|timestamp|time|date)"

  header = names(data)
  dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)

  if(length(dt_indx) < 1){
    if(error){
      stop('Unable to find a datetime column. Datetime column was supplied.')
    }else{
      warning('Unable to find a datetime column. Assuming no datetime column was supplied.')
      return(data)
    }

  }else if(length(dt_indx) > 1){
    stop('datetime column ambiguity. You can only have one column of datetime.')
  }

  return(data[,-dt_indx, drop=FALSE])
}

#' @title Search for and return the datetime column from a ts data.frame
#'
#' @description Warns if unavailable then returns NULL.
#'
#' @inheritParams drop.datetime
#'
get.datetime = function(data, error=FALSE){
  datetime.pattern = "(datetime|timestamp|time|date)"

  header = names(data)
  dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)

  if(length(dt_indx) < 1){
    if(error){
      stop('Unable to find a datetime column.')
    }else{
      warning('Unable to find a datetime column, attempting to ignore.')
      return(NULL)
    }
  }else if(length(dt_indx) > 1){
    stop('datetime column ambiguity. You can only have one column of datetime.')
  }

  return(data[,dt_indx])
}

# lake metabolism estimates for GLEON Catchment and metabolism group
# 2015-05-19;
# for(i in 12:17){
library(dplyr)

i=10
toRm=ls()
toRm=toRm[-which(toRm=='i')]
rm(list=toRm)
library(LakeMetabolizer)
source('R_code/metabolism_code/metab.support.R')
source('R_code/metabolism_code/fillHoles.R')
source('R_code/metabolism_code/floorMins.R')
# library(xts) # useful for timeseries visulization
Sys.setenv(tz='GMT')

dir<-'data/metab_data/' # directory where data is stored
lakes<-list.files(dir)
lake<-lakes[i] # current lake for metabolism estimates
metaData<-read.csv('data/metadataLookUp.csv',stringsAsFactor=F)
alt<-metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
wnd.z<-metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
lake.area<-metaData$Surface.Area..m2.[metaData$Lake.Name==lake] # lake area in m2
do.z<-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees

# load time series metabolism data
doobs<-load.ts(file.path(dir,lake,paste(lake,'_doobs.txt',sep='')))
wtr<-load.ts(file.path(dir,lake,paste(lake,'_wtr.txt',sep='')))
wnd<-load.ts(file.path(dir,lake,paste(lake,'_wnd.txt',sep='')))
par<-load.ts(file.path(dir,lake,paste(lake,'_par.txt',sep='')))
if(ncol(doobs)>2){
  doobs<-doobs[,which(colnames(doobs)%in%tolower(c('datetime','doobs')))]
}
if(ncol(wnd)>2){
  wnd<-wnd[,which(colnames(wnd)%in%tolower(c('datetime','wnd')))]
}
timeStep<-as.numeric(diff(doobs$datetime)[1]) # time step difference for doobs

if(length(grep('_',colnames(wtr[,2:ncol(wtr)])))<(ncol(wtr)-1)){
  colnames(wtr)[2:ncol(wtr)]<-gsub('wtr','wtr_',colnames(wtr)[2:ncol(wtr)])
}

# can't have 0 meter temp depth; change to 0.1m if so
if(0%in%get.offsets(wtr)){
  colnames(wtr)[which(0==get.offsets(wtr))+1]<-'wtr_0.1'
}

doobs <- doobs %>%
  arrange(datetime)
wtr <- wtr %>%
  arrange(datetime)
wnd <- wnd %>%
  arrange(datetime)
par <- par %>%
  arrange(datetime)

# making time step all the same
#Make all data sets extend from startTime to endTime by timeStep
#Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
# #Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
if(lake=='Trout'|lake=='Feeagh'){
  timeStep=10 # forcing Trout and Feeagh data to 10 min timestep
  if(lake=='Trout'){
    wtr<-wtr[,!colnames(wtr)=='wtr_0.1'] #Trout shallow temp pendent is consistently lower than 1 meter pendant so it was removed
  }
}
if(lake=='Mendota'){
  timeStep=10 # Mendota par and wtr are 60 mins; linearly interpolating below
}

doobs$datetime <- floorMins(doobs)
par$datetime <- floorMins(par)
wnd$datetime <- floorMins(wnd)
wtr$datetime <- floorMins(wtr)

#Remove rows with duplicate datetime stamps (and warn)
notDupRows <- findNotDupRows("doobs")
doobs <- doobs[notDupRows,]
notDupRows <- findNotDupRows("par")
par <- par[notDupRows,]
notDupRows <- findNotDupRows("wnd")
wnd <- wnd[notDupRows,]
notDupRows <- findNotDupRows("wtr")
wtr <- wtr[notDupRows,]

doTemp<-data.frame(datetime=wtr$datetime,wtr=rep(NA,length(wtr$datetime))) # temperature at do sensor
colnames(doTemp)[2]<-paste('wtr',do.z,sep='')
if(do.z%in%get.offsets(wtr)){
  doTemp<-wtr[,c(1,which(do.z==get.offsets(wtr))+1)]
}else{ # interpolate temp to do sensor depth if not in wtr file
  for(i in 1:length(wtr$datetime)){
    tryCatch(
      doTemp[i,2]<-approx(get.offsets(wtr),wtr[i,c(2:ncol(wtr))],xout = do.z)$y
  , error=function(e){doTemp[i,2]=wtr[i,2]})
  }
}

# linearly interpolate wind data to fill holes; max 60 min gap
wnd <- fillHoles(wnd,maxLength=60,timeStep=timeStep)

# calculate DO at saturation
do.sat<-o2.at.sat(doTemp,altitude = alt)

# calculate k.gas using vachon
U10<-wind.scale(wnd, wnd.z = wnd.z) # scaling measured wind height to wind at 10 meters
k600<-k.vachon(ts.data = U10,lake.area = lake.area) # estimating k600 with gas coefficient model
# k600<-k.cole(ts.data=U10) # alternative k model
colnames(k600)<-c('datetime','k600')
k.gas<-k600.2.kGAS(merge(k600,doTemp)) # converting k600 to k.O2

# calculating z.mix ; LOOK INTO NA'S => MAY BE BECAUSE Z.MIX IS AT BOTTOM OF LAKE
# -Inf means that all data is missing for given time point
z.mix<-ts.meta.depths(wtr,na.rm = T)
colnames(z.mix)<-c('datetime','z.mix','bottom')
# z.mix$z.mix<-ifelse(is.na(z.mix$z.mix),yes = 18,no = z.mix$z.mix) # forcing NA's to be to bottom of lake
# z.mix$z.mix<-ifelse(is.infinite(z.mix$z.mix),yes = ,no = z.mix$z.mix) # fix infinite values
# library(xts)
# doobs$datetime<-strftime(align.time(as.POSIXct(doobs$datetime,'%Y-%m-%d %H:%M:%S',tz=''),n=600),'%Y-%m-%d %H:%M:%S')
# doobs$datetime<-as.POSIXct(doobs$datetime)

# merge variables into one data frame
ts.data<-doobs
ts.data<-merge(ts.data,do.sat)
ts.data<-merge(ts.data,par)
ts.data<-merge(ts.data,k.gas)
ts.data<-merge(ts.data,doTemp)
ts.data<-merge(ts.data,z.mix)
# using a flux dummy (flux to atmosphere is 0 if z.mix is shallower than sensor (sensor placement about 0.5m ))
ts.data$k.gas<-ifelse(ts.data$z.mix<=do.z,0,ts.data$k.gas)
# standardize wtr depths for k.gas???
# Acton horizontal mixing - Mike used oxygen profiles to inform zmix
sun<-sun.rise.set(ts.data$datetime,lat=lat)
sun<-data.frame(matrix(sun,ncol=2))
colnames(sun)<-c('sunrise','sunset')
sun$sunrise<-as.POSIXct(sun$sunrise,origin='1970-01-01')
sun$sunset<-as.POSIXct(sun$sunset,origin='1970-01-01')
# account for daylight savings...
# sun$sunrise<-sun$sunrise+60*60
# sun$sunset<-sun$sunset+60*60
ts.data$sunrise<-sun$sunrise

# metabolism function requires do.obs, do.sat, irr, k.gas, z.mix, wtr (wtr at depth of DO probe )
# metab.out<-metab(ts.data,method = 'mle',wtr.name = colnames(ts.data[grep('wtr',colnames(ts.data))]),irr.name = 'par',do.obs.name = 'doobs')
error.type='PE' # observation error or process error specification for MLE (OE fits initial DO)
logged=T # whether or not to log /exponentiate parameter estimates for constraining positive /negative
bootstrap=F # whether or not to bootstrap the fits to produce distribution of fitted parameters (uncertainty in parameter estimate)
n.boot=1000 # how many iterations in bootstrapping if bootstrap = T
ar1.resids=T # maintain autocorrelation in residuals when bootstrapping if True
guesses=c(1E-2,1E-2) # MLE guesses for gppCoeff and rCoeff
# guesses=c(1E-1,1E-4,1E-4) # MLE guess for gppMaxCoeff, gppCoeff, and rCoeff for light saturating function
nDaysSim=1 #number of days over which to estimate metab coefficients
optim_method='Nelder-Mead'
sunrise=T # if True, fit model from sunrise to sunrise
# source('/Users/Jake/Dropbox/GLEON Catchment & Lake Metabolism/R Code/metab.support.R')
# source('/Users/Jake/Dropbox/GLEON Catchment & Lake Metabolism/R Code/metab.support.lightSaturating.R') # light saturating GPP functions
# dyn.load('/Users/Jake/Desktop/mleLoopLightSat.dll') # loading in compiled C code for looping

metab.out<-my.metab(data=ts.data,method = 'mle',wtr.name = colnames(ts.data[grep('wtr',colnames(ts.data))]),
                            irr.name = 'par',do.obs.name = 'doobs',error.type=error.type,logged=logged,
                    bootstrap=bootstrap,n.boot=n.boot,ar1.resids=ar1.resids,
                    guesses=guesses,nDaysSim=nDaysSim,optim_method=optim_method,sunrise=sunrise)
# metab.out<-metab(data=ts.data,method = 'bookkeep',wtr.name = colnames(ts.data[grep('wtr',colnames(ts.data))]),
#                     irr.name = 'par',do.obs.name = 'doobs',lake.lat=lat)

windows()
plot(metab.out$GPP)
windows()
plot(metab.out$R)

plot(ma.weighted(metab.out$GPP,metab.out$GPP_SD/metab.out$GPP,7),type='l')
plot(ma.weighted(metab.out$R,metab.out$R_SD/metab.out$R,7),type='l')
plot(ma.weighted(metab.out$GPP,metab.out$GPP_SD/metab.out$GPP,7)+ma.weighted(metab.out$R,metab.out$R_SD/metab.out$R,7)~metab.out$doy,type='l')


# predicted DO based on parameter estimates
pars<-attr(metab.out,'par') # parameters fit using MLE
wtr.name = colnames(ts.data[grep('wtr',colnames(ts.data))])
irr.name = 'par'
do.obs.name = 'doobs'
# #Rename the WTR column to be used (must be wtr to easily be passed to meta.* functions)
# if(wtr.name != "wtr"){
#   if(!"wtr"%in%names(ts.data)){
#     names(ts.data)[names(ts.data)==wtr.name] <- "wtr"
#   }else{
#     ts.data[,"wtr"] <- ts.data[,wtr.name]
#   }
# }
#
# if(irr.name != "irr"){
#   if(!"irr"%in%names(ts.data)){
#     names(ts.data)[names(ts.data)==irr.name] <- "irr"
#   }else{
#     ts.data[,"irr"] <- ts.data[,irr.name]
#   }
# }
#
# if(do.obs.name != "do.obs"){
#   if(!"do.obs"%in%names(ts.data)){
#     names(ts.data)[names(ts.data)==do.obs.name] <- "do.obs"
#   }else{
#     ts.data[,"do.obs"] <- ts.data[,do.obs.name]
#   }
# }

data1 <- addNAs(ts.data[complete.cases(ts.data),], percentReqd=1) # note that addNAs ALSO checks for POSIXct datetime, and adds year/doy
data2 <- data1[complete.cases(data1),]

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
data2$doHat<-rep(NA, length(data2$datetime))
days<-unique(metab.out$doy)
data2$doy<-trunc(data2$doy)

# plotting daily fits
pdf(file.path('results/metab/20161107/',lake,paste(lake,'dailyFits.pdf',sep='_')),
    width=11,height=8.5)
layout(rbind(matrix(c(1:14),nrow=2,byrow=F),matrix(c(15:28),nrow=2,byrow=F)),heights=c(1,1.5,1,1.5))
par(mar=c(1,2,0,0)+0.1)

for(i in 1:nid){
  cur<-data2[ids==i,]
  if(error.type=='OE'){
    curPar<-as.numeric(pars[pars$doy==cur$doy[1],c('gppCoeff','rCoeff','doInit')])
    if(logged==T){
      curPar<-c(log(curPar[1]),log(-curPar[2]),log(curPar[3]))
    }
  }
  if(error.type=='PE'){
    curPar<-as.numeric(pars[pars$doy==cur$doy[1],c('gppCoeff','rCoeff')])
    if(logged==T){
      curPar<-c(log(curPar[1]),log(-curPar[2]))
    }
  }

  freq<-calc.freq(cur$datetime)
#   curPar<-curPar/freq
  cur$k.gas<-cur$k.gas/freq
  predix<-predictDO(do.obs = cur$doobs,k.gas = cur$k.gas, do.sat = cur$do.sat,z.mix = cur$z.mix,irr = cur$par,
                       wtr=cur[,grep('wtr',colnames(cur))],par=curPar,logged=logged,error.type=error.type)
  cur$doHat<-predix$DOHat
  data2$doHat[ids==i]<-predix$DOHat

  #Limits for y-axis for drivers
  irrLims <- range(cur$par,na.rm=T)
  zMixLims <- c(max(max(cur$z.mix,na.rm=T),do.z,na.rm=T),0)
  atmFluxLims <- c(min(cur$k.gas,na.rm=T),max(cur$k.gas,na.rm=T))
  #Plot irradiance (orange points), zMix (dashed line), atmFlux (hollow black points)
  #y-axis tick labels are for atmFlux; positive values are flux into lake and negative values are flux out of lake
  par(mar=c(1,2,0,2)+0.1)
  plot(cur$par~cur$datetime, ylim=irrLims, axes=F, xlab="", ylab="", pch=18, col="dark orange")
  axis.POSIXct(1,cur$datetime,labels=F); box()
  text(x=min(cur$datetime),y=irrLims[2],labels=format(cur$datetime[1],format="%d-%b"),adj=c(0,1))
  par(new=T); plot(cur$z.mix~cur$datetime, ylim=zMixLims, type="l", lty=2, axes=F, xlab="", ylab="")
  axis(4)
  abline(do.z,0,lty=1,col='red',lwd=2)
  par(new=T); plot(cur$k.gas~cur$datetime, ylim=atmFluxLims, axes=F, xlab="", ylab=""); axis(2)

  #Plot observed and predicted DO
  yLims <- range(c(cur$doHat,cur$doobs),na.rm=T)
  par(mar=c(2,2,0,2)+0.1)
  plot(cur$doHat[!is.na(cur$doHat)] ~ cur$datetime[!is.na(cur$doHat)], xlim=range(cur$datetime),
       ylim=yLims, type="l", axes=F, xlab="", ylab="")
  axis.POSIXct(1,cur$datetime,format="%H:%M")
  axis(2)
  box()
  points(cur$doobs ~ cur$datetime)
  meanDOSat <- round(mean(cur$do.sat,na.rm=T),1)
  text(x=min(cur$datetime),y=yLims[2],labels=paste('DOSat',meanDOSat),adj=c(0,1))
}

dev.off()

pdf(file.path('results/metab/20161107/',lake,paste(lake,'metabPlots.pdf',sep='_')),
    width=12,height=14)
par(mfrow=c(2,1))
lower<-metab.out$GPP-metab.out$GPP_SD
upper<-metab.out$GPP+metab.out$GPP_SD
x<-c(metab.out$doy,rev(metab.out$doy))
y<-c(upper,rev(lower))
xy<-data.frame(x=x,y=y)
ylim=c(min(lower),max(upper))
plot(metab.out$GPP~metab.out$doy,type='l',ylab='GPP',xlab='DOY',ylim=ylim,
     main='black = estimate; gray = SD')
polygon(xy$x,xy$y,col='gray80',border='grey20',lwd=2)
lines(metab.out$GPP~metab.out$doy,lwd=2)
abline(0,0,lwd=2,lty=2,col='grey')

lower<-metab.out$R-metab.out$R_SD
upper<-metab.out$R+metab.out$R_SD
x<-c(metab.out$doy,rev(metab.out$doy))
y<-c(upper,rev(lower))
xy<-data.frame(x=x,y=y)
ylim=c(min(lower),max(upper))
plot(metab.out$R~metab.out$doy,type='l',ylab='R',xlab='DOY',ylim=ylim,
     main='black = estimate; gray = SD')
polygon(xy$x,xy$y,col='gray80',border='grey20',lwd=2)
lines(metab.out$R~metab.out$doy,lwd=2)
abline(0,0,lwd=2,lty=2,col='grey')

dev.off()

write.table(metab.out,
            file.path('results/metab/20161107/',lake,paste(lake,'metabEst.txt',sep='_')),
            row.names=F,sep='\t',quote=F)

write.table(pars,
            file.path('results/metab/20161107/',lake,paste(lake,'gpp_r_coeff.txt',sep='_')),
            row.names=F,sep='\t',quote=F)

# }

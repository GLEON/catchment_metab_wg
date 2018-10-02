# plotting GPP vs. R and color coding by season; JAZ & JK; 2018-03-09

library(dplyr)

dir<-'results/metab/20161107/' # directory of metabolism data
folders<-list.files(dir) # folders in this dir
folders<-folders[-grep('.doc',folders)] # get rid of README doc
folders<-folders[-grep('Trout',folders)] # skipping trout for now; have to do bootstrapping on this still

all_metab<-data.frame() # data frame to store all metab data
for(i in 1:length(folders)){ # loops over all folders in metab directory
  cur<-read.table(file.path(dir,folders[i],paste(folders[i],'_metabEst.txt',sep='')),header=T,sep='\t',
                  stringsAsFactors = F) # read in lake specific metab data
  cur<-cur[,1:12] # getting rid of any unnecessary columns
  cur$lake<-folders[i]
  all_metab<-rbind(all_metab,cur)
}

all_metab$date<-as.Date(paste(all_metab$year,all_metab$doy),format='%Y %j') # making date
all_metab <- as_tibble(all_metab)

season_cutoff <- readRDS('results/z_scored_schmidt.rds') # seasonal cutoff based on z-scored schmidt stability
all_metab <- left_join(all_metab, season_cutoff, by = c('lake' = 'lake', 'date' = 'date'))

# season cutoffs used by Jim ; can change if needed
# Spring = (doy < 180)
# Summer = (doy >= 180 & doy <=240)
# Fall = (doy > 240)
# all_metab$season <- rep(NA,nrow(all_metab))
# all_metab$season <- ifelse(all_metab$doy>120&all_metab$doy<180,'spring',all_metab$season) # if doy with criteria, spring, else leave alone
# all_metab$season <- ifelse(all_metab$doy>=180&all_metab$doy<=240,'summer',all_metab$season)
# all_metab$season <- ifelse(all_metab$doy>240&all_metab$doy<305,'fall',all_metab$season)



# plotting GPP vs. R by season
windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
col = c('orange','green','blue') # fall , spring, summer colors ; arranges them alphabetically
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
  ylim=c(min(cur$R,na.rm = T),max(cur$R,na.rm = T))
  xlim=c(min(cur$GPP,na.rm = T),max(cur$GPP,na.rm = T))

  plot(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',main=unique(all_metab$lake)[i],col=col[as.factor(cur$season)])
  abline(0,-1,lty=2,lwd=2)
}


windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
xlim=c(0,100)
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[cur$GPP_SD/cur$GPP<=xlim[2],]

  hist(cur$GPP_SD/cur$GPP,xlab='GPP CV',xlim=xlim,main=paste(unique(all_metab$lake)[i],round(median(cur$GPP_SD/cur$GPP),digits = 2)))
  abline(v=median(cur$GPP_SD/cur$GPP),lwd=2,lty=2)
}

windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
xlim=c(0,10)
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur$R = cur$R*-1
  cur <- cur[cur$R_SD/cur$R<=xlim[2],]

  hist(cur$R_SD/cur$R,xlab='R CV',xlim=xlim,main=paste(unique(all_metab$lake)[i],round(median(cur$R_SD/cur$R),digits = 2)))
  abline(v=median(cur$R_SD/cur$R),lwd=2,lty=2)
}

# plotting GPP vs. R by season with different CV cutoffs
cv_thres = 4
windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
col = c('orange','green','blue') # fall , spring, summer colors ; arranges them alphabetically
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]
  ylim=c(min(cur$R,na.rm = T),max(cur$R,na.rm = T))
  xlim=c(min(cur$GPP,na.rm = T),max(cur$GPP,na.rm = T))

  plot(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',main=unique(all_metab$lake)[i],col=col[as.factor(cur$season)])
  abline(0,-1,lty=2,lwd=2)
}


# plotting GPP vs. R by lake with different lakes as colors; using CV cutoff for easier visualization and patterns
cv_thres = 4
windows()
col = rainbow(n=length(unique(all_metab$lake)))
ylim=c(min(all_metab$R,na.rm = T),max(all_metab$R,na.rm = T))
xlim=c(min(all_metab$GPP,na.rm = T),max(all_metab$GPP,na.rm = T))
cex=2
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]
  if(i==1){
    plot(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',col=col[i],cex=cex)
    abline(0,-1,lty=2,lwd=2)
  }else{
    points(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='',xlab='',col=col[i],cex=cex)
  }
}

# plotting one dot per lake
cv_thres = 4
windows()
col = rainbow(n=length(unique(all_metab$lake)))
gpp=aggregate(all_metab$GPP[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&!is.na(all_metab$season)], by=list(all_metab$lake[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&!is.na(all_metab$season)]),FUN = mean)
r=aggregate(all_metab$R[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&!is.na(all_metab$season)]~all_metab$lake[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&!is.na(all_metab$season)],FUN = mean)
ylim=c(min(c(r[,2],-gpp[,2]),na.rm = T),0)
xlim=c(0,max(c(gpp[,2],-r[,2]),na.rm = T))
cex=2
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]
  cur <- data.frame(t(sapply(cur,FUN = mean)))
  if(i==1){
    plot(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',col=col[i],cex=cex)
    abline(0,-1,lty=2,lwd=2)
    text(x=cur$GPP,y=cur$R,labels = unique(all_metab$lake)[i],col = col[i],pos = 4)
  }else{
    points(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='',xlab='',col=col[i],cex=cex)
    text(x=cur$GPP,y=cur$R,labels = unique(all_metab$lake)[i],col = col[i],pos = 4)
  }
}

# plotting one dot per lake for specific season
cv_thres = 4
season = c('spring','summer','fall')
windows()
col = rainbow(n=length(unique(all_metab$lake)))
gpp=aggregate(all_metab$GPP[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&all_metab$season==season&!is.na(all_metab$season)], by=list(all_metab$lake[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&all_metab$season==season&!is.na(all_metab$season)]),FUN = mean)
r=aggregate(all_metab$R[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&all_metab$season==season&!is.na(all_metab$season)]~all_metab$lake[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&all_metab$season==season&!is.na(all_metab$season)],FUN = mean)
ylim=c(min(c(r[,2],-gpp[,2]),na.rm = T),0)
xlim=c(0,max(c(gpp[,2],-r[,2]),na.rm = T))
cex=2
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[cur$season==season&!is.na(cur$season),] # only keeping dates that fall within our season of interest
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]
  cur <- data.frame(t(sapply(cur,FUN = mean)))
  if(i==1){
    plot(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',col=col[i],cex=cex,main=season)
    abline(0,-1,lty=2,lwd=2)
    text(x=cur$GPP,y=cur$R,labels = unique(all_metab$lake)[i],col = col[i],pos = 4)
  }else{
    points(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='',xlab='',col=col[i],cex=cex)
    text(x=cur$GPP,y=cur$R,labels = unique(all_metab$lake)[i],col = col[i],pos = 4)
  }
}


# plotting one dot per lake for specific season
cv_thres = 4
season = c('spring','summer','fall')
windows()
par(mfrow=c(2,2))
col = rainbow(n=length(unique(all_metab$lake)))
gpp=aggregate(all_metab$GPP[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&!is.na(all_metab$season)], by=list(all_metab$lake[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&!is.na(all_metab$season)]),FUN = mean)
r=aggregate(all_metab$R[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&!is.na(all_metab$season)]~all_metab$lake[all_metab$GPP_SD/all_metab$GPP<cv_thres&all_metab$R_SD/all_metab$R<cv_thres&!is.na(all_metab$season)],FUN = mean)
ylim=c(min(c(r[,2],-gpp[,2]),na.rm = T),0)
xlim=c(0,max(c(gpp[,2],-r[,2]),na.rm = T))
ylim=c(-2.5,0)
xlim=c(0,2.5)
cex=2
for(j in 1:length(season)){
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[cur$season==season[j]&!is.na(cur$season),] # only keeping dates that fall within our season of interest
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]
  cur <- data.frame(t(sapply(cur,FUN = mean)))
  if(i==1){
    plot(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',col=col[i],cex=cex,main=season[j])
    abline(0,-1,lty=2,lwd=2)
    text(x=cur$GPP,y=cur$R,labels = unique(all_metab$lake)[i],col = col[i],pos = 4)
  }else{
    points(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='',xlab='',col=col[i],cex=cex)
    text(x=cur$GPP,y=cur$R,labels = unique(all_metab$lake)[i],col = col[i],pos = 4)
  }
}
}



# plotting GPP vs. R colored by doy
windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  # windows()
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  col <- rev(grey.colors(n = nrow(cur),start = 0,end = 1)) # colors based on DOY
  ylim=c(min(cur$R,na.rm = T),max(cur$R,na.rm = T))
  xlim=c(min(cur$GPP,na.rm = T),max(cur$GPP,na.rm = T))

  plot(cur$R~cur$GPP,pch=21,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',main=unique(all_metab$lake)[i],col='black',bg=col[as.factor(cur$doy)]) # darker colors are later in year
  abline(0,-1,lty=2,lwd=2)
}

# plotting GPP vs. R colored by doy with CV threshold
cv_thres = 4
windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  # windows()
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]
  col <- rev(grey.colors(n = nrow(cur),start = 0,end = 1)) # colors based on DOY
  ylim=c(min(cur$R,na.rm = T),max(cur$R,na.rm = T))
  xlim=c(min(cur$GPP,na.rm = T),max(cur$GPP,na.rm = T))

  plot(cur$R~cur$GPP,pch=21,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',main=unique(all_metab$lake)[i],col='black',bg=col[as.factor(cur$doy)]) # darker colors are later in year
  abline(0,-1,lty=2,lwd=2)
}

# hi=c(1,3,54,12,333,354,433,335,3,23,322,330,334,332,408,409,405,401,402)
# hi=seq(1,20)
# hi=c(1,2,4,5,200)
# test = rev(grey.colors(n= length(hi)/4, start=0, end=1 ))
# test = rev(grey.colors(n= max(hi), start=0, end=1 ))
# test = rev(grey.colors(n = length(seq(0,max(hi),by = 5)),start = 0, end = 1))
#
# plot(hi,pch=21,bg=test[as.factor(hi)])
#
# plot(hi,pch=21, bg=plotrix::color.scale(hi,c(0,0,0),c(1,1,1),0))
#


#################### Light Climate

windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
xlim=c(0,10)
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur$R = cur$R*-1
  cur <- cur[cur$R_SD/cur$R<=xlim[2],]

  hist(cur$R_SD/cur$R,xlab='R CV',xlim=xlim,main=paste(unique(all_metab$lake)[i],round(median(cur$R_SD/cur$R),digits = 2)))
  abline(v=median(cur$R_SD/cur$R),lwd=2,lty=2)
}

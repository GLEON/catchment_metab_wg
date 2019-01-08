# plotting GPP vs. R and color coding by season; JAZ & JK; 2018-03-09

#first part is creating dataframes with data we want to plot.
#starts with reading in individual lake files and binding them into one data frame
#then merges dataframes together (e.g., metabolism dataframe with loading data frame)
#there is code in the dropbox for creating a dataframe for the light climate data but I've left this
#out for now because the light climate data are not on Github
#code for plotting starts~line 100


library(xlsx)
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
  cur$lake<-folders[i] #creating a column of lake names from the names of the folders
  all_metab<-rbind(all_metab,cur) #stacking all the lakes in one dataframe one by one
}

all_metab$date<-as.Date(paste(all_metab$year,all_metab$doy),format='%Y %j') # making date

season_cutoff <- readRDS('results/z_scored_schmidt.rds') %>% select(-doy)# seasonal cutoff based on z-scored schmidt stability
all_metab <- left_join(all_metab, season_cutoff, by = c('lake' = 'lake', 'date' = 'date'))

# creating season column. season cutoffs used by Jim ; can change if needed
# Spring = (doy < 180)
# Summer = (doy >= 180 & doy <=240)
# Fall = (doy > 240)
#all_metab$season <- rep(NA,nrow(all_metab))
#all_metab$season <- ifelse(all_metab$doy>120&all_metab$doy<180,'spring',all_metab$season) # if doy with criteria, spring, else leave alone
#all_metab$season <- ifelse(all_metab$doy>=180&all_metab$doy<=240,'summer',all_metab$season)
#all_metab$season <- ifelse(all_metab$doy>240&all_metab$doy<305,'fall',all_metab$season)

#loop below creates one dataframe with all loading data in it - JK modifying the loop JZ created above for metabolism
dir<-'results/nutrient load/' # directory of loading data
folders<-list.files(dir) # folders in this dir (in this case there are only files no folders)
folders<-folders[-grep('Readme',folders)] # get rid of README doc
folders<-folders[-grep('Trout',folders)] # skipping trout for now; have to do bootstrapping on this still

all_load<-data.frame() # data frame to store all loading data + discharge
for(i in 1:length(folders)){ # loops over all folders in loads directory, needed to change syntax on file path because there are no lake folders in main load folder
  cur<-read.table(file.path(dir,folders[i]),header=T,sep='\t',
                  stringsAsFactors = F) # read in lake specific load data
  cur$lake<-strsplit(folders[i],split = '_')[[1]][1]  # this line creates the lake column,splits the string (the type of variable that is files[i]) by a character that you choose, then chooses the 1st split
  all_load<-rbind(all_load,cur) #stacking all the lakes in one dataframe
}
#creating logged inflow colum
all_load$loginflow<- log(all_load$inflow)

#merging loads and metabolism data into one dataframe
#does not include lakes without inflows or Mendota (no load yet as of 10/28/18)

all_metabload<-merge(all_metab, all_load, by=c('lake','doy')) #does not include lakes without inflows or Mendota (no load)
all_metabload$DOC_TP<-all_metabload$DOC_load/all_metabload$TP_load #creating DOC:TP column
all_metabload$TN_TP<-all_metabload$TN_load/all_metabload$TP_load #creating TP:TN column


#loop below creates one dataframe with all schmidt stability data in it - JK modifying JZ allmetab loop
dir<-'data/schmidt stability/' # directory of stability data
files<-list.files(dir) # files in this dir

all_st<-data.frame() # data frame to store all stability data, this data are not summarized by day
for(i in 1:length(files)){ # loops over all files in stability directory
  cur<-read.table(file.path(dir,files[i],sep=''),header=T,sep='\t',
                  stringsAsFactors = F) # read in lake specific buoyancy data
  cur$lake<-strsplit(files[i],split = '_')[[1]][1]  # this line creates the lake column,splits the string (the type of variable that is files[i]) by a character that you choose, then chooses the 1st split
  all_st<-rbind(all_st,cur)
}

#adding doy column to all_st
all_st$doy <- strftime(all_st$datetime, format = "%j")
#calculating average daily stability using doy and lake as by variable
ave_st<-aggregate(all_st, by=list(all_st$doy,all_st$lake),
                     FUN=mean, na.rm=TRUE)

#dropping variables that couldn't calculate mean (doy, lake) and renaming group.1 and group.2

ave_st<-ave_st[c(-3,-5,-6)]
names(ave_st)[1] <- "doy"
names(ave_st)[2]<-"lake"

#merging stability and metabolism data into one dataframe

all_metabst<-merge(all_metab, ave_st, by=c('lake', 'doy'))

# plotting GPP vs. R by season
cv_thres = 10  #cutoff for keeping metabolism data
windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)

par(mar=c(3,2,2,2), oma=c(3,3,1,1))
all_metab = all_metab %>%
  mutate(color = case_when(season == 'spring' ~ 'green',
                           season == 'summer' ~ 'blue',
                           season == 'fall' ~ 'orange')) # creates a new column in the all_metab data frame for season color

for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]#keeping only days that meet CV threshold
  xlim=c(0,max(abs(c(cur$R,cur$GPP)),na.rm = T))
  ylim=c(-1*xlim[2],xlim[1])

  plot(cur$R~cur$GPP,pch=16,ylim=ylim,xlim=xlim,ylab='',xlab='',main=unique(all_metab$lake)[i],col=cur$color)
  abline(0,-1,lty=2,lwd=2)
}
mtext(expression(R~(mg~O[2]~L^-1~day^-1)), side=2, outer=TRUE)
mtext(expression(GPP~(mg~O[2]~L^-1~day^-1)), side=1, outer=TRUE)

# plotting GPP vs. R colored by doy in grey scale
cv_thres = 10  #cutoff for keeping metabolism data
windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(all_metab$lake))){ # looping through each lake
  cur<-all_metab[all_metab$lake==unique(all_metab$lake)[i],]
  cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]#keeping only days that meet CV threshold
  col <- rev(grey.colors(n = nrow(cur))) # colors based on DOY
  ylim=c(min(cur$R,na.rm = T),max(cur$R,na.rm = T))
  xlim=c(min(cur$GPP,na.rm = T),max(cur$GPP,na.rm = T))

  plot(cur$R~cur$GPP,pch=21,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',main=unique(all_metab$lake)[i],col='black',bg=col[as.factor(cur$doy)]) # darker colors are later in year
  abline(0,-1,lty=2,lwd=2)
}

# plotting GPP vs. R colored by 3rd variable (using either all_metabload or all_metabst)
#current code codes by TP_load

cv_thres = 4  #cutoff for keeping metabolism data
windows() #don't use when plotting each lake separately
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(all_metabload$lake))){ # looping through each lake
  #windows() #use if want each lake in separate plot
  cur<-all_metabload[all_metabload$lake==unique(all_metabload$lake)[i],]
  cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]#keeping only days that meet CV threshold
  col <- rev(grey.colors(n = nrow(cur), start=0, end=1)) #number of colors based on number of rows
  ylim=c(min(cur$R,na.rm = T),max(cur$R,na.rm = T))
  xlim=c(min(cur$GPP,na.rm = T),max(cur$GPP,na.rm = T))
  #cur <- subset(cur, cur$doy>=180&cur$doy<=240) #using this line plots only summer data
  #cur <- subset(cur, cur$doy>=121&cur$doy<=179) #using this line plots only spring data
  #cur <- subset(cur, cur$doy>=241&cur$doy<=304) #using this line plots only fall data


  plot(cur$R~cur$GPP,pch=21,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',main=unique(all_metabload$lake)[i],col='black',bg=col[as.factor(cur$TP_load)]) # darker colors are higher values
  abline(0,-1,lty=2,lwd=2)
}

#looking across lakes at effect of third variable
#this code plots all lakes on the same graph coded by a third variable in greyscale
windows()
cur<-all_metabload
cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
col <- rev(grey.colors(n = nrow(cur), start=0, end=1)) #number of colors based on number of rows
ylim=c(min(cur$R,na.rm = T),max(cur$R,na.rm = T))
xlim=c(min(cur$GPP,na.rm = T),max(cur$GPP,na.rm = T))
#cur <- subset(cur, cur$doy>=180&cur$doy<=240) #using this line plots only summer data
#cur <- subset(cur, cur$doy>=121&cur$doy<=179) #using this line plots only spring data
#cur <- subset(cur, cur$doy>=241&cur$doy<=304) #using this line plots only fall data


plot(cur$R~cur$GPP,pch=21,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',col='black',bg=col[as.factor(cur$DOC_TP)]) # darker colors are higher values
abline(0,-1,lty=2,lwd=2)

#testing use of quartiles for categories of driver variables
#should we calculate quartiles before or after dropping poorly fitting days? I think before

# plotting GPP vs. R colored by 3rd variable (using either all_metabload or all_metabst)
cv_thres = 4  #cutoff for keeping metabolism data
windows() #don't use when plotting each lake separately
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
col = c('orange','green','blue', 'black')
for(i in 1:length(unique(all_metabst$lake))){ # looping through each lake
  #windows() #use if want each lake in separate plot
  cur<-all_metabst[all_metabst$lake==unique(all_metabst$lake)[i],]
  cur <- cur[!is.na(cur$season),] # only keeping dates that fall within our pre-defined seasons
  cur<- cur[!is.na(cur$schmidt.stability),]#keeps only days with  data
  #cur <- subset(cur, cur$doy>=180&cur$doy<=240) #using this line uses only summer data
  cur$cat <- rep(NA,nrow(cur)) #creating a new column to populate with 1,2,3,4 quartile
  y= quantile(cur$schmidt.stability, c(.25)) #finds the cutoff value for 25th percentile
  y1=quantile(cur$schmidt.stability, c(.5))  #finds the cutoff value for 50th percentile
  y2=quantile(cur$schmidt.stability, c(0.75)) #finds the cutoff values for 75th percentile
  cur$cat<- ifelse(cur$schmidt.stability<=y,'1',cur$cat) #next 4 lines code the quartile based on value of each day
  cur$cat<- ifelse(cur$schmidt.stability>y & cur$schmidt.stability<=y1,'2',cur$cat)
  cur$cat<- ifelse(cur$schmidt.stability>y1 & cur$schmidt.stability<=y2,'3',cur$cat)
  cur$cat<- ifelse(cur$schmidt.stability>y2,'4',cur$cat)
  cur <- cur[cur$GPP_SD/cur$GPP<cv_thres&cur$R_SD/cur$R<cv_thres,]#keeping only days that meet CV threshold
  ylim=c(min(cur$R,na.rm = T),max(cur$R,na.rm = T))
  xlim=c(min(cur$GPP,na.rm = T),max(cur$GPP,na.rm = T))
  #cur <- subset(cur, cur$doy>=180&cur$doy<=240) #using this line plots only summer data, need to think about whether you've calculated quartiles within or aross seasons if plotting a subset
  #cur <- subset(cur, cur$doy>=121&cur$doy<=179) #using this line plots only spring data
  #cur <- subset(cur, cur$doy>=241&cur$doy<=304) #using this line plots only fall data
  plot(cur$R~cur$GPP,pch=16, cex=.9,ylim=ylim,xlim=xlim,ylab='R',xlab='GPP',main=unique(all_metabst$lake)[i],col=col[as.factor(cur$cat)])

  abline(0,-1,lty=2,lwd=2)
}

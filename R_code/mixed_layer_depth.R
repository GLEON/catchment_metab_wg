# output for mixed layer depth

library(dplyr)

i=17
toRm=ls()
toRm=toRm[-which(toRm=='i')]
rm(list=toRm)
library(LakeMetabolizer)
source('R_code/metabolism_code/fillHoles.R')
source('R_code/metabolism_code/floorMins.R')
# library(xts) # useful for timeseries visulization
Sys.setenv(tz='GMT')

dir<-'data/metab_data/' # directory where data is stored
lakes<-list.files(dir)
lake<-lakes[i] # current lake for metabolism estimates
metaData<-read.csv('data/metadataLookUp.csv',stringsAsFactor=F)

# load time series metabolism data
wtr<-load.ts(file.path(dir,lake,paste(lake,'_wtr.txt',sep='')))

timeStep<-as.numeric(diff(wtr$datetime)[1]) # time step difference for doobs

if(length(grep('_',colnames(wtr[,2:ncol(wtr)])))<(ncol(wtr)-1)){
  colnames(wtr)[2:ncol(wtr)]<-gsub('wtr','wtr_',colnames(wtr)[2:ncol(wtr)])
}

# can't have 0 meter temp depth; change to 0.1m if so
if(0%in%get.offsets(wtr)){
  colnames(wtr)[which(0==get.offsets(wtr))+1]<-'wtr_0.1'
}

wtr <- wtr %>%
  arrange(datetime)

if(lake=='Trout'|lake=='Feeagh'){
  timeStep=10 # forcing Trout and Feeagh data to 10 min timestep
  if(lake=='Trout'){
    wtr<-wtr[,!colnames(wtr)=='wtr_0.1'] #Trout shallow temp pendent is consistently lower than 1 meter pendant so it was removed
  }
}
if(lake=='Mendota'){
  timeStep=10 # Mendota par and wtr are 60 mins; linearly interpolating below
}

wtr$datetime <- floorMins(wtr)

notDupRows <- findNotDupRows("wtr")
wtr <- wtr[notDupRows,]

z.mix<-ts.meta.depths(wtr,na.rm = T)
colnames(z.mix)<-c('datetime','z.mix','meta.bottom')

z.mix_out <- z.mix %>%
  mutate(date = as.Date(datetime)) %>%
  dplyr::filter(!is.infinite(z.mix), !is.na(z.mix)) %>%
  group_by(date) %>%
  summarise(z_mix_min = min(z.mix, na.rm = T),
            z_mix_max = max(z.mix, na.rm = T),
            z_mix_median = median(z.mix, na.rm =T),
            z_mix_mean = mean(z.mix, na.rm = T)) %>%
  ungroup()

z.mix_out$z_mix_mean
plot(z.mix_out$z_mix_mean, main = lake)

write.table(z.mix_out, file = file.path('data/mixed layer depth/',paste(lake, 'z_mix.txt',sep ='_')), row.names = F, quote = F, sep = '\t')



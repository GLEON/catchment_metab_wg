# plotting GPP vs. R and color coding by season; JAZ & JK; 2018-03-09

library(dplyr)
library(ggplot2)

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

cv_cutoff = 10

metab_plot <- dplyr::filter(all_metab, doy.x > 140, doy.x < 300, GPP_SD/GPP < cv_cutoff) %>%
  group_by(lake) %>%
  mutate(mean_gpp = mean(GPP, na.rm=T)) %>%
  ungroup() %>%
  arrange(mean_gpp)

metab <- ggplot(metab_plot, aes(x = doy.x, y = GPP, group = lake)) +
  geom_line() +
  theme_classic() +
  facet_wrap(~mean_gpp, scales = 'free_x') +
  geom_line(data = metab_plot, aes( x= doy.x, y = R, group = lake)) +
  geom_line(data = metab_plot, aes( x= doy.x, y = NEP, group = lake))


metab


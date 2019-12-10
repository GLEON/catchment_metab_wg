# plotting metabolism timeseries for figure 1

library(dplyr)
library(tidyr)
library(ggplot2)
library(yaml)

analysis_cfg <- yaml::yaml.load_file('lib/cfg/analysis_cfg.yml') # this file holds important analysis info such as CV cutoff
dir<-'results/metab/20161107/' # directory of metabolism data
folders<-list.files(dir) # folders in this dir
folders<-folders[-grep('.doc',folders)] # get rid of README doc
# folders<-folders[-grep('Trout',folders)] # skipping trout for now; have to do bootstrapping on this still

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

season_cutoff <- readRDS('results/z_scored_schmidt.rds') %>%
  select(-doy)# seasonal cutoff based on z-scored schmidt stability
all_metab <- left_join(all_metab, season_cutoff, by = c('lake' = 'lake', 'date' = 'date'))

cv_cutoff = analysis_cfg$cv_cutoff
min_doy = analysis_cfg$min_doy
max_doy = analysis_cfg$max_doy

metab_plot <- dplyr::filter(all_metab, doy > min_doy, doy < max_doy, GPP_SD/GPP < cv_cutoff, R_SD/abs(R) < cv_cutoff, GPP > 0, R < 0) %>%
  dplyr::group_by(lake) %>%
  dplyr::mutate(mean_gpp = mean(GPP, na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(lake = factor(lake),
                season = factor(season),
                plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'))

#ordering by mean GPP
lakes_sorted <- metab_plot$lake[sort.list(metab_plot$mean_gpp)]
lakes_sorted <- as.character(lakes_sorted[!duplicated(lakes_sorted)])
seasons_sorted <- c('spring','summer','fall')

metab_plot$lake <- factor(metab_plot$lake,levels = lakes_sorted)
metab_plot$season <- factor(metab_plot$season, levels = seasons_sorted)

# facet labeller
lake_names <- c('Acton' = 'Acton Lake',
                'Crampton' = 'Crampton Lake',
                'EastLong' = 'East Long Lake',
                'Feeagh' = 'Lough Feeagh',
                'Harp' = 'Harp Lake',
                'Langtjern' = 'Lake Langtjern',
                'Lillinonah' = 'Lake Lillinonah',
                'Lillsjoliden' = 'Lillsjöliden',
                'Mangstrettjarn' = 'Mångstrettjärn',
                'Mendota' = 'Lake Mendota',
                'Morris' = 'Morris Lake',
                'Nastjarn' = 'Nästjärn',
                'Ovre' = 'Övre Björntjärn',
                'Struptjarn' = 'Struptjärn',
                'Trout' = 'Trout Lake',
                'Vortsjarv' = 'Lake Võrtsjärv'
                )

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colorblind-friendly pallete

# keeping x and y axis scales the same for every plot
linesize = .5
pointsize = .8
metab <- ggplot(dplyr::filter(metab_plot, !is.na(season)), aes(x = plot_date, y = GPP, group = lake ,color = season)) +
  geom_line(size = linesize) +
  geom_point(size = pointsize) +
  geom_line(data = metab_plot, aes( x= plot_date, y = R, group = lake), size = linesize) +
  geom_point(data = metab_plot, aes( x= plot_date, y = R, group = lake), size = pointsize) +
  facet_wrap(~lake,labeller = as_labeller(lake_names), strip.position = 'top') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  scale_color_manual(name = 'season',
                     values = c('spring' = '#009E73',
                                'summer' = '#56B4E9',
                                'fall' = '#E69F00'),
                     labels = c('Spring', 'Summer', 'Fall')) +
  scale_fill_manual(name = 'season',
                     values = c('spring' = '#009E73',
                                'summer' = '#56B4E9',
                                'fall' = '#E69F00'),
                     labels = c('Spring', 'Summer', 'Fall')) +
  ylab(expression(Metabolism~(mg~O[2]~L^-1~day^-1))) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey')

# metab

ggsave('figures/fig_metab_timeseries.png', plot = metab, width = 10, height = 10)


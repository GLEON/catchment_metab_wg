# plotting load timeseries for figure 2

library(dplyr)
library(ggplot2)

### loading in metabolism data for sorting by mean GPP ###
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

season_cutoff <- readRDS('results/z_scored_schmidt.rds') %>%
  select(-doy)# seasonal cutoff based on z-scored schmidt stability
all_metab <- left_join(all_metab, season_cutoff, by = c('lake' = 'lake', 'date' = 'date'))

cv_cutoff = 10
min_doy = 120
max_doy = 300

metab_plot <- dplyr::filter(all_metab, doy > min_doy, doy < max_doy, GPP_SD/GPP < cv_cutoff) %>%
  group_by(lake) %>%
  mutate(mean_gpp = mean(GPP, na.rm=T)) %>%
  ungroup()

dir<-'results/nutrient load/' # directory of load data
files<-list.files(dir) # folders in this dir
files<-files[-grep('Readme',files)] # get rid of README doc
files<-files[-grep('Trout', files)]

all_load<-data.frame() # data frame to store all load data
for(i in 1:length(files)){ # loops over all files in load directory
  cur<-read.table(file.path(dir,files[i]),header=T,sep='\t',
                  stringsAsFactors = F) # read in lake specific load data
  cur$lake<-strsplit(files[i], split = '_loads.txt')[[1]][1]
  all_load<-rbind(all_load,cur)
}

all_load <- as_tibble(all_load) %>%
  mutate(date = as.Date(Date)) %>%
  select(-Date)

season_cutoff <- readRDS('results/z_scored_schmidt.rds') %>%
  select(-doy)# seasonal cutoff based on z-scored schmidt stability
all_load <- left_join(all_load, season_cutoff, by = c('lake' = 'lake', 'date' = 'date'))

metaData <- read.csv('data/metadataLookUp.csv',stringsAsFactor=F) %>%
  select(Lake.Name, Volume..m3., Surface.Area..m2., Catchment.Area..km2., Lake.Residence.Time..year.)
all_load <- left_join(all_load, metaData, by = c('lake' = 'Lake.Name'))

all_load <- left_join(all_load, dplyr::select(metab_plot, lake, date, mean_gpp), by = c('lake'='lake','date'='date'))

cv_cutoff = 10
min_doy = 120
max_doy = 300

load_plot <- dplyr::filter(all_load, doy > min_doy, doy < max_doy) %>%
  group_by(lake) %>%
  mutate(mean_tp = mean(TP_load / Volume..m3., na.rm=T)) %>%
  ungroup() %>%
  mutate(lake = factor(lake),
         season = factor(season),
         plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'),
         TP_load = ifelse(TP_load == 0, NA, TP_load),
         TP_load = TP_load / 31,
         TN_load = TN_load / 14,
         DOC_load = DOC_load / 12) # changing to mol for stoichiometry plot

#ordering by mean inflow
lakes_sorted <- load_plot$lake[sort.list(load_plot$mean_gpp)]
lakes_sorted <- as.character(lakes_sorted[!duplicated(lakes_sorted)])
seasons_sorted <- c('spring','summer','fall')

load_plot$lake <- factor(load_plot$lake,levels = lakes_sorted)
load_plot$season <- factor(load_plot$season, levels = seasons_sorted)

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
stoich <- ggplot(load_plot, aes(x = plot_date, y = DOC_load/TP_load, group = lake)) +
  geom_line(aes(color = 'a'), size = 1) +
  geom_line(data = load_plot, aes(x = plot_date, y = TN_load / TP_load, group = lake, color = 'b'), size = 1)+
  geom_line(data = load_plot, aes(x = plot_date, y = DOC_load / TN_load, group = lake, color = 'c'), size = 1)+
  facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  scale_color_manual(name = '',
                     values = c('a' = 'black',
                                'b' = '#CC79A7',
                                'c' = '#D55E00'),
                     labels = c('C:P', 'N:P', 'C:N')) +
  ylab(expression(Load~Stoichiometry~(mol:mol))) +
  scale_y_log10() +
  geom_hline(yintercept = 106, color = 'black', linetype = 'dashed')+ # redfield ratios
  geom_hline(yintercept = 16, color ='#CC79A7', linetype = 'dashed')+
  geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed')
  # geom_hline(yintercept = 374.6, color = 'black', linetype = 'dashed')+ # maranger et al 2018 average stoich of watershed inputs for comparison
  # geom_hline(yintercept = 24.1, color ='#CC79A7', linetype = 'dashed')+
  # geom_hline(yintercept = 15.5, color = '#D55E00', linetype = 'dashed')


stoich

ggsave('figures/fig_stoich_timeseries.png', plot = stoich, width = 10, height = 10)


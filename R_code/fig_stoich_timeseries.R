# plotting load timeseries for figure 2

library(dplyr)
library(ggplot2)
library(yaml)

analysis_cfg <- yaml::yaml.load_file('lib/cfg/analysis_cfg.yml') # this file holds important analysis info such as CV cutoff
### loading in metabolism data for sorting by mean GPP ###
dir<-'results/metab/20161107/' # directory of metabolism data
folders<-list.files(dir) # folders in this dir
folders<-folders[-grep('.doc',folders)] # get rid of README doc

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
  group_by(lake) %>%
  dplyr::mutate(mean_gpp = mean(GPP, na.rm=T)) %>%
  ungroup()

dir<-'results/nutrient load/' # directory of load data
files<-list.files(dir) # folders in this dir
files<-files[-grep('README',files)] # get rid of README doc

all_load<-data.frame() # data frame to store all load data
for(i in 1:length(files)){ # loops over all files in load directory
  cur<-read.table(file.path(dir,files[i]),header=T,sep='\t',
                  stringsAsFactors = F) # read in lake specific load data
  cur$lake<-strsplit(files[i], split = '_loads.txt')[[1]][1]
  all_load<-rbind(all_load,cur)
}

all_load <- as_tibble(all_load) %>%
  dplyr::mutate(date = as.Date(Date)) %>%
  select(-Date)

# adding in lakes w/o streams to plot
no_streams <- all_metab[!all_metab$lake%in%all_load$lake, ] %>%
  select(year, doy, lake, date) %>%
  rename(Year = year) %>%
  dplyr::mutate(TN_load = NA, TP_load = NA, DOC_load = NA, inflow = NA) %>%
  select(Year, doy, TN_load, TP_load, DOC_load, inflow, lake, date)

all_load <- bind_rows(all_load, no_streams)

season_cutoff <- readRDS('results/z_scored_schmidt.rds') %>%
  select(-doy)# seasonal cutoff based on z-scored schmidt stability
all_load <- left_join(all_load, season_cutoff, by = c('lake' = 'lake', 'date' = 'date'))

metaData <- read.csv('data/metadataLookUp.csv',stringsAsFactor=F) %>%
  select(Lake.Name, Volume..m3., Surface.Area..m2., Catchment.Area..km2., Lake.Residence.Time..year.)
all_load <- left_join(all_load, metaData, by = c('lake' = 'Lake.Name'))

all_load <- left_join(all_load, dplyr::select(metab_plot, lake, date, mean_gpp), by = c('lake'='lake','date'='date'))

in_lake_nutrients = read.table('data/in_lake_nutrients/GLEON_nutrient_inlake.txt', stringsAsFactors = F, header= T) %>% as_tibble() %>%
  mutate(date = as.Date(dateTime, format = '%Y-%m-%d//%H:%M:%S')) %>%
  select(-id, -dateTime)

colnames(in_lake_nutrients) <- c('lake', 'TP', 'TN', 'SRP','PO4','NH4','NO3_NO2','NO3','NO2','DOC','depth_m', 'Comment','date')

# Lilli doesn't have DOC data; Mendota and Trout have multiple depths => using surface for their nutrients (0 meters)
in_lake_nutrients = in_lake_nutrients %>%
  dplyr::filter(depth_m %in% c(0,NA)) %>%
  group_by(lake, date) %>%
  summarise_all(funs(mean(.,na.rm=T))) %>%  # averaging if multiple samples (only occurs for Mendota and Trout I think)
  ungroup() %>%
  mutate(lake = case_when(lake == 'Lillsjölidtjärnen' ~ 'Lillsjoliden', # renaming for joining purposes
                          lake == 'Mångstrettjärn' ~ 'Mangstrettjarn',
                          lake == 'Nästjärn' ~ 'Nastjarn',
                          lake == 'Övre_Björntjärn' ~ 'Ovre',
                          lake == 'Struptjärn' ~ 'Struptjarn',
                          lake == 'Lilli' ~ 'Lillinonah',
                          TRUE ~ lake),
         DOC = case_when(lake != 'Trout' ~ DOC,
                         lake == 'Trout' & DOC < 10 ~ DOC),# getting rid of DOC outlier for Trout
         TP = case_when(lake != 'Trout' ~ TP,
                        lake == 'Trout' & TP > 0 ~ TP)) # getting rid of undetected TP for Trout

all_load = left_join(all_load, in_lake_nutrients, by = c('lake' = 'lake', 'date' = 'date'))

cv_cutoff = analysis_cfg$cv_cutoff
min_doy = analysis_cfg$min_doy
max_doy = analysis_cfg$max_doy

load_plot <- dplyr::filter(all_load, doy > min_doy, doy < max_doy) %>%
  group_by(lake) %>%
  dplyr::mutate(mean_tp = mean(TP_load / Volume..m3., na.rm=T)) %>%
  ungroup() %>%
  dplyr::mutate(lake = factor(lake),
         season = factor(season),
         plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'),
         TP_load = ifelse(TP_load == 0, NA, TP_load),
         TP_load = TP_load / 31, # changing to mol for stoichiometry plot
         TN_load = TN_load / 14,
         DOC_load = DOC_load / 12,
         TP = TP / 1000 / 31, # converting to mol/m3
         TN = TN / 1000 / 14,
         DOC = DOC / 12)

#ordering by mean gpp
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
load_stoich <- ggplot(load_plot, aes(x = plot_date, y = DOC_load/TP_load, group = lake)) +
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
  geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed')+
  scale_x_date(date_labels = '%b')
  # geom_hline(yintercept = 374.6, color = 'black', linetype = 'dashed')+ # maranger et al 2018 average stoich of watershed inputs for comparison
  # geom_hline(yintercept = 24.1, color ='#CC79A7', linetype = 'dashed')+
  # geom_hline(yintercept = 15.5, color = '#D55E00', linetype = 'dashed')
windows()
load_stoich

in_lake_stoich = ggplot(load_plot, aes(x = plot_date, y = DOC/TP, group = lake)) +
  geom_point(aes(color = 'a'), size = 1) +
  geom_point(data = load_plot, aes(x = plot_date, y = TN / TP, group = lake, color = 'b'), size = 1)+
  geom_point(data = load_plot, aes(x = plot_date, y = DOC / TN, group = lake, color = 'c'), size = 1)+
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
  ylab(expression(Lake~Stoichiometry~(mol:mol))) +
  scale_y_log10() +
  geom_hline(yintercept = 106, color = 'black', linetype = 'dashed')+ # redfield ratios
  geom_hline(yintercept = 16, color ='#CC79A7', linetype = 'dashed')+
  geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed')+
  scale_x_date(date_labels = '%b')

windows()
in_lake_stoich

lake_load_stoich <- ggplot(load_plot, aes(x = plot_date, y = DOC_load/TP_load, group = lake)) +
  geom_line(aes(color = 'a'), size = 1) +
  geom_line(data = load_plot, aes(x = plot_date, y = TN_load / TP_load, group = lake, color = 'b'), size = 1)+
  geom_line(data = load_plot, aes(x = plot_date, y = DOC_load / TN_load, group = lake, color = 'c'), size = 1)+
  geom_point(data = load_plot, aes(x = plot_date, y = DOC / TP, group = lake, color = 'a'), size = 1)+
  geom_point(data = load_plot, aes(x = plot_date, y = TN / TP, group = lake, color = 'b'), size = 1)+
  geom_point(data = load_plot, aes(x = plot_date, y = DOC / TN, group = lake, color = 'c'), size = 1)+
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
  ylab(expression(Load~or~Lake~Stoichiometry~(mol:mol))) +
  scale_y_log10() +
  geom_hline(yintercept = 106, color = 'black', linetype = 'dashed')+ # redfield ratios
  geom_hline(yintercept = 16, color ='#CC79A7', linetype = 'dashed')+
  geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed') +
  scale_x_date(date_labels = '%b')

lake_load_stoich

ggsave('figures/fig_load_stoich_timeseries.png', plot = load_stoich, width = 10, height = 10)
ggsave('figures/fig_lake_stoich_timeseries.png', plot = in_lake_stoich, width = 10, height = 10)
ggsave('figures/fig_lake_load_stoich_timeseries.png', plot = lake_load_stoich, width = 10, height = 10)


ave_load = load_plot %>%
  group_by(lake) %>%
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  ungroup()

ggplot(ave_load, aes(x = DOC_load/TP_load, y = DOC/TP)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0)


ggplot(ave_load, aes(x = DOC_load/TN_load, y = DOC/TN)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0)


ggplot(ave_load, aes(x = TN_load/TP_load, y = TN/TP)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0)




ggplot(load_plot, aes(x = plot_date, y = TN_load/TP_load, group = lake, color = lake)) +
  geom_line(size = 2, alpha = .6) +
  # geom_line(data = load_plot, aes(x = plot_date, y = TN_load / TP_load, group = lake, color = 'b'), size = 1)+
  # geom_line(data = load_plot, aes(x = plot_date, y = DOC_load / TN_load, group = lake, color = 'c'), size = 1)+
  # facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  ylab(expression(N:P~Load~Stoichiometry~(mol:mol))) +
  scale_y_log10() +
  geom_hline(yintercept = 16, color = 'black', linetype = 'dashed') # redfield ratios

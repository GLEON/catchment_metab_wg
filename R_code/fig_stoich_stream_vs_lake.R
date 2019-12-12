# plotting load timeseries for figure 2

library(dplyr)
library(ggplot2)
library(yaml)
library(cowplot)

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

metab_plot <- dplyr::filter(all_metab, doy > min_doy, doy < max_doy, GPP_SD/GPP < cv_cutoff, R_SD/(R*-1) < cv_cutoff) %>%
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
         DOC = DOC / 12) %>%
  group_by(lake) %>%
  dplyr::summarise(mean_c_p_load = mean(DOC_load/TP_load, na.rm = T),
                   mean_c_n_load = mean(DOC_load/TN_load, na.rm = T),
                   mean_n_p_load = mean(TN_load/TP_load, na.rm = T),
                   mean_c_p = mean(DOC/TP, na.rm = T),
                   mean_c_n = mean(DOC/TN, na.rm = T),
                   mean_n_p = mean(TN/TP, na.rm = T)) %>%
  ungroup()



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
c_p_load_vs_lake_stoich <- ggplot(load_plot, aes(x = mean_c_p_load, y = mean_c_p, group = lake)) +
  geom_point(size = 5) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(C:P~Load~Stoichiometry~(mol:mol))) +
  ylab(expression(C:P~Lake~Stoichiometry~(mol:mol))) +
  geom_abline(slope = 1, color = 'black', linetype = 'dashed') +
  annotate(geom = 'text',
           x = 9000, y = 9600, angle = 45, size = 6,
           label = '1:1') +
  xlim(range(c(load_plot$mean_c_p_load,load_plot$mean_c_p),na.rm = T)) +
  ylim(range(c(load_plot$mean_c_p_load,load_plot$mean_c_p),na.rm = T))

summary(lm(data = dplyr::filter(load_plot, !is.na(mean_c_p), !is.na(mean_c_p_load)),
           formula = mean_c_p~mean_c_p_load))

# load_vs_lake_stoich

c_n_load_vs_lake_stoich <- ggplot(load_plot, aes(x = mean_c_n_load, y = mean_c_n, group = lake)) +
  geom_point(size = 5, color = '#D55E00') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(C:N~Load~Stoichiometry~(mol:mol))) +
  ylab(expression(C:N~Lake~Stoichiometry~(mol:mol))) +
  geom_abline(slope = 1, color = 'black', linetype = 'dashed')  +
  annotate(geom = 'text',
           x = 80, y = 87, angle = 45, size = 6,
           label = '1:1') +
  xlim(range(c(load_plot$mean_c_n_load,load_plot$mean_c_n),na.rm = T)) +
  ylim(range(c(load_plot$mean_c_n_load,load_plot$mean_c_n),na.rm = T))

summary(lm(data = dplyr::filter(load_plot, !is.na(mean_c_n), !is.na(mean_c_n_load)),
           formula = mean_c_n~mean_c_n_load))
# load_vs_lake_stoich

n_p_load_vs_lake_stoich <- ggplot(load_plot, aes(x = mean_n_p_load, y = mean_n_p, group = lake)) +
  geom_point(size = 5, color ='#CC79A7') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(N:P~Load~Stoichiometry~(mol:mol))) +
  ylab(expression(N:P~Lake~Stoichiometry~(mol:mol))) +
  geom_abline(slope = 1, color = 'black', linetype = 'dashed')  +
  annotate(geom = 'text',
           x = 160, y = 175, angle = 45, size = 6,
           label = '1:1') +
  xlim(range(c(load_plot$mean_n_p_load,load_plot$mean_n_p),na.rm = T)) +
  ylim(range(c(load_plot$mean_n_p_load,load_plot$mean_n_p),na.rm = T))

summary(lm(data = dplyr::filter(load_plot, !is.na(mean_n_p), !is.na(mean_n_p_load)),
           formula = mean_n_p~mean_n_p_load))
# load_vs_lake_stoich



g = plot_grid(c_n_load_vs_lake_stoich, c_p_load_vs_lake_stoich, n_p_load_vs_lake_stoich,
              labels = c('A', 'B', 'C'), align = 'hv',nrow = 2)

# g

ggsave('figures/fig_stoich_stream_vs_lake.png', plot = g, width = 8, height = 8)





# plotting load timeseries for figure 2

library(dplyr)
library(cowplot)
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

metab_plot <- dplyr::filter(all_metab, doy > min_doy, doy < max_doy, GPP_SD/GPP < cv_cutoff) %>%
  group_by(lake) %>%
  dplyr::summarise(mean_gpp = mean(GPP, na.rm=T),
            mean_r = mean(R, na.rm =T),
            mean_nep = mean(NEP, na.rm=T)) %>%
  ungroup()

#### loading in nutrient load time series ###
dir<-'results/nutrient load/' # directory of load data
files<-list.files(dir) # folders in this dir
files<-files[-grep('Readme',files)] # get rid of README doc

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
  select(Lake.Name, Volume..m3., Surface.Area..m2., Catchment.Area..km2., Lake.Residence.Time..year., kD)
all_load <- left_join(all_load, metaData, by = c('lake' = 'Lake.Name'))

# Units for nutrient load files = kg/day for TP, TN, DOC and m3/s for inflow
all_load <- all_load %>%
  mutate(ave_tp_conc_load_mol_m3 = TP_load / 31 * 1000 / (inflow * 86400),
         ave_tn_conc_load_mol_m3 = TN_load / 14 * 1000 / (inflow * 86400),
         ave_doc_conc_load_mol_m3 = DOC_load / 12 * 1000 / (inflow * 86400))

cv_cutoff = analysis_cfg$cv_cutoff
min_doy = analysis_cfg$min_doy
max_doy = analysis_cfg$max_doy

load_plot <- dplyr::filter(all_load, doy > min_doy, doy < max_doy) %>%
  group_by(lake) %>%
  dplyr::summarise(mean_tp_load = mean(TP_load / Volume..m3., na.rm=T),
                   sd_tp_load = sd(TP_load / Volume..m3., na.rm=T),
                   mean_tn_load = mean(TN_load / Volume..m3., na.rm =T),
                   sd_tn_load = sd(TN_load / Volume..m3., na.rm =T),
                   mean_doc_load = mean(DOC_load / Volume..m3., na.rm=T),
                   sd_doc_load = sd(DOC_load / Volume..m3., na.rm=T),
                   mean_doc_tp_load = mean((DOC_load / 12) / (TP_load/31), na.rm=T),
                   mean_doc_tn_load = mean((DOC_load / 12) / (TN_load/14), na.rm=T),
                   mean_tn_tp_load = mean((TN_load / 14) / (TP_load/31), na.rm=T),
                   mean_tp_conc_load_mol_m3 = mean(ave_tp_conc_load_mol_m3, na.rm = T),
                   mean_tn_conc_load_mol_m3 = mean(ave_tn_conc_load_mol_m3, na.rm = T),
                   mean_doc_conc_load_mol_m3 = mean(ave_doc_conc_load_mol_m3, na.rm = T),
                   mean_inflow_m3 = mean((inflow * 86400), na.rm = T),
                   kD = mean(kD)) %>%
  ungroup()

plot_data <- left_join(load_plot, metab_plot, by = 'lake')

#ordering by mean inflow
lakes_sorted <- plot_data$lake[sort.list(plot_data$mean_gpp)]
lakes_sorted <- as.character(lakes_sorted[!duplicated(lakes_sorted)])

plot_data$lake <- factor(plot_data$lake,levels = lakes_sorted)

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

doc_tp <- ggplot(plot_data, aes(x = mean_doc_load *1000*1000, y = mean_tp_load *1000*1000, group = lake)) +
  geom_abline(slope = (1*31)/(106*12), intercept = 0, linetype = 'dashed') +
  geom_point(size = 5, alpha = .5) +
  # geom_errorbar(aes(ymin = mean_tp_load *1000*1000 - sd_tp_load*1000*1000,
  #                    ymax = mean_tp_load *1000*1000 + sd_tp_load*1000*1000))+
  # geom_errorbarh(aes(xmin = mean_doc_load *1000*1000 - sd_doc_load*1000*1000,
  #                    xmax = mean_doc_load *1000*1000 + sd_doc_load*1000*1000))+
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(DOC~Load~(mg~m^-3~day^-1))) +
  ylab(expression(TP~Load~(mg~m^-3~day^-1)))

doc_tp

doc_tn <- ggplot(plot_data, aes(x = mean_doc_load *1000*1000, y = mean_tn_load *1000*1000, group = lake)) +
  geom_abline(slope = (16*14)/(106*12), intercept = 0, linetype = 'dashed') +
  geom_point(size = 5, alpha = .5) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(DOC~Load~(mg~m^-3~day^-1))) +
  ylab(expression(TN~Load~(mg~m^-3~day^-1)))

doc_tn

tn_tp <- ggplot(plot_data, aes(x = mean_tn_load *1000*1000, y = mean_tp_load *1000*1000, group = lake)) +
  geom_abline(slope = (1*31)/(16*14), intercept = 0, linetype = 'dashed') +
  geom_point(size = 5, alpha = .5) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(TN~Load~(mg~m^-3~day^-1))) +
  ylab(expression(TP~Load~(mg~m^-3~day^-1)))

tn_tp


g = plot_grid(doc_tn, doc_tp, tn_tp,
              labels = c('A', 'B', 'C'), align = 'hv',nrow = 2)

g

ggsave('figures/fig_load_1_to_1.png', plot = g, width = 8, height = 8)



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

metab_plot <- dplyr::filter(all_metab, doy > min_doy, doy < max_doy, GPP_SD/GPP < cv_cutoff, R_SD/(R*-1) < cv_cutoff) %>%
  group_by(lake) %>%
  dplyr::summarise(mean_gpp = mean(GPP, na.rm=T),
            mean_r = mean(R, na.rm =T),
            mean_nep = mean(NEP, na.rm=T)) %>%
  ungroup()

#### loading in nutrient load time series ###
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
  select(Lake.Name, Volume..m3., Surface.Area..m2., Catchment.Area..km2., Lake.Residence.Time..year., kD)
all_load <- left_join(all_load, metaData, by = c('lake' = 'Lake.Name'))

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
                   kD = mean(kD),
                   mean_lake_doc = mean(DOC, na.rm = T),
                   mean_lake_tn = mean(TN, na.rm = T),
                   mean_lake_tp = mean(TP, na.rm = T)) %>%
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

redfield_line = function(ratio, x_axis_element, x_axis_range){
  if(ratio == 'n_p'){
    if(x_axis_element == 'n'){
      out = tibble(x = seq(x_axis_range[1],x_axis_range[2], length.out = 100))
      out$y = out$x * (1*31)/(16*14)
    }else if(x_axis_element == 'p'){
      out = tibble(x = seq(x_axis_range[1],x_axis_range[2], length.out = 100))
      out$y = out$x * (16*14)/(1*31)
    }
  }else if(ratio == 'c_p'){
    if(x_axis_element == 'c'){
      out = tibble(x = seq(x_axis_range[1],x_axis_range[2], length.out = 100))
      out$y = out$x * (1*31)/(106*12) * 1000
    }else if(x_axis_element == 'p'){
      out = tibble(x = seq(x_axis_range[1],x_axis_range[2], length.out = 100))
      out$y = out$x / 1000 * (106*12)/(1*31)
    }
  }else if(ratio == 'c_n'){
    if(x_axis_element == 'c'){
      out = tibble(x = seq(x_axis_range[1],x_axis_range[2], length.out = 100))
      out$y = out$x * (16*14)/(106*12) * 1000
    }else if(x_axis_element == 'n'){
      out = tibble(x = seq(x_axis_range[1],x_axis_range[2], length.out = 100))
      out$y = out$x / 1000 * (106*12)/(16*14)
    }
  }
  return(out)
}

doc_tp <- ggplot(plot_data, aes(x = mean_doc_load *1000*1000, y = mean_tp_load *1000*1000*1000)) +
  geom_line(data = redfield_line(ratio = 'c_p',
                                 x_axis_element = 'c',
                                 x_axis_range = range(plot_data$mean_doc_load *1000*1000, na.rm = T)),
            aes(x = x, y = y),
            linetype = 'dashed') +
  geom_point(size = 5) +
  annotate(geom = 'text',
           x = 10, y = 420, angle = 42, size = 6,
           label = 'Redfield Ratio') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(DOC~Load~(mg~C~m^-3~day^-1))) +
  ylab(expression(TP~Load~(mu*g~P~m^-3~day^-1)))+ scale_y_log10() + scale_x_log10()
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))

doc_tp


doc_tn <- ggplot(plot_data, aes(x = mean_doc_load *1000*1000, y = mean_tn_load *1000*1000*1000)) +
  # geom_abline(slope = (16*14)/(106*12), intercept = 0, linetype = 'dashed') +
  geom_line(data = redfield_line(ratio = 'c_n',
                                 x_axis_element = 'c',
                                 x_axis_range = range(plot_data$mean_doc_load *1000*1000, na.rm = T)),
            aes(x = x, y = y),
            linetype = 'dashed') +
  geom_point(size = 5, color = '#D55E00') +
  annotate(geom = 'text',
           x = 10, y = 5000, angle = 42, size = 6,
           label = 'Redfield Ratio') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(DOC~Load~(mg~C~m^-3~day^-1))) +
  ylab(expression(TN~Load~(mu*g~P~m^-3~day^-1))) + scale_y_log10() + scale_x_log10()
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))

doc_tn

tn_tp <- ggplot(plot_data, aes(x = mean_tn_load *1000*1000*1000, y = mean_tp_load *1000*1000*1000)) +
  # geom_abline(slope = (1*31)/(16*14), intercept = 0, linetype = 'dashed') +
  geom_line(data = redfield_line(ratio = 'n_p',
                                 x_axis_element = 'n',
                                 x_axis_range = range(plot_data$mean_tn_load *1000*1000*1000, na.rm = T)),
            aes(x = x, y = y),
            linetype = 'dashed') +
  geom_point(size = 5, color ='#CC79A7') +
  annotate(geom = 'text',
           x = 1000, y = 600, angle = 47, size = 6,
           label = 'Redfield Ratio') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(TN~Load~(mu*g~m^-3~day^-1))) +
  ylab(expression(TP~Load~(mu*g~m^-3~day^-1))) +
  scale_y_log10() + scale_x_log10()
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))

tn_tp

lake_doc_tp <- ggplot(plot_data, aes(x = mean_lake_doc, y = mean_lake_tp)) +
  geom_line(data = redfield_line(ratio = 'c_p',
                                 x_axis_element = 'c',
                                 x_axis_range = range(plot_data$mean_lake_doc, na.rm = T)),
            aes(x = x, y = y),
            linetype = 'dashed') +
  geom_point(size = 5) +
  annotate(geom = 'text',
           x = 10, y = 320, angle = 26, size = 6,
           label = 'Redfield Ratio') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(Lake~DOC~(mg~C~L^-1))) +
  ylab(expression(Lake~TP~(mu*g~P~L^-1))) +
  scale_y_log10() + scale_x_log10()
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x)))

lake_doc_tp


lake_doc_tn <- ggplot(plot_data, aes(x = mean_lake_doc, y = mean_lake_tn)) +
  geom_line(data = redfield_line(ratio = 'c_n',
                                 x_axis_element = 'c',
                                 x_axis_range = range(plot_data$mean_lake_doc, na.rm = T)),
            aes(x = x, y = y),
            linetype = 'dashed') +
  geom_point(size = 5, color = '#D55E00') +
  annotate(geom = 'text',
           x = 10, y = 2300, angle = 37, size = 6,
           label = 'Redfield Ratio') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(Lake~DOC~(mg~C~L^-1))) +
  ylab(expression(Lake~TN~(mu*g~N~L^-1)))  +
  scale_y_log10() + scale_x_log10()
# scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#               labels = trans_format("log10", math_format(10^.x))) +
# scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#               labels = trans_format("log10", math_format(10^.x)))

lake_doc_tn

lake_tn_tp <- ggplot(plot_data, aes(x = mean_lake_tn, y = mean_lake_tp)) +
  geom_line(data = redfield_line(ratio = 'n_p',
                                 x_axis_element = 'n',
                                 x_axis_range = range(plot_data$mean_lake_tn, na.rm = T)),
            aes(x = x, y = y),
            linetype = 'dashed') +
  geom_point(size = 5, color ='#CC79A7') +
  annotate(geom = 'text',
           x = 1000, y = 230, angle = 35, size = 6,
           label = 'Redfield Ratio') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab(expression(Lake~TN~(mu*g~N~L^-1))) +
  ylab(expression(Lake~TP~(mu*g~P~L^-1))) +
  scale_y_log10() + scale_x_log10()
# scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#               labels = trans_format("log10", math_format(10^.x))) +
# scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#               labels = trans_format("log10", math_format(10^.x)))

lake_tn_tp

g = plot_grid(doc_tn, doc_tp, tn_tp, lake_doc_tn, lake_doc_tp, lake_tn_tp,
              labels = c('A', 'B', 'C', 'D', 'E', 'F'), align = 'hv',nrow = 2)

# g

ggsave('figures/fig_load_lake_scatter.png', plot = g, width = 12, height = 8)



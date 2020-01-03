# statistics for predictors of lake metabolism

library(dplyr)
library(cowplot)
library(ggplot2)
library(yaml)
library(MuMIn)
library(kableExtra)
library(rcompanion)
library(scales)

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
  group_by(lake, season) %>%
  dplyr::summarise(mean_gpp = mean(GPP, na.rm=T),
                   mean_r = mean(R, na.rm =T),
                   mean_nep = mean(NEP, na.rm=T)) %>%
  ungroup()

metab_plot_annual <- dplyr::filter(all_metab, doy > min_doy, doy < max_doy, GPP_SD/GPP < cv_cutoff, R_SD/(R*-1) < cv_cutoff) %>%
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
  select(Lake.Name, Volume..m3., Surface.Area..m2., Catchment.Area..km2., Lake.Residence.Time..year., kD, Number.of.streams, Land.Use...Land.Cover.Description, Latitude..decimal.degrees., Longitude..decimal.degrees.)
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

summary_df <- dplyr::filter(all_load, doy > min_doy, doy < max_doy) %>%
  group_by(lake) %>%
  dplyr::summarise(mean_tp_load = mean(TP_load / Volume..m3., na.rm=T),
                   mean_tn_load = mean(TN_load / Volume..m3., na.rm =T),
                   mean_doc_load = mean(DOC_load / Volume..m3., na.rm=T),
                   mean_doc_tp_load = mean((DOC_load / 12) / (TP_load/31), na.rm=T),
                   mean_doc_tn_load = mean((DOC_load / 12) / (TN_load/14), na.rm=T),
                   mean_tn_tp_load = mean((TN_load / 14) / (TP_load/31), na.rm=T),
                   mean_tp_conc_load_ug_L = mean(ave_tp_conc_load_mol_m3 * 31 *1000*1000/1000, na.rm = T),
                   mean_tn_conc_load_ug_L = mean(ave_tn_conc_load_mol_m3 * 14 *1000*1000/1000, na.rm = T),
                   mean_doc_conc_load_mg_L = mean(ave_doc_conc_load_mol_m3 * 12 *1000/1000, na.rm = T),
                   mean_inflow_m3 = mean((inflow * 86400), na.rm = T),
                   kD = mean(kD),
                   mean_lake_tp = mean(TP, na.rm = T),
                   mean_lake_tn = mean(TN, na.rm = T),
                   mean_lake_doc = mean(DOC, na.rm = T),
                   WRT = mean(Lake.Residence.Time..year., na.rm = T),
                   n_streams = mean(Number.of.streams, na.rm = T),
                   vol = mean(Volume..m3., na.rm = T),
                   area = mean(Surface.Area..m2., na.rm = T),
                   basin_area = mean(Catchment.Area..km2., na.rm = T) * 1000 * 1000,
                   mean_z = vol / area,
                   drainage_ratio = basin_area / area,
                   lat = mean(Latitude..decimal.degrees., na.rm = T),
                   long = mean(Longitude..decimal.degrees., na.rm = T)) %>%
  ungroup()

summary_df <- left_join(summary_df, metab_plot_annual, by = c('lake'))


# summarized data
summary(summary_df)

# are stream / lake concentrations correlated?
summary(lm(summary_df$mean_lake_doc~summary_df$mean_doc_conc_load_mg_L)) # no
summary(lm(summary_df$mean_lake_tp~summary_df$mean_tp_conc_load_ug_L)) # yes
summary(lm(summary_df$mean_lake_tn~summary_df$mean_tn_conc_load_ug_L)) # yes

plot(summary_df$mean_lake_doc~summary_df$mean_doc_conc_load_mg_L)
plot(summary_df$mean_lake_tp~summary_df$mean_tp_conc_load_ug_L)
plot(summary_df$mean_lake_tn~summary_df$mean_tn_conc_load_ug_L)

summary(lm(summary_df$mean_lake_doc~summary_df$mean_doc_load))
summary(lm(summary_df$mean_lake_tp~summary_df$mean_tp_load))
summary(lm(summary_df$mean_lake_tn~summary_df$mean_tn_load))

plot(summary_df$mean_lake_doc~summary_df$mean_doc_load)
plot(summary_df$mean_lake_tp~summary_df$mean_tp_load)
plot(summary_df$mean_lake_tn~summary_df$mean_tn_load)

doc <- ggplot(summary_df, aes(y = mean_lake_doc, x = mean_doc_conc_load_mg_L)) +
  geom_point(size = 5) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  ylab(expression(Lake~DOC~(mg~L^-1))) +
  xlab(expression(Stream~DOC~(mg~L^-1))) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  annotate(geom = 'text',
           x = 15, y = 20, size = 6,
           label = '1:1')

doc

tn <- ggplot(summary_df, aes(y = mean_lake_tn, x = mean_tn_conc_load_ug_L)) +
  geom_point(size = 5) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  ylab(expression(Lake~TN~(mu*g~L^-1))) +
  xlab(expression(Stream~TN~(mu*g~L^-1))) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  annotate(geom = 'text',
           x = 2500, y = 3000, size = 6,
           label = '1:1')
tn

tp <- ggplot(summary_df, aes(y = mean_lake_tp, x = mean_tp_conc_load_ug_L)) +
  geom_point(size = 5) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  ylab(expression(Lake~TP~(mu*g~L^-1))) +
  xlab(expression(Stream~TP~(mu*g~L^-1)))+
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
  annotate(geom = 'text',
           x = 50, y = 60, size = 6,
           label = '1:1')
tp

g = plot_grid(doc, tn, tp,
              labels = c('A', 'B', 'C'), align = 'hv',nrow = 2)

ggsave('figures/fig_stream_lake_conc_1_1.png', plot = g, width = 8, height = 8)


# long data for plotting
long_df = summary_df %>%
  select(lake, mean_tp_conc_load_ug_L, mean_tn_conc_load_ug_L,
         mean_doc_conc_load_mg_L, mean_lake_doc, mean_lake_tp, mean_lake_tn) %>%
  tidyr::gather(key = 'constituent', 'concentration', contains('mean')) %>%
  mutate(lake_stream = case_when(grepl(pattern = 'load', x = constituent)~ 'stream',
                                 grepl(pattern = 'lake', x = constituent) ~ 'lake'),
         constituent = case_when(grepl(pattern = 'tp', x = constituent)~ 'tp',
                                 grepl(pattern = 'tn', x = constituent) ~ 'tn',
                                 grepl(pattern = 'doc', x = constituent) ~ 'doc'),
         concentration = case_when(constituent == 'tp' ~ concentration / 1000, # converting to mg / L
                                   constituent == 'tn' ~ concentration / 1000,
                                   TRUE ~ concentration),
         constituent = paste(constituent, lake_stream, sep = '_'))


long_df

long_df$constituent = factor(long_df$constituent,
                             levels = c('doc_stream', 'doc_lake', 'tn_stream', 'tn_lake', 'tp_stream', 'tp_lake'))


concentration_plot = ggplot(long_df, aes(x =constituent, y = concentration, group = constituent, fill = lake_stream)) +
  geom_boxplot() +
  geom_jitter(width = .1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_discrete(labels=c('Stream DOC', 'Lake DOC', 'Stream TN', 'Lake TN', 'Stream TP', 'Lake TP')) +
  scale_fill_manual(name = 'lake_stream',
                    values = c('dodgerblue1','lightblue'),
                    labels = c('Lake', 'Stream')) +
  annotation_logticks(sides = 'l') +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.placement = 'inside',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size =12)) +
  xlab('') +
  ylab(expression(Concentration~(mg~L^-1)))


ggsave('figures/fig_stream_lake_concentration.png', plot = concentration_plot,  width = 8, height = 8)


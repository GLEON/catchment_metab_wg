# statistics for predictors of lake metabolism

library(dplyr)
library(cowplot)
library(ggplot2)
library(yaml)
library(MuMIn)
library(kableExtra)

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
  group_by(lake, season) %>%
  dplyr::summarise(mean_gpp = mean(GPP, na.rm=T),
            mean_r = mean(R, na.rm =T),
            mean_nep = mean(NEP, na.rm=T)) %>%
  ungroup()

metab_plot_annual <- dplyr::filter(all_metab, doy > min_doy, doy < max_doy, GPP_SD/GPP < cv_cutoff) %>%
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
  group_by(lake, season) %>%
  dplyr::summarise(mean_tp_load = mean(TP_load / Volume..m3., na.rm=T),
            mean_tn_load = mean(TN_load / Volume..m3., na.rm =T),
            mean_doc_load = mean(DOC_load / Volume..m3., na.rm=T),
            mean_doc_tp_load = mean((DOC_load / 12) / (TP_load/31), na.rm=T),
            mean_doc_tn_load = mean((DOC_load / 12) / (TN_load/14), na.rm=T),
            mean_tn_tp_load = mean((TN_load / 14) / (TP_load/31), na.rm=T),
            mean_tp_conc_load_mol_m3 = mean(ave_tp_conc_load_mol_m3, na.rm = T),
            mean_tn_conc_load_mol_m3 = mean(ave_tn_conc_load_mol_m3, na.rm = T),
            mean_doc_conc_load_mol_m3 = mean(ave_doc_conc_load_mol_m3, na.rm = T),
            mean_inflow_m3 = mean((inflow * 86400), na.rm = T),
            kD = mean(kD),
            mean_lake_tp = mean(TP, na.rm = T),
            mean_lake_tn = mean(TN, na.rm = T),
            mean_lake_doc = mean(DOC, na.rm = T)) %>%
  ungroup()

load_plot_annual <- dplyr::filter(all_load, doy > min_doy, doy < max_doy) %>%
  group_by(lake) %>%
  dplyr::summarise(mean_tp_load = mean(TP_load / Volume..m3., na.rm=T),
                   mean_tn_load = mean(TN_load / Volume..m3., na.rm =T),
                   mean_doc_load = mean(DOC_load / Volume..m3., na.rm=T),
                   mean_doc_tp_load = mean((DOC_load / 12) / (TP_load/31), na.rm=T),
                   mean_doc_tn_load = mean((DOC_load / 12) / (TN_load/14), na.rm=T),
                   mean_tn_tp_load = mean((TN_load / 14) / (TP_load/31), na.rm=T),
                   mean_tp_conc_load_mol_m3 = mean(ave_tp_conc_load_mol_m3, na.rm = T),
                   mean_tn_conc_load_mol_m3 = mean(ave_tn_conc_load_mol_m3, na.rm = T),
                   mean_doc_conc_load_mol_m3 = mean(ave_doc_conc_load_mol_m3, na.rm = T),
                   mean_inflow_m3 = mean((inflow * 86400), na.rm = T),
                   kD = mean(kD),
                   mean_lake_tp = mean(TP, na.rm = T),
                   mean_lake_tn = mean(TN, na.rm = T),
                   mean_lake_doc = mean(DOC, na.rm = T)) %>%
  ungroup()

plot_data <- left_join(load_plot, metab_plot, by = c('lake', 'season'))
plot_data_annual <- left_join(load_plot_annual, metab_plot_annual, by = c('lake'))


## multi model selection based on AIC

gpp_out = tibble()
r_out = tibble()
nep_out = tibble()
seasons = c('spring', 'summer', 'fall', 'annual')
for(i in seasons){
  if(i == 'annual'){
    # GPP
    options(na.action = 'na.fail')

    predictors = c('mean_tp_load', 'mean_tn_load', 'mean_doc_load', 'mean_doc_tp_load', 'mean_doc_tn_load', 'mean_tn_tp_load')
    gpp_data = plot_data_annual %>%
      select(rbind('mean_gpp',predictors)) %>%
      na.omit()
    gpp_corr_matrix = gpp_data %>% as.matrix() %>% Hmisc::rcorr()

    global_model_gpp = lm(mean_gpp ~ ., data = gpp_data)

    all_gpp_mods = dredge(global_model_gpp) %>% dplyr::filter(delta <= 2) %>% as_tibble()
    all_gpp_mods = mutate(all_gpp_mods, season = i, lakes = nrow(gpp_data))

    # ER
    r_data = plot_data_annual %>%
      select(rbind('mean_r',predictors)) %>%
      na.omit() %>%
      mutate(mean_r = mean_r * -1)

    global_model_r = lm(mean_r ~ ., data = r_data)

    all_r_mods = dredge(global_model_r) %>% dplyr::filter(delta <= 2) %>% as_tibble()
    all_r_mods = mutate(all_r_mods, season = i, lakes = nrow(r_data))

    # NEP
    nep_data = plot_data_annual %>%
      select(rbind('mean_nep',predictors)) %>%
      na.omit()

    global_model_nep = lm(mean_nep ~ ., data = nep_data)

    all_nep_mods = dredge(global_model_nep) %>% dplyr::filter(delta <= 2) %>% as_tibble()
    all_nep_mods = mutate(all_nep_mods, season = i, lakes = nrow(nep_data))

    gpp_out = bind_rows(gpp_out, all_gpp_mods)
    r_out = bind_rows(r_out, all_r_mods)
    nep_out = bind_rows(nep_out, all_nep_mods)
  }else{
    # GPP
    options(na.action = 'na.fail')

    predictors = c('mean_tp_load', 'mean_tn_load', 'mean_doc_load', 'mean_doc_tp_load', 'mean_doc_tn_load', 'mean_tn_tp_load')
    gpp_data = plot_data %>%
      dplyr::filter(season == i) %>%
      select(rbind('mean_gpp',predictors)) %>%
      na.omit()
    gpp_corr_matrix = gpp_data %>% as.matrix() %>% Hmisc::rcorr()

    global_model_gpp = lm(mean_gpp ~ ., data = gpp_data)

    all_gpp_mods = dredge(global_model_gpp) %>% dplyr::filter(delta <= 2) %>% as_tibble()
    all_gpp_mods = mutate(all_gpp_mods, season = i, lakes = nrow(gpp_data))

    # ER
    r_data = plot_data %>%
      dplyr::filter(season == i) %>%
      select(rbind('mean_r',predictors)) %>%
      na.omit() %>%
      mutate(mean_r = mean_r * -1)

    global_model_r = lm(mean_r ~ ., data = r_data)

    all_r_mods = dredge(global_model_r) %>% dplyr::filter(delta <= 2) %>% as_tibble()
    all_r_mods = mutate(all_r_mods, season = i, lakes = nrow(r_data))

    # NEP
    nep_data = plot_data %>%
      dplyr::filter(season == i) %>%
      select(rbind('mean_nep',predictors)) %>%
      na.omit()

    global_model_nep = lm(mean_nep ~ ., data = nep_data)

    all_nep_mods = dredge(global_model_nep) %>% dplyr::filter(delta <= 2) %>% as_tibble()
    all_nep_mods = mutate(all_nep_mods, season = i, lakes = nrow(nep_data))

    gpp_out = bind_rows(gpp_out, all_gpp_mods)
    r_out = bind_rows(r_out, all_r_mods)
    nep_out = bind_rows(nep_out, all_nep_mods)
  }
}

all_out = bind_rows(gpp_out, r_out, nep_out) %>%
  mutate(metab_response = c(rep('gpp',nrow(gpp_out)), rep('r', nrow(r_out)), rep('nep', nrow(nep_out))))

saveRDS(all_out, 'results/AIC_models/metab_aic.rds')

all_out %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  save_kable(file = 'results/AIC_models/metab_aic_table.html', self_contained = T)

###########################################################


# false discovery rate control
# gpp_fdr = as_tibble(gpp_corr_matrix$P) %>%
#   select(mean_gpp) %>%
#   mutate(predictor = rownames(gpp_corr_matrix$P)) %>%
#   rename(pval = mean_gpp) %>%
#   na.omit()
#
# fdrtool(gpp_fdr$pval, statistic = 'pvalue')






collate_metab_timeseries <- function(
  metab_dir
){

  dir <- metab_dir # directory of metabolism data
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

  return(all_metab)
}


filter_metab_timeseries <- function(
  metab_data,
  season_data,
  config
){
  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  metab_data <- left_join(metab_data,
                          season_data,
                          by = c('lake' = 'lake', 'date' = 'date'))

  metab_data <- dplyr::filter(metab_data,
                              doy > min_doy,
                              doy < max_doy,
                              GPP_SD/GPP < cv_cutoff,
                              R_SD/abs(R) < cv_cutoff,
                              GPP > 0,
                              R < 0) %>%
    dplyr::group_by(lake) %>%
    dplyr::mutate(mean_gpp = mean(GPP, na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(lake = factor(lake),
                  season = factor(season),
                  plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'))

  return(metab_data)
}


collate_load_timeseries <- function(
  load_dir,
  config,
  metab_data,
  metadata_file,
  season_cutoff_file
){

  browser()
  all_metab = metab_data

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  metab_plot <- dplyr::filter(all_metab,
                              doy > min_doy,
                              doy < max_doy,
                              GPP_SD/GPP < cv_cutoff,
                              R_SD/abs(R) < cv_cutoff,
                              GPP > 0,
                              R < 0) %>%
    group_by(lake) %>%
    dplyr::mutate(mean_gpp = mean(GPP, na.rm=T)) %>%
    ungroup()

  dir<-load_dir # directory of load data
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

  browser()
  season_cutoff <- readRDS(season_cutoff_file) %>%
    select(-doy)# seasonal cutoff based on z-scored schmidt stability
  all_load <- left_join(all_load, season_cutoff, by = c('lake' = 'lake', 'date' = 'date'))

  metaData <- read.csv(metadata_file,stringsAsFactor=F) %>%
    select(Lake.Name, Volume..m3., Surface.Area..m2., Catchment.Area..km2., Lake.Residence.Time..year.)
  all_load <- left_join(all_load, metaData, by = c('lake' = 'Lake.Name'))

  all_load <- left_join(all_load,
                        dplyr::select(metab_plot, lake, date, mean_gpp),
                        by = c('lake'='lake','date'='date'))

  return(all_load)
}


collate_inlake_nutrient_timeseries <- function(
  inlake_nutrient_file
){

  in_lake_nutrients = read_csv(inlake_nutrient_file)

  # Lilli doesn't have DOC data; Mendota and Trout have multiple depths => using surface for their nutrients (0 meters)
  in_lake_nutrients = in_lake_nutrients %>%
    dplyr::filter(depth_m %in% c(0,NA)) %>%
    group_by(lake, date) %>%
    summarise_all(funs(mean(.,na.rm=T))) %>%  # averaging if multiple samples (only occurs for Mendota and Trout I think)
    ungroup() %>%
    mutate(DOC = case_when(lake != 'Trout' ~ DOC,
                           lake == 'Trout' & DOC < 10 ~ DOC),# getting rid of DOC outlier for Trout
           TP = case_when(lake != 'Trout' ~ TP,
                          lake == 'Trout' & TP > 0 ~ TP)) # getting rid of undetected TP for Trout

  return(in_lake_nutrients)
}


load_season_cutoff <- function(
  in_file
){
  season_cutoff <- readRDS(in_file) %>%
    select(-doy)

  return(season_cutoff)
}

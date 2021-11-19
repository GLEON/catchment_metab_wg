


collate_metab_timeseries <- function(
  metab_dir,
  config
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

  season_cutoff <- readRDS('results/z_scored_schmidt.rds') %>%
    select(-doy)# seasonal cutoff based on z-scored schmidt stability
  all_metab <- left_join(all_metab, season_cutoff, by = c('lake' = 'lake', 'date' = 'date'))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  metab_plot <- dplyr::filter(all_metab, doy > min_doy, doy < max_doy, GPP_SD/GPP < cv_cutoff, R_SD/abs(R) < cv_cutoff, GPP > 0, R < 0) %>%
    dplyr::group_by(lake) %>%
    dplyr::mutate(mean_gpp = mean(GPP, na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(lake = factor(lake),
                  season = factor(season),
                  plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'))


  return(metab_plot)
}

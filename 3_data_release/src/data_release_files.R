

load_metab_data_release <- function(
    load_dir,
    config,
    metab_data,
    metadata_file,
    out_file
){
  all_metab = metab_data

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  # flag maetabolism data if it was removed. 1 means it was removed, 0 means it remained
  metab_plot <- mutate(all_metab,
                       metab_flag = case_when(
                         GPP_SD/GPP >= cv_cutoff |
                         R_SD/abs(R) >= cv_cutoff |
                         GPP < 0 |
                         R > 0
                         ~ 1,
                         TRUE ~ 0)) %>%
    select(lake, date, GPP, GPP_SD, R, R_SD, NEP, metab_flag) %>%
    rename(GPP_sd = GPP_SD, R_sd = R_SD)


  dir<-load_dir # directory of load data
  files<-list.files(dir) # folders in this dir
  files<-files[-grep("README",files)] # get rid of README doc

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

  metaData <- read.csv(metadata_file,stringsAsFactor=F) %>%
    select(Lake.Name, Volume..m3., Surface.Area..m2., Catchment.Area..km2., Lake.Residence.Time..year.)
  all_load <- left_join(all_load, metaData, by = c('lake' = 'Lake.Name'))

  out <- full_join(all_load,
                   metab_plot,
                   by = c("lake", "date"))

  # The load data for Lilli is from USGS. When looking to see whether there
  # was any TOC data, I realized that the load data I submitted as DOC was
  # actually OC measured on an unfiltered sample so was actually TOC.  It
  # appears that sometimes they filter and sometimes they don't filter the
  # sample before analyzing - there are two separate parameter codes. I was
  # able to find a few dates where they have both (but not for 2014).  If
  # you use an average of all the samples (n=12), DOC is 90% of TOC, the
  # median is 92%. If you use an average of Jun-Oct (the time window of our
  # study, n=5), DOC is 93% of TOC, the median is 95%. The range is 80-97%
  # I don't think this will make a big difference in the results but it seems
  # like we should correct it. I'm not sure what would be best.

  ## Let's use a scaling factor of 0.93 for Lillinonah C loads
  out <- out %>%
    mutate(DOC_load = case_when(lake == 'Lillinonah' ~ DOC_load * 0.93,
                                TRUE ~ DOC_load))
  # unit conversions for loads
  # currently in kg/day for TP, TN, DOC load and m3/s for inflow
  # need to conver loads to ug N/P m-3(lake water) day-1 for TN/TP loads and
  #  mg C m-3(lake water) day-1 for DOC loads
  out <- mutate(out,
                TN_load = TN_load / Volume..m3. * 1000 * 1000 * 1000, # ug N / (m3 lake water) / day
                TP_load = TP_load / Volume..m3. * 1000 * 1000 * 1000, # ug P / (m3 lake water) / day
                DOC_load = DOC_load / Volume..m3. * 1000 * 1000) %>%   # mg C / (m3 lake water) / day
    select(lake, date, GPP, GPP_sd, R, R_sd, NEP, metab_flag,
           DOC_load, TN_load, TP_load) %>%
    rename(Lake = lake, DateTime = date) %>%
    mutate(across(c(GPP, GPP_sd, R, R_sd, NEP), ~ round(.x, digits = 3)),
           across(c(DOC_load, TN_load, TP_load), ~ round(.x, digits = 2))) %>%
    arrange(Lake, DateTime)

  write_csv(out, out_file)
  return(out_file)
}



metab_inputs_data_release <- function(
  lake,
  metab_input_dir,
  out_file
){

  metaData<-read.csv('data/metadataLookUp.csv',stringsAsFactor=F)
  alt<-metaData$Altitude..m.[metaData$Lake.Name==lake] #altitude in m
  wnd.z<-metaData$Wind.Height..m.[metaData$Lake.Name==lake] # wind sensor height in m
  lake.area<-metaData$Surface.Area..m2.[metaData$Lake.Name==lake] # lake area in m2
  do.z<-metaData$DO.Sensor.Depth..m.[metaData$Lake.Name==lake] # DO sensor depth in m
  lat<-metaData$Latitude..decimal.degrees.[metaData$Lake.Name==lake] # latitude in decimal degrees

  # load time series metabolism data
  doobs<-load.ts(file.path(metab_input_dir,lake,paste(lake,'_doobs.txt',sep=''))) %>%
    filter(!is.na(datetime))
  wtr<-load.ts(file.path(metab_input_dir,lake,paste(lake,'_wtr.txt',sep=''))) %>%
    filter(!is.na(datetime))
  wnd<-load.ts(file.path(metab_input_dir,lake,paste(lake,'_wnd.txt',sep=''))) %>%
    filter(!is.na(datetime))
  par<-load.ts(file.path(metab_input_dir,lake,paste(lake,'_par.txt',sep=''))) %>%
    filter(!is.na(datetime))
  if(ncol(doobs)>2){
    doobs<-doobs[,which(colnames(doobs)%in%tolower(c('datetime','doobs')))]
  }
  if(ncol(wnd)>2){
    wnd<-wnd[,which(colnames(wnd)%in%tolower(c('datetime','wnd')))]
  }
  timeStep<-as.numeric(diff(doobs$datetime)[1]) # time step difference for doobs

  if(length(grep('_',colnames(wtr[,2:ncol(wtr)])))<(ncol(wtr)-1)){
    colnames(wtr)[2:ncol(wtr)]<-gsub('wtr','wtr_',colnames(wtr)[2:ncol(wtr)])
  }

  # can't have 0 meter temp depth; change to 0.1m if so
  if(0%in%get.offsets(wtr)){
    colnames(wtr)[which(0==get.offsets(wtr))+1]<-'wtr_0.1'
  }

  doobs <- doobs %>%
    arrange(datetime)
  wtr <- wtr %>%
    arrange(datetime)
  wnd <- wnd %>%
    arrange(datetime)
  par <- par %>%
    arrange(datetime)

  # making time step all the same
  #Make all data sets extend from startTime to endTime by timeStep
  #Note that for some lakes it may be necessary to aggregate some variables to coarser time scale to get match up
  # #Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
  if(lake=='Trout'|lake=='Feeagh'){
    timeStep=10 # forcing Trout and Feeagh data to 10 min timestep
    if(lake=='Trout'){
      wtr<-wtr[,!colnames(wtr)=='wtr_0.1'] #Trout shallow temp pendent is consistently lower than 1 meter pendant so it was removed
    }
  }
  if(lake=='Mendota'){
    timeStep=10 # Mendota par and wtr are 60 mins; linearly interpolating below
  }

  doobs$datetime <- floorMins(doobs, timeStep)
  par$datetime <- floorMins(par, timeStep)
  wnd$datetime <- floorMins(wnd, timeStep)
  wtr$datetime <- floorMins(wtr, timeStep)

  #Remove rows with duplicate datetime stamps (and warn)
  notDupRows <- findNotDupRows(doobs)
  doobs <- doobs[notDupRows,]
  notDupRows <- findNotDupRows(par)
  par <- par[notDupRows,]
  notDupRows <- findNotDupRows(wnd)
  wnd <- wnd[notDupRows,]
  notDupRows <- findNotDupRows(wtr)
  wtr <- wtr[notDupRows,] %>%
    select(c(datetime, starts_with("wtr_")))

  doTemp<-data.frame(datetime=wtr$datetime,wtr=rep(NA,length(wtr$datetime))) # temperature at do sensor
  colnames(doTemp)[2]<-paste('wtr',do.z,sep='')
  if(do.z%in%get.offsets(wtr)){
    doTemp<-wtr[,c(1,which(do.z==get.offsets(wtr))+1)]
  }else{ # interpolate temp to do sensor depth if not in wtr file
    for(i in 1:length(wtr$datetime)){
      tryCatch(
        doTemp[i,2]<-approx(get.offsets(wtr),wtr[i,c(2:ncol(wtr))],xout = do.z)$y
        , error=function(e){doTemp[i,2]=wtr[i,2]})
    }
  }

  colnames(doTemp)[2] <- "wtr_doobs"

  # linearly interpolate wind data to fill holes; max 60 min gap
  wnd <- fillHoles(wnd,maxLength=60,timeStep=timeStep)

  out <- left_join(doobs, doTemp) %>%
    left_join(par) %>%
    left_join(wnd) %>%
    left_join(wtr) %>% as_tibble() %>%
    mutate(lake = lake) %>%
    relocate(lake) %>%
    pivot_longer(cols = c(3:ncol(.)),
                 names_to = "variable",
                 values_to = "observation")

  write_csv(out, out_file)
  return(out_file)
}

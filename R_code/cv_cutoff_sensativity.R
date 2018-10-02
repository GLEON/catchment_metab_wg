# sensativity in analysis to changing CV cutoff
library(ggplot2)
library(dplyr)

dir<-'results/metab/20161107/' # directory of metabolism data
folders<-list.files(dir) %>% # folders in this dir
  tbl_df() %>%
  dplyr::filter(!grepl('.doc|Trout', value)) # get rid of README doc; skipping trout for now --> have to do bootstrapping on this still

all_metab <- lapply(folders$value, function(lake){
  cur = read.table(file.path(dir,lake,paste(lake,'_metabEst.txt',sep='')), header=T, sep='\t', stringsAsFactors = F) %>%
    select(1:12) %>%
    mutate(lake = lake,
           date = as.Date(paste(year, doy), format = '%Y %j'))
}) %>% bind_rows()

# season cutoffs used by Jim ; can change if needed
# Spring = (doy < 180)
# Summer = (doy >= 180 & doy <=240)
# Fall = (doy > 240)
all_metab <- all_metab %>%
  mutate(season = case_when(
    doy < 180 & doy > 120 ~ 'spring',
    doy >= 180 & doy <= 240 ~ 'summer',
    doy > 240 & doy < 305 ~ 'fall'),
    GPP_CV = GPP_SD/GPP,
    R_CV = R_SD/-R)

cv_seq <- seq(0, 10, by = 0.25)

cv_sens_out <- tidyr::crossing(all_metab, cv_seq) %>%
  dplyr::filter(!is.na(season)) %>%
  group_by(lake, cv_seq, season) %>%
  mutate(total_obs = n()) %>%
  dplyr::filter((GPP_SD/GPP) < cv_seq,
                (R_SD/-R) < cv_seq) %>%
  summarise(mean_gpp = mean(GPP),
            mean_r = mean(R),
            r_gpp_coeff = summary(lm(R~GPP))$coeff[2],
            r_gpp_int = summary(lm(R~GPP))$coeff[1],
            n_obs = n(),
            total_obs = mean(total_obs)) %>%
  ungroup() %>%
  rename(cv_cutoff = cv_seq) %>%
  mutate(frac_obs = n_obs / total_obs)

cv_sens_out

season_table = cv_sens_out %>%
  group_by(lake, season) %>%
  summarise(obs = mean(total_obs))

windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(cv_sens_out$lake))){ # looping through each lake
  cur<-cv_sens_out %>%
    dplyr::filter(lake == unique(cv_sens_out$lake)[i])

  plot(cur$mean_gpp~cur$cv_cutoff, pch=16, ylab='GPP',xlab='CV Threshold',main=unique(cv_sens_out$lake)[i])
}

windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(cv_sens_out$lake))){ # looping through each lake
  cur<-cv_sens_out %>%
    dplyr::filter(lake == unique(cv_sens_out$lake)[i])

  plot(cur$mean_r~cur$cv_cutoff, pch=16, ylab='R',xlab='CV Threshold',main=unique(cv_sens_out$lake)[i])
}

windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(cv_sens_out$lake))){ # looping through each lake
  cur<-cv_sens_out %>%
    dplyr::filter(lake == unique(cv_sens_out$lake)[i])

  plot(cur$frac_obs~cur$cv_cutoff, pch=16, ylab='Fraction of Total Obs',xlab='CV Threshold',main=unique(cv_sens_out$lake)[i])
}

windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(cv_sens_out$lake))){ # looping through each lake
  cur<-cv_sens_out %>%
    dplyr::filter(lake == unique(cv_sens_out$lake)[i])

  plot(cur$r_gpp_coeff~cur$cv_cutoff, pch=16, ylab='R_GPP_Slope',xlab='CV Threshold',main=unique(cv_sens_out$lake)[i])
}

windows()
par(mfrow=c(4,4)) # how many panels on graph (alternatively we could call a new window in every iteration of for loop)
for(i in 1:length(unique(cv_sens_out$lake))){ # looping through each lake
  cur<-cv_sens_out %>%
    dplyr::filter(lake == unique(cv_sens_out$lake)[i])

  plot(cur$r_gpp_int~cur$cv_cutoff, pch=16, ylab='R_GPP_Int',xlab='CV Threshold',main=unique(cv_sens_out$lake)[i])
}

windows()
par(mfrow=c(4,4))
cols = c('orange','green', 'blue') # fall, spring, summer
for(i in 1:length(unique(cv_sens_out$lake))){ # looping through each lake

  cur<-cv_sens_out %>%
    dplyr::filter(lake == unique(cv_sens_out$lake)[i])

  plot(cur$frac_obs~cur$cv_cutoff, pch=16, ylab='Fraction of Seasonal Obs',xlab='CV Threshold',main=unique(cv_sens_out$lake)[i],
       col = cols[as.factor(cur$season)])
}


windows()
ggplot(season_table, aes(x = lake, y = obs)) +
  geom_bar(aes(fill = season), position = 'dodge', stat = 'identity')

cv_sens_out %>%
  dplyr::filter(cv_cutoff==2) %>%
  select(lake, cv_cutoff, n_obs, total_obs, frac_obs)




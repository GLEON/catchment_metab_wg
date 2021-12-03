# Mendota blip and removing blip comparison
# JAZ; 2019-01-31

library(dplyr)
library(ggplot2)

blip_days = read.csv('data/metab_data/Mendota/blip_detect.csv', stringsAsFactors = F) %>%
  dplyr::mutate(date = as.Date(date, '%m/%d/%Y'),
                doy = lubridate::yday(date)) %>% as_tibble()

no_blip = read.table('results/metab/20161107/Mendota/Mendota_metabEst.txt', stringsAsFactors = F, header = T) %>%
  left_join(blip_days, by = 'doy') %>%
  mutate(blip = 'removed')
blip = read.table('results/metab/20161107/Mendota/Mendota_metabEst_blip_included.txt', stringsAsFactors = F, header = T) %>%
  left_join(blip_days, by = 'doy') %>%
  mutate(blip = 'included')

all = bind_rows(no_blip, blip) %>% as_tibble() %>%
  dplyr::filter(GPP_SD/GPP < 4, R_SD/abs(R) <4)


windows()
ggplot(dplyr::filter(all,blip_T_F), aes(x= blip, y = GPP, group = blip, color = blip))  +
  geom_violin() +
  geom_jitter(aes(size = GPP_SD/GPP), width = .2, alpha = .5) +
  theme_bw()

summary(dplyr::filter(all, blip_T_F, blip == 'removed'))
summary(dplyr::filter(all, blip_T_F, blip == 'included'))


# comparison to Mendota 2013 metab data - when there were better fits
me_13 = read.table('/Users/jzwart/Documents/Jake/MyPapers/GLEON Catchment & Lake Metabolism/metab results/20161107/Mendota/Mendota_metabEst.txt',
                   stringsAsFactors = F, header = T)

summary(dplyr::filter(me_13, GPP_SD/GPP < 4, R_SD/abs(R) < 4))
summary(dplyr::filter(no_blip, GPP_SD/GPP < 4, R_SD/abs(R) < 4))

year_compare = bind_rows(me_13,no_blip)

ggplot(dplyr::filter(year_compare, GPP_SD/GPP < 4, R_SD/abs(R) < 4),
       aes(x = as.factor(year), y = GPP, group = as.factor(year), color = as.factor(year))) +
  geom_violin() +
  geom_jitter(aes(size = GPP_SD/GPP), width = .2, alpha = .5) +
  theme_bw()



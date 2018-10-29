# seasonal cutoff (spring / summer / fall) based on strength of stratification or something similar
# 2018-07-05
#
# Notes:
# Could create two rules that make sense, one for dimictic lakes and one for non-dimictic
#      - Dimictic: spring = unstratified period to a stratification strength period (or a week after stratification starts), fall = when stratification strength falls below threshold
#      - Non-dimictic: Vortsjarv, and Lilli? Could use a top to bottom water temp difference. Or use surface water temperature cutoff
#      - Use surface water temp from dimictic lakes when stratification sets up to apply to non-dimictic lakes for spring / fall cutoff.

################################
library(ggplot2)
library(dplyr)

dir<-'data/buoyancy freq/' # directory of metabolism data
files<-list.files(dir) %>% # folders in this dir
  tbl_df() %>%
  dplyr::filter(!grepl('.doc|Trout', value)) # get rid of README doc; skipping trout for now --> have to do bootstrapping on this still

all <- lapply(files$value, function(file){
  cur = read.table(file.path(dir,file), header=T, sep='\t', stringsAsFactors = F) %>%
    mutate(lake = strsplit(file, '_buo')[[1]][1],
           date = as.Date(datetime))
}) %>% bind_rows()

agg <- all %>%
  group_by(lake) %>%
  mutate(z_n2 = (n2 - mean(n2, na.rm=T)) / sd(n2, na.rm =T)) %>%
  ungroup() %>%
  group_by(lake, date) %>%
  summarise(n2 = mean(n2, na.rm = T),
            z_n2 = mean(z_n2, na.rm = T)) %>%
  ungroup()

n2_cutoff <- 0.004

windows()
ggplot(agg, aes(x = date, y = n2)) +
  geom_point() +
  geom_hline(yintercept = n2_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()

# use z score for n2 cuttoff?
z_n2_cutoff = 0

windows()
ggplot(agg, aes(x = date, y = z_n2)) +
  geom_point() +
  geom_hline(yintercept = z_n2_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()


out <- agg %>%
  group_by(lake) %>%
  mutate(doy = as.numeric(strftime(date, format = '%j', tz ='GMT')),
         spring_end = min(doy[z_n2>z_n2_cutoff], na.rm=T), # earliest date that buoyancy frequency exceeds threshold
         fall_start = min(doy[z_n2<z_n2_cutoff & doy > spring_end + 80], na.rm=T), # earliest date that buoyancy frequency is less than threshold and more than 2 months past start of summer
         season = case_when(
           doy < spring_end & doy > 80 ~ 'spring',
           doy >= spring_end & doy <= fall_start ~ 'summer',
           doy > fall_start & doy < 350 ~ 'fall')) %>%
  ungroup()


windows()
ggplot(out, aes(x = date, y = z_n2, color = season)) +
  geom_point() +
  geom_hline(yintercept = z_n2_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()

# using z-score cutoff:
#   - average fall start is DOY 243, with range of 229 to 275
#   - average spring end is DOY 154, with range of 126 to 185


# using non-z-scored cutoff; it seems like buoyancy frequency is dependent on measurement depths which is why z-score may be better choice
out <- agg %>%
  group_by(lake) %>%
  mutate(doy = as.numeric(strftime(date, format = '%j', tz ='GMT')),
         spring_end = min(doy[n2>n2_cutoff], na.rm=T), # earliest date that buoyancy frequency exceeds threshold
         fall_start = min(doy[n2<n2_cutoff & doy > spring_end + 80], na.rm=T), # earliest date that buoyancy frequency is less than threshold and more than 2 months past start of summer
         season = case_when(
           doy < spring_end & doy > 80 ~ 'spring',
           doy >= spring_end & doy <= fall_start ~ 'summer',
           doy > fall_start & doy < 350 ~ 'fall')) %>%
  ungroup()

# using non-z-score cutoff:
#   - average fall start is DOY 250, with range of 238 to 276
#   - average spring end is DOY 146, with range of 118 to 185

windows()
ggplot(out, aes(x = date, y = n2, color = season)) +
  geom_point() +
  geom_hline(yintercept = n2_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()

##########################################################################
# schmidt stability cutoff

library(ggplot2)
library(dplyr)

dir<-'data/schmidt stability/' # directory of metabolism data
files<-list.files(dir) %>% # folders in this dir
  tbl_df() %>%
  dplyr::filter(!grepl('.doc|Trout', value)) # get rid of README doc; skipping trout for now --> have to do bootstrapping on this still

all <- lapply(files$value, function(file){
  cur = read.table(file.path(dir,file), header=T, sep='\t', stringsAsFactors = F) %>%
    mutate(lake = strsplit(file, '_sch')[[1]][1],
           date = as.Date(datetime))
}) %>% bind_rows()

agg <- all %>%
  dplyr::filter(lubridate::month(date) > 3, lubridate::month(date) < 11) %>% # keeping only April - Oct months
  group_by(lake) %>%
  mutate(z_schmidt.stability = (schmidt.stability - mean(schmidt.stability, na.rm=T)) / sd(schmidt.stability, na.rm =T)) %>%
  ungroup() %>%
  group_by(lake, date) %>%
  summarise(schmidt.stability = mean(schmidt.stability, na.rm = T),
            z_schmidt.stability = mean(z_schmidt.stability, na.rm = T)) %>%
  ungroup()

schmidt_cutoff <- 10

windows()
ggplot(agg, aes(x = date, y = schmidt.stability)) +
  geom_point() +
  geom_hline(yintercept = schmidt_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()

# use z score for schmidt.stability cuttoff?
z_schmidt.stability_cutoff = 0

windows()
ggplot(agg, aes(x = date, y = z_schmidt.stability)) +
  geom_point() +
  geom_hline(yintercept = z_schmidt.stability_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()


out <- agg %>%
  group_by(lake) %>%
  mutate(doy = as.numeric(strftime(date, format = '%j', tz ='GMT')),
         spring_end = min(doy[z_schmidt.stability>z_schmidt.stability_cutoff], na.rm=T), # earliest date that buoyancy frequency exceeds threshold
         fall_start = min(doy[z_schmidt.stability<z_schmidt.stability_cutoff & doy > spring_end + 80], na.rm=T), # earliest date that buoyancy frequency is less than threshold and more than 2 months past start of summer
         season = case_when(
           doy < spring_end & doy > 80 ~ 'spring',
           doy >= spring_end & doy <= fall_start ~ 'summer',
           doy > fall_start & doy < 350 ~ 'fall')) %>%
  ungroup()

saveRDS(out, 'results/z_scored_schmidt.rds')

windows()
ggplot(out, aes(x = date, y = z_schmidt.stability, color = season)) +
  geom_point() +
  geom_hline(yintercept = z_schmidt.stability_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()

# using z-score cutoff:
#   - average fall start is DOY 243, with range of 229 to 275
#   - average spring end is DOY 154, with range of 126 to 185


# using non-z-scored cutoff; it seems like buoyancy frequency is dependent on measurement depths which is why z-score may be better choice
out <- agg %>%
  group_by(lake) %>%
  mutate(doy = as.numeric(strftime(date, format = '%j', tz ='GMT')),
         spring_end = min(doy[schmidt.stability>schmidt.stability_cutoff], na.rm=T), # earliest date that buoyancy frequency exceeds threshold
         fall_start = min(doy[schmidt.stability<schmidt.stability_cutoff & doy > spring_end + 80], na.rm=T), # earliest date that buoyancy frequency is less than threshold and more than 2 months past start of summer
         season = case_when(
           doy < spring_end & doy > 80 ~ 'spring',
           doy >= spring_end & doy <= fall_start ~ 'summer',
           doy > fall_start & doy < 350 ~ 'fall')) %>%
  ungroup()

# using non-z-score cutoff:
#   - average fall start is DOY 250, with range of 238 to 276
#   - average spring end is DOY 146, with range of 118 to 185

windows()
ggplot(out, aes(x = date, y = schmidt.stability, color = season)) +
  geom_point() +
  geom_hline(yintercept = schmidt.stability_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()
###################################
# lake number cuttoff

library(ggplot2)
library(dplyr)

dir<-'data/lake number/' # directory of metabolism data
files<-list.files(dir) %>% # folders in this dir
  tbl_df() %>%
  dplyr::filter(!grepl('.doc|Trout', value)) # get rid of README doc; skipping trout for now --> have to do bootstrapping on this still

all <- lapply(files$value, function(file){
  cur = read.table(file.path(dir,file), header=T, sep='\t', stringsAsFactors = F) %>%
    mutate(lake = strsplit(file, '_sch')[[1]][1],
           date = as.Date(datetime))
}) %>% bind_rows()

agg <- all %>%
  group_by(lake) %>%
  mutate(z_lake.number = (lake.number - mean(lake.number, na.rm=T)) / sd(lake.number, na.rm =T)) %>%
  ungroup() %>%
  group_by(lake, date) %>%
  summarise(lake.number = median(lake.number, na.rm = T),
            z_lake.number = median(z_lake.number, na.rm = T)) %>%
  ungroup()

agg <- all %>%
  group_by(lake, date) %>%
  summarise(lake.number = median(lake.number, na.rm = T)) %>%
  ungroup() %>%
  group_by(lake) %>%
  mutate(z_lake.number = (lake.number - mean(lake.number, na.rm=T)) / sd(lake.number, na.rm =T)) %>%
  ungroup()


lake.number_cutoff <- 1

windows()
ggplot(agg, aes(x = date, y = lake.number)) +
  geom_point() +
  geom_hline(yintercept = lake.number_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()

# use z score for lake.number cuttoff?
z_lake.number_cutoff = 0

windows()
ggplot(agg, aes(x = date, y = z_lake.number)) +
  geom_point() +
  geom_hline(yintercept = z_lake.number_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()


out <- agg %>%
  group_by(lake) %>%
  mutate(doy = as.numeric(strftime(date, format = '%j', tz ='GMT')),
         spring_end = min(doy[z_lake.number>z_lake.number_cutoff], na.rm=T), # earliest date that buoyancy frequency exceeds threshold
         fall_start = min(doy[z_lake.number<z_lake.number_cutoff & doy > spring_end + 80], na.rm=T), # earliest date that buoyancy frequency is less than threshold and more than 2 months past start of summer
         season = case_when(
           doy < spring_end & doy > 80 ~ 'spring',
           doy >= spring_end & doy <= fall_start ~ 'summer',
           doy > fall_start & doy < 350 ~ 'fall')) %>%
  ungroup()


windows()
ggplot(out, aes(x = date, y = z_lake.number, color = season)) +
  geom_point() +
  geom_hline(yintercept = z_lake.number_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()

# using z-score cutoff:
#   - average fall start is DOY 243, with range of 229 to 275
#   - average spring end is DOY 154, with range of 126 to 185


# using non-z-scored cutoff; it seems like buoyancy frequency is dependent on measurement depths which is why z-score may be better choice
out <- agg %>%
  group_by(lake) %>%
  mutate(doy = as.numeric(strftime(date, format = '%j', tz ='GMT')),
         spring_end = min(doy[lake.number>lake.number_cutoff], na.rm=T), # earliest date that buoyancy frequency exceeds threshold
         fall_start = min(doy[lake.number<lake.number_cutoff & doy > spring_end + 80], na.rm=T), # earliest date that buoyancy frequency is less than threshold and more than 2 months past start of summer
         season = case_when(
           doy < spring_end & doy > 80 ~ 'spring',
           doy >= spring_end & doy <= fall_start ~ 'summer',
           doy > fall_start & doy < 350 ~ 'fall')) %>%
  ungroup()

# using non-z-score cutoff:
#   - average fall start is DOY 250, with range of 238 to 276
#   - average spring end is DOY 146, with range of 118 to 185

windows()
ggplot(out, aes(x = date, y = lake.number, color = season)) +
  geom_point() +
  geom_hline(yintercept = lake.number_cutoff) +
  facet_wrap(~lake, scales = 'free') +
  theme_classic()



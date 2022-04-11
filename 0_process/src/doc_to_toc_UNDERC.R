library(tidyverse)

# DOC to TOC for UNDERC streams


fpath = '0_process/in/7438598/'
db_name = 'MFEdb_20210423.db'

source('0_process/src/dbTable.R')
source('0_process/src/dbTableList.R')

dbTableList(fpath = fpath, dbname = db_name)

lake_geom = dbTable(table = 'lakes',
                    fpath = fpath, dbname = db_name)
temp = dbTable(table = 'limno_profiles',
               fpath = fpath, dbname = db_name)

lakes = c('EL', 'MO', 'CR')
streams = c('DeepHole','Inlet1', 'Inlet2','Inlet3')
parameters = c('DOC', 'POC')

data = dbTable(table = 'water_chem', fpath = fpath,
               dbname = db_name, lakeID = lakes)

data$site = str_extract(data$sampleID, '_[^_]*_') %>% gsub('_','',.)

stream_sites = data %>%
  filter(site %in% streams,
         parameter %in% parameters,
         flag == '0') %>%
  tibble() %>%
  mutate(dateSample = as.Date(dateSample)) %>%
  select(lakeID,site,dateSample,parameter, parameterValue) %>%
  group_by(lakeID,site,dateSample,parameter) %>%
  summarise(parameterValue = mean(as.numeric(parameterValue)), .groups = 'drop') %>%
  pivot_wider(id_cols = c('lakeID','site','dateSample'),
              names_from = 'parameter', values_from = 'parameterValue') %>%
  na.omit() %>%
  mutate(doc_to_toc = DOC / (DOC + POC))

ggplot(data = stream_sites)+
  geom_boxplot(aes(x = lakeID, y = doc_to_toc, group = lakeID))

agg = stream_sites %>%
  group_by(lakeID, site) %>%
  summarise(doc_to_toc = mean(doc_to_toc), .groups = 'drop')


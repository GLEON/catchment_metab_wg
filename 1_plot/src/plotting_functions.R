
plot_metab_timeseries <- function(
  metab_data,
  season_data,
  config,
  out_file
){

  metab_data = filter_metab_timeseries(metab_data = metab_data,
                                       season_data = season_data,
                                       config = config)

  #ordering by mean GPP
  lakes_sorted <- metab_data$lake[sort.list(metab_data$mean_gpp)]
  lakes_sorted <- as.character(lakes_sorted[!duplicated(lakes_sorted)])
  seasons_sorted <- c('spring','summer','fall')

  metab_data$lake <- factor(metab_data$lake,levels = lakes_sorted)
  metab_data$season <- factor(metab_data$season, levels = seasons_sorted)

  # facet labeller
  lake_names <- c('Acton' = 'Acton Lake',
                  'Crampton' = 'Crampton Lake',
                  'EastLong' = 'East Long Lake',
                  'Feeagh' = 'Lough Feeagh',
                  'Harp' = 'Harp Lake',
                  'Langtjern' = 'Lake Langtjern',
                  'Lillinonah' = 'Lake Lillinonah',
                  'Lillsjoliden' = 'Lillsjölidtjärnen',
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

  # keeping x and y axis scales the same for every plot
  linesize = .5
  pointsize = .9
  line_alpha = .3
  metab <- ggplot(metab_data,
                  aes(x = plot_date, y = GPP), color = "black") +
    geom_line(size = linesize, alpha = line_alpha,
              color = "#40B0A6", linetype = "dashed") +
    geom_point(size = pointsize, color = "#40B0A6") +
    geom_line(data = metab_data,
              aes( x= plot_date, y = R), size = linesize, alpha = line_alpha,
              color = "#E1BE6A", linetype = "dashed") +
    geom_point(data = metab_data,
               aes( x= plot_date, y = R), size = pointsize,
               color = "#E1BE6A") +
    facet_wrap(~lake,labeller = as_labeller(lake_names), strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12),
          panel.spacing.x = unit(1, "lines")) +
    ylab(expression(Metabolism~(mg~O[2]~L^-1~day^-1))) +
    scale_x_date(limits = c(as.Date("2001-01-01"), as.Date("2001-12-31")), date_labels = '%b') +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey')

  ggsave(out_file,
         plot = metab,
         width = 10,
         height = 10)
}


plot_doc_timeseries <- function(
    load_data,
    inlake_data,
    config,
    out_file
){

  load_data = left_join(load_data,
                        inlake_data,
                        by = c('lake' = 'lake', 'date' = 'date'))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  load_data <- dplyr::filter(load_data, doy > min_doy, doy < max_doy) %>%
    group_by(lake) %>%
    dplyr::mutate(mean_tp = mean(TP_load / Volume..m3., na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(lake = factor(lake),
                  season = factor(season),
                  plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'),
                  TP_load = ifelse(TP_load == 0, NA, TP_load),
                  TP_load = TP_load / Volume..m3., # kg P m-3 day-1
                  TN_load = TN_load / Volume..m3., # kg N m-3 day-1
                  DOC_load = DOC_load / Volume..m3., # kg C m-3 day-1
                  TP = TP / 1000 / 31, # converting to mol/m3
                  TN = TN / 1000 / 14,
                  DOC = DOC / 12)

  #ordering by mean gpp
  lakes_sorted <- load_data$lake[sort.list(load_data$mean_gpp)]
  lakes_sorted <- as.character(lakes_sorted[!duplicated(lakes_sorted)])
  seasons_sorted <- c('spring','summer','fall')

  load_data$lake <- factor(load_data$lake,levels = lakes_sorted)
  load_data$season <- factor(load_data$season, levels = seasons_sorted)

  # facet labeller
  lake_names <- c('Acton' = 'Acton Lake',
                  'Crampton' = 'Crampton Lake',
                  'EastLong' = 'East Long Lake',
                  'Feeagh' = 'Lough Feeagh',
                  'Harp' = 'Harp Lake',
                  'Langtjern' = 'Lake Langtjern',
                  'Lillinonah' = 'Lake Lillinonah',
                  'Lillsjoliden' = 'Lillsjölidtjärnen',
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

  # keeping x and y axis scales the same for every plot
  doc_load <- ggplot(load_data, aes(x = plot_date, y = DOC_load*1000*1000, group = lake)) +
    geom_line(color = "black", size = 1) +
    facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12),
          panel.spacing.x = unit(1, "lines")) +
    ylab(expression(DOC~Load~(mg~C~(m^3~lake~water)^-1~day^-1))) +
    scale_y_log10() +
    scale_x_date(date_labels = '%b')

  ggsave(out_file,
         plot = doc_load,
         width = 10, height = 10)
}

plot_tn_timeseries <- function(
    load_data,
    inlake_data,
    config,
    out_file
){

  load_data = left_join(load_data,
                        inlake_data,
                        by = c('lake' = 'lake', 'date' = 'date'))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  load_data <- dplyr::filter(load_data, doy > min_doy, doy < max_doy) %>%
    group_by(lake) %>%
    dplyr::mutate(mean_tp = mean(TP_load / Volume..m3., na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(lake = factor(lake),
                  season = factor(season),
                  plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'),
                  TP_load = ifelse(TP_load == 0, NA, TP_load),
                  TP_load = TP_load / Volume..m3., # kg P m-3 day-1
                  TN_load = TN_load / Volume..m3., # kg N m-3 day-1
                  DOC_load = DOC_load / Volume..m3., # kg C m-3 day-1
                  TP = TP / 1000 / 31, # converting to mol/m3
                  TN = TN / 1000 / 14,
                  DOC = DOC / 12)

  #ordering by mean gpp
  lakes_sorted <- load_data$lake[sort.list(load_data$mean_gpp)]
  lakes_sorted <- as.character(lakes_sorted[!duplicated(lakes_sorted)])
  seasons_sorted <- c('spring','summer','fall')

  load_data$lake <- factor(load_data$lake,levels = lakes_sorted)
  load_data$season <- factor(load_data$season, levels = seasons_sorted)

  # facet labeller
  lake_names <- c('Acton' = 'Acton Lake',
                  'Crampton' = 'Crampton Lake',
                  'EastLong' = 'East Long Lake',
                  'Feeagh' = 'Lough Feeagh',
                  'Harp' = 'Harp Lake',
                  'Langtjern' = 'Lake Langtjern',
                  'Lillinonah' = 'Lake Lillinonah',
                  'Lillsjoliden' = 'Lillsjölidtjärnen',
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

  # keeping x and y axis scales the same for every plot
  tn_load <- ggplot(load_data, aes(x = plot_date, y = TN_load*1000*1000*1000, group = lake)) +
    geom_line(color = "black", size = 1) +
    facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12),
          panel.spacing.x = unit(1, "lines")) +
    ylab(expression(TN~Load~(mu*g~N~(m^3~lake~water)^-1~day^-1))) +
    scale_y_log10() +
    scale_x_date(date_labels = '%b')

  ggsave(out_file,
         plot = tn_load,
         width = 10, height = 10)
}


plot_tp_timeseries <- function(
    load_data,
    inlake_data,
    config,
    out_file
){

  load_data = left_join(load_data,
                        inlake_data,
                        by = c('lake' = 'lake', 'date' = 'date'))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  load_data <- dplyr::filter(load_data, doy > min_doy, doy < max_doy) %>%
    group_by(lake) %>%
    dplyr::mutate(mean_tp = mean(TP_load / Volume..m3., na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(lake = factor(lake),
                  season = factor(season),
                  plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'),
                  TP_load = ifelse(TP_load == 0, NA, TP_load),
                  TP_load = TP_load / Volume..m3., # kg P m-3 day-1
                  TN_load = TN_load / Volume..m3., # kg N m-3 day-1
                  DOC_load = DOC_load / Volume..m3., # kg C m-3 day-1
                  TP = TP / 1000 / 31, # converting to mol/m3
                  TN = TN / 1000 / 14,
                  DOC = DOC / 12)

  #ordering by mean gpp
  lakes_sorted <- load_data$lake[sort.list(load_data$mean_gpp)]
  lakes_sorted <- as.character(lakes_sorted[!duplicated(lakes_sorted)])
  seasons_sorted <- c('spring','summer','fall')

  load_data$lake <- factor(load_data$lake,levels = lakes_sorted)
  load_data$season <- factor(load_data$season, levels = seasons_sorted)

  # facet labeller
  lake_names <- c('Acton' = 'Acton Lake',
                  'Crampton' = 'Crampton Lake',
                  'EastLong' = 'East Long Lake',
                  'Feeagh' = 'Lough Feeagh',
                  'Harp' = 'Harp Lake',
                  'Langtjern' = 'Lake Langtjern',
                  'Lillinonah' = 'Lake Lillinonah',
                  'Lillsjoliden' = 'Lillsjölidtjärnen',
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

  # keeping x and y axis scales the same for every plot
  tp_load <- ggplot(load_data, aes(x = plot_date, y = TP_load*1000*1000*1000, group = lake)) +
    geom_line(color = "black", size = 1) +
    facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12),
          panel.spacing.x = unit(1, "lines")) +
    ylab(expression(TP~Load~(mu*g~P~(m^3~lake~water)^-1~day^-1))) +
    scale_y_log10() +
    scale_x_date(date_labels = '%b')

  ggsave(out_file,
         plot = tp_load,
         width = 10, height = 10)
}


plot_stoich_timeseries <- function(
  load_data,
  inlake_data,
  config,
  out_file
){

  load_data = left_join(load_data,
                        inlake_data,
                        by = c('lake' = 'lake', 'date' = 'date'))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  load_data <- dplyr::filter(load_data, doy > min_doy, doy < max_doy) %>%
    group_by(lake) %>%
    dplyr::mutate(mean_tp = mean(TP_load / Volume..m3., na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(lake = factor(lake),
                  season = factor(season),
                  plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'),
                  TP_load = ifelse(TP_load == 0, NA, TP_load),
                  TP_load = TP_load / 31, # changing to mol for stoichiometry plot
                  TN_load = TN_load / 14,
                  DOC_load = DOC_load / 12,
                  TP = TP / 1000 / 31, # converting to mol/m3
                  TN = TN / 1000 / 14,
                  DOC = DOC / 12)

  #ordering by mean gpp
  lakes_sorted <- load_data$lake[sort.list(load_data$mean_gpp)]
  lakes_sorted <- as.character(lakes_sorted[!duplicated(lakes_sorted)])
  seasons_sorted <- c('spring','summer','fall')

  load_data$lake <- factor(load_data$lake,levels = lakes_sorted)
  load_data$season <- factor(load_data$season, levels = seasons_sorted)

  # facet labeller
  lake_names <- c('Acton' = 'Acton Lake',
                  'Crampton' = 'Crampton Lake',
                  'EastLong' = 'East Long Lake',
                  'Feeagh' = 'Lough Feeagh',
                  'Harp' = 'Harp Lake',
                  'Langtjern' = 'Lake Langtjern',
                  'Lillinonah' = 'Lake Lillinonah',
                  'Lillsjoliden' = 'Lillsjölidtjärnen',
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

  # keeping x and y axis scales the same for every plot
  # load_stoich <- ggplot(load_data, aes(x = plot_date, y = DOC_load/TP_load, group = lake)) +
  #   geom_line(aes(color = 'a'), size = 1) +
  #   geom_line(data = load_data, aes(x = plot_date, y = TN_load / TP_load, group = lake, color = 'b'), size = 1)+
  #   geom_line(data = load_data, aes(x = plot_date, y = DOC_load / TN_load, group = lake, color = 'c'), size = 1)+
  #   facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
  #   theme_classic() +
  #   theme(strip.background = element_blank(),
  #         strip.placement = 'inside',
  #         axis.title = element_text(size = 16),
  #         axis.text = element_text(size = 12),
  #         axis.title.x = element_blank(),
  #         legend.title = element_blank(),
  #         legend.text = element_text(size =12)) +
  #   scale_color_manual(name = '',
  #                      values = c('a' = 'black',
  #                                 'b' = '#CC79A7',
  #                                 'c' = '#D55E00'),
  #                      labels = c('C:P', 'N:P', 'C:N')) +
  #   ylab(expression(Load~Stoichiometry~(mol:mol))) +
  #   scale_y_log10() +
  #   geom_hline(yintercept = 106, color = 'black', linetype = 'dashed')+ # redfield ratios
  #   geom_hline(yintercept = 16, color ='#CC79A7', linetype = 'dashed')+
  #   geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed')+
  #   scale_x_date(date_labels = '%b')
  #
  #
  # in_lake_stoich = ggplot(load_data, aes(x = plot_date, y = DOC/TP, group = lake)) +
  #   geom_point(aes(color = 'a'), size = 1) +
  #   geom_point(data = load_data, aes(x = plot_date, y = TN / TP, group = lake, color = 'b'), size = 1)+
  #   geom_point(data = load_data, aes(x = plot_date, y = DOC / TN, group = lake, color = 'c'), size = 1)+
  #   facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
  #   theme_classic() +
  #   theme(strip.background = element_blank(),
  #         strip.placement = 'inside',
  #         axis.title = element_text(size = 16),
  #         axis.text = element_text(size = 12),
  #         axis.title.x = element_blank(),
  #         legend.title = element_blank(),
  #         legend.text = element_text(size =12)) +
  #   scale_color_manual(name = '',
  #                      values = c('a' = 'black',
  #                                 'b' = '#CC79A7',
  #                                 'c' = '#D55E00'),
  #                      labels = c('C:P', 'N:P', 'C:N')) +
  #   ylab(expression(Lake~Stoichiometry~(mol:mol))) +
  #   scale_y_log10() +
  #   geom_hline(yintercept = 106, color = 'black', linetype = 'dashed')+ # redfield ratios
  #   geom_hline(yintercept = 16, color ='#CC79A7', linetype = 'dashed')+
  #   geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed')+
  #   scale_x_date(date_labels = '%b')

  lake_load_stoich <- ggplot(load_data,
                             aes(x = plot_date, y = DOC_load/TP_load, group = lake)) +
    geom_line(aes(color = 'a'),
              size = 0.9, alpha = 0.6) +
    geom_hline(yintercept = 106,
               color = 'black',
               linetype = 'dashed',
               alpha = 0.3)+ # redfield ratios
    geom_hline(yintercept = 16,
               color ='#CC79A7',
               linetype = 'dashed',
               alpha = 0.3)+
    geom_hline(yintercept = 6.6,
               color = '#D55E00',
               linetype = 'dashed',
               alpha = 0.3) +
    geom_line(data = load_data,
              aes(x = plot_date, y = TN_load / TP_load, group = lake, color = 'b'),
              size = .9, alpha = 0.6)+
    geom_line(data = load_data,
              aes(x = plot_date, y = DOC_load / TN_load, group = lake, color = 'c'),
              size = .9, alpha = 0.6)+
    geom_point(data = load_data,
               aes(x = plot_date, y = DOC / TP, group = lake, color = 'a'),
               size = 1)+
    geom_point(data = load_data,
               aes(x = plot_date, y = TN / TP, group = lake, color = 'b'),
               size = 1)+
    geom_point(data = load_data,
               aes(x = plot_date, y = DOC / TN, group = lake, color = 'c'),
               size = 1)+
    facet_wrap(~lake,
               labeller = as_labeller(lake_names),
               strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12),
          panel.spacing.x = unit(1, "lines")) +
    scale_color_manual(name = '',
                       values = c('a' = 'black',
                                  'b' = '#CC79A7',
                                  'c' = '#D55E00'),
                       labels = c('C:P', 'N:P', 'C:N')) +
    ylab(expression(Load~or~Lake~Stoichiometry~(mol:mol))) +
    scale_y_log10() +
    scale_x_date(date_labels = '%b')

  ggsave(out_file,
         plot = lake_load_stoich,
         width = 10, height = 10)
}


plot_stream_lake_nutrient <- function(
  load_data,
  inlake_data,
  config,
  label_id,
  out_file
){

  all_load = left_join(load_data,
                       inlake_data,
                       by = c('lake' = 'lake', 'date' = 'date'))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  # Units for nutrient load files = kg/day for TP, TN, DOC and m3/s for inflow
  all_load <- all_load %>%
    mutate(ave_tp_conc_load_mol_m3 = TP_load / 31 * 1000 / (inflow * 86400),
           ave_tn_conc_load_mol_m3 = TN_load / 14 * 1000 / (inflow * 86400),
           ave_doc_conc_load_mol_m3 = DOC_load / 12 * 1000 / (inflow * 86400))

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
                     mean_lake_tp = mean(TP, na.rm = T),
                     mean_lake_tn = mean(TN, na.rm = T),
                     mean_lake_doc = mean(DOC, na.rm = T),
                     WRT = mean(Lake.Residence.Time..year., na.rm = T),
                     vol = mean(Volume..m3., na.rm = T),
                     area = mean(Surface.Area..m2., na.rm = T),
                     basin_area = mean(Catchment.Area..km2., na.rm = T) * 1000 * 1000,
                     mean_z = vol / area,
                     drainage_ratio = basin_area / area,
                     .groups = 'drop') %>%
    left_join(select(label_id, -mean_gpp), by = "lake")

  doc_mod <- summary(lm(log10(summary_df$mean_lake_doc) ~
                          log10(summary_df$mean_doc_conc_load_mg_L)))
  doc_pval = round(doc_mod$coefficients[8], digits = 2)
  doc_r2 = round(doc_mod$r.squared, digits = 2)

  doc <- ggplot(summary_df, aes(y = mean_lake_doc, x = mean_doc_conc_load_mg_L)) +
    geom_point(size = 5, alpha = 0.4) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~DOC~(mg~L^-1))) +
    xlab(expression(Stream~DOC~(mg~L^-1))) +
    geom_line(stat = "smooth", size = 1, method = 'lm',
              color = 'black', se = F, alpha = 0.5) +
    annotate(geom = 'text',
             x = 7, y = 20,
             label = paste('p-val < 0.01','\n','R2:', doc_r2),
             size = 6) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
    annotate(geom = 'text',
             x = 7.5, y = 10, size = 6,
             label = '1:1') +
    ggrepel::geom_text_repel(aes(label=label_id))  +
    scale_y_log10() + scale_x_log10()

  tn_mod <- summary(lm(log10(summary_df$mean_lake_tn) ~
                          log10(summary_df$mean_tn_conc_load_ug_L)))
  tn_pval = round(tn_mod$coefficients[8], digits = 2)
  tn_r2 = round(tn_mod$r.squared, digits = 2)

  tn <- ggplot(summary_df, aes(y = mean_lake_tn, x = mean_tn_conc_load_ug_L)) +
    geom_point(size = 5, alpha = 0.4) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~TN~(mu*g~L^-1))) +
    xlab(expression(Stream~TN~(mu*g~L^-1))) +
    geom_line(stat = "smooth", size = 1, method = 'lm',
              color = 'black', se = F, alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
    annotate(geom = 'text',
             x = 300, y = 3000,
             label = paste('p-val < 0.01','\n','R2:', tn_r2),
             size = 6) +
    annotate(geom = 'text',
             x = 2000, y = 3000, size = 6,
             label = '1:1') +
    ggrepel::geom_text_repel(aes(label=label_id))  +
    scale_y_log10() + scale_x_log10()

  tp_mod <- summary(lm(log10(summary_df$mean_lake_tp) ~
                         log10(summary_df$mean_tp_conc_load_ug_L)))
  tp_pval = round(tp_mod$coefficients[8], digits = 2)
  tp_r2 = round(tp_mod$r.squared, digits = 2)

  tp <- ggplot(summary_df, aes(y = mean_lake_tp, x = mean_tp_conc_load_ug_L)) +
    geom_point(size = 5, alpha = 0.4) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~TP~(mu*g~L^-1))) +
    xlab(expression(Stream~TP~(mu*g~L^-1)))+
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
    geom_line(stat = "smooth", size = 1, method = 'lm',
              color = 'black', se = F, alpha = 0.5) +
    annotate(geom = 'text',
             x = 15, y = 70,
             label = paste('p-val < 0.01','\n','R2:', tp_r2),
             size = 6) +
    annotate(geom = 'text',
             x = 50, y = 70, size = 6,
             label = '1:1') +
    ggrepel::geom_text_repel(aes(label=label_id))  +
    scale_y_log10() + scale_x_log10()

  g = cowplot::plot_grid(doc, tn, tp,
                labels = c('A', 'B', 'C'), align = 'hv',nrow = 1)

  ggsave(out_file,
         plot = g,
         width = 12, height = 4)
  return(out_file)
}


plot_stream_lake_stoich_scatter <- function(
  load_data,
  inlake_data,
  config,
  label_id,
  out_file
){

  all_load = left_join(load_data,
                       inlake_data,
                       by = c('lake' = 'lake', 'date' = 'date'))

  # Units for nutrient load files = kg/day for TP, TN, DOC and m3/s for inflow
  all_load <- all_load %>%
    mutate(ave_tp_conc_load_mol_m3 = TP_load / 31 * 1000 / (inflow * 86400),
           ave_tn_conc_load_mol_m3 = TN_load / 14 * 1000 / (inflow * 86400),
           ave_doc_conc_load_mol_m3 = DOC_load / 12 * 1000 / (inflow * 86400))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

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
                     mean_lake_doc = mean(DOC, na.rm = T),
                     mean_lake_tn = mean(TN, na.rm = T),
                     mean_lake_tp = mean(TP, na.rm = T),
                     .groups = 'drop') %>%
    left_join(select(label_id, -mean_gpp), by = "lake")

  # facet labeller
  lake_names <- c('Acton' = 'Acton Lake',
                  'Crampton' = 'Crampton Lake',
                  'EastLong' = 'East Long Lake',
                  'Feeagh' = 'Lough Feeagh',
                  'Harp' = 'Harp Lake',
                  'Langtjern' = 'Lake Langtjern',
                  'Lillinonah' = 'Lake Lillinonah',
                  'Lillsjoliden' = 'Lillsjölidtjärnen',
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

  doc_tp_mod <- summary(lm(log10(load_plot$mean_doc_load *1000*1000) ~
                             log10(load_plot$mean_tp_load *1000*1000*1000)))
  doc_tp_pval = round(doc_tp_mod$coefficients[8], digits = 2)
  doc_tp_r2 = round(doc_tp_mod$r.squared, digits = 2)

  doc_tp <- ggplot(load_plot, aes(y = mean_doc_load *1000*1000, x = mean_tp_load *1000*1000*1000)) +
    geom_line(data = redfield_line(ratio = 'c_p',
                                   x_axis_element = 'p',
                                   x_axis_range = range(load_plot$mean_tp_load *1000*1000*1000, na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, alpha = 0.5) +
    annotate(geom = 'text',
             x = 1000, y = 1,
             label = paste('p-val < 0.01','\n','R2:', doc_tp_r2),
             size = 4.5) +
    annotate(geom = 'text',
             y = 10, x = 420, angle = 42, size = 4.5,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(DOC~Load~(mg~C~(m^3~lake~water)^-1~day^-1))) +
    xlab(expression(TP~Load~(mu*g~P~(m^3~lake~water)^-1~day^-1)))+ scale_y_log10() + scale_x_log10()+
    geom_line(stat = "smooth", method = 'lm', size = 1,
              color = 'black', se = F, alpha = 0.5) +
    ggrepel::geom_text_repel(aes(label=label_id))

  doc_tn_mod <- summary(lm(log10(load_plot$mean_doc_load *1000*1000) ~
                             log10(load_plot$mean_tn_load *1000*1000*1000)))
  doc_tn_pval = round(doc_tn_mod$coefficients[8], digits = 2)
  doc_tn_r2 = round(doc_tn_mod$r.squared, digits = 2)

  doc_tn <- ggplot(load_plot, aes(y = mean_doc_load *1000*1000, x = mean_tn_load *1000*1000*1000)) +
    # geom_abline(slope = (16*14)/(106*12), intercept = 0, linetype = 'dashed') +
    geom_line(data = redfield_line(ratio = 'c_n',
                                   x_axis_element = 'n',
                                   x_axis_range = range(load_plot$mean_tn_load *1000*1000*1000,
                                                        na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, alpha = 0.5, color = '#D55E00') +
    annotate(geom = 'text',
             x = 30000, y = 0.8,
             label = paste('p-val < 0.01','\n','R2:', doc_tn_r2),
             size = 4.5) +
    annotate(geom = 'text',
             y = 1.5, x = 500, angle = 46, size = 4.5,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(DOC~Load~(mg~C~(m^3~lake~water)^-1~day^-1))) +
    xlab(expression(TN~Load~(mu*g~N~(m^3~lake~water)^-1~day^-1))) + scale_y_log10() + scale_x_log10()+
    geom_line(stat = "smooth", method = 'lm', size = 1,
              color = '#D55E00', se = F, alpha = 0.5) +
    ggrepel::geom_text_repel(aes(label=label_id))

  tn_tp_mod <- summary(lm(log10(load_plot$mean_tn_load *1000*1000*1000) ~
                             log10(load_plot$mean_tp_load *1000*1000*1000)))
  tn_tp_pval = round(tn_tp_mod$coefficients[8], digits = 2)
  tn_tp_r2 = round(tn_tp_mod$r.squared, digits = 2)

  tn_tp <- ggplot(load_plot, aes(y = mean_tn_load *1000*1000*1000,
                                 x = mean_tp_load *1000*1000*1000)) +
    # geom_abline(slope = (1*31)/(16*14), intercept = 0, linetype = 'dashed') +
    geom_line(data = redfield_line(ratio = 'n_p',
                                   x_axis_element = 'p',
                                   x_axis_range = range(load_plot$mean_tp_load *1000*1000*1000,
                                                        na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, alpha = 0.5, color ='#CC79A7') +
    annotate(geom = 'text',
             x = 1000, y = 100,
             label = paste('p-val < 0.01','\n','R2:', tn_tp_r2),
             size = 4.5) +
    annotate(geom = 'text',
             y = 1500, x = 400, angle = 42, size = 4.5,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(TN~Load~(mu*g~N~(m^3~lake~water)^-1~day^-1))) +
    xlab(expression(TP~Load~(mu*g~P~(m^3~lake~water)^-1~day^-1))) +
    scale_y_log10() + scale_x_log10()+
    # geom_smooth(method = 'lm', color = '#CC79A7', se = F, alpha = 0.5) +
    geom_line(stat = "smooth", method = "lm",
              size = 1, alpha = 0.5, color = '#CC79A7') +
    ggrepel::geom_text_repel(aes(label=label_id))

  lake_doc_tp_mod <- summary(lm(log10(load_plot$mean_lake_doc) ~
                                  log10(load_plot$mean_lake_tp)))
  lake_doc_tp_pval = round(lake_doc_tp_mod$coefficients[8], digits = 2)
  lake_doc_tp_r2 = round(lake_doc_tp_mod$r.squared, digits = 2)

  lake_doc_tp <- ggplot(load_plot, aes(y = mean_lake_doc, x = mean_lake_tp)) +
    geom_line(data = redfield_line(ratio = 'c_p',
                                   x_axis_element = 'p',
                                   x_axis_range = range(load_plot$mean_lake_tp,
                                                        na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, alpha = 0.5) +
    annotate(geom = 'text',
             x = 50, y = 0.3,
             label = paste('p-val:', lake_doc_tp_pval,'\n','R2:', lake_doc_tp_r2),
             size = 4.5) +
    annotate(geom = 'text',
             y = 0.9, x = 30, angle = 36.5, size = 4.5,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~DOC~(mg~C~L^-1))) +
    xlab(expression(Lake~TP~(mu*g~P~L^-1))) +
    scale_y_log10() + scale_x_log10()+
    geom_line(stat = "smooth", size = 1, method = 'lm', linetype = "dashed",
              color = 'black', se = F, alpha = 0.5) +
    ggrepel::geom_text_repel(aes(label=label_id))

  lake_doc_tn_mod <- summary(lm(log10(load_plot$mean_lake_doc) ~
                                  log10(load_plot$mean_lake_tn)))
  lake_doc_tn_pval = round(lake_doc_tn_mod$coefficients[8], digits = 2)
  lake_doc_tn_r2 = round(lake_doc_tn_mod$r.squared, digits = 2)

  lake_doc_tn <- ggplot(load_plot, aes(y = mean_lake_doc, x = mean_lake_tn)) +
    geom_line(data = redfield_line(ratio = 'c_n',
                                   x_axis_element = 'n',
                                   x_axis_range = range(load_plot$mean_lake_tn,
                                                        na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, alpha = 0.5, color = '#D55E00') +
    annotate(geom = 'text',
             x = 2000, y = 1.6,
             label = paste('p-val:', lake_doc_tn_pval,'\n','R2:', lake_doc_tn_r2),
             size = 4.5) +
    annotate(geom = 'text',
             y = 2.3, x = 500, angle = 47, size = 4.5,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~DOC~(mg~C~L^-1))) +
    xlab(expression(Lake~TN~(mu*g~N~L^-1)))  +
    scale_y_log10() + scale_x_log10()+
    geom_line(stat = "smooth", size = 1, method = 'lm', linetype = "dashed",
              color = '#D55E00', se = F, alpha = 0.5) +
    ggrepel::geom_text_repel(aes(label=label_id))

  lake_tn_tp_mod <- summary(lm(log10(load_plot$mean_lake_tn) ~
                                  log10(load_plot$mean_lake_tp)))
  lake_tn_tp_pval = round(lake_tn_tp_mod$coefficients[8], digits = 2)
  lake_tn_tp_r2 = round(lake_tn_tp_mod$r.squared, digits = 2)

  lake_tn_tp <- ggplot(load_plot, aes(y = mean_lake_tn, x = mean_lake_tp)) +
    geom_line(data = redfield_line(ratio = 'n_p',
                                   x_axis_element = 'p',
                                   x_axis_range = range(load_plot$mean_lake_tp, na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, alpha = 0.5, color ='#CC79A7') +
    annotate(geom = 'text',
             x = 50, y = 50,
             label = paste('p-val < 0.01', '\n','R2:', lake_tn_tp_r2),
             size = 4.5) +
    annotate(geom = 'text',
             y = 150, x = 30, angle = 35, size = 4.5,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~TN~(mu*g~N~L^-1))) +
    xlab(expression(Lake~TP~(mu*g~P~L^-1))) +
    scale_y_log10() + scale_x_log10()+
    geom_line(stat = "smooth", size = 1, method = 'lm',
              color = '#CC79A7', se = F, alpha = 0.5) +
    ggrepel::geom_text_repel(aes(label=label_id))

  g = cowplot::plot_grid(doc_tn, doc_tp, tn_tp, lake_doc_tn, lake_doc_tp, lake_tn_tp,
                labels = c('A', 'B', 'C', 'D', 'E', 'F'), align = 'hv', nrow = 2)

  ggsave(out_file,
         plot = g,
         width = 12, height = 8)
  return(out_file)
}


plot_stoich_stream_vs_lake <- function(
  load_data,
  inlake_data,
  config,
  label_id,
  out_file
){

  all_load = left_join(load_data,
                       inlake_data,
                       by = c('lake' = 'lake', 'date' = 'date'))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  load_plot <- dplyr::filter(all_load, doy > min_doy, doy < max_doy) %>%
    group_by(lake) %>%
    dplyr::mutate(mean_tp = mean(TP_load / Volume..m3., na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(lake = factor(lake),
                  season = factor(season),
                  plot_date = as.Date(paste('2001-',doy,sep=''), format = '%Y-%j', tz ='GMT'),
                  TP_load = ifelse(TP_load == 0, NA, TP_load),
                  TP_load = TP_load / 31, # changing to mol for stoichiometry plot
                  TN_load = TN_load / 14,
                  DOC_load = DOC_load / 12,
                  TP = TP / 1000 / 31, # converting to mol/m3
                  TN = TN / 1000 / 14,
                  DOC = DOC / 12) %>%
    group_by(lake) %>%
    dplyr::summarise(mean_c_p_load = mean(log(DOC_load/TP_load), na.rm = T),
                     mean_c_n_load = mean(log(DOC_load/TN_load), na.rm = T),
                     mean_n_p_load = mean(log(TN_load/TP_load), na.rm = T),
                     max_c_p_load = max(log(DOC_load/TP_load), na.rm = T),
                     max_c_n_load = max(log(DOC_load/TN_load), na.rm = T),
                     max_n_p_load = max(log(TN_load/TP_load), na.rm = T),
                     min_c_p_load = min(log(DOC_load/TP_load), na.rm = T),
                     min_c_n_load = min(log(DOC_load/TN_load), na.rm = T),
                     min_n_p_load = min(log(TN_load/TP_load), na.rm = T),
                     mean_c_p = mean(log(DOC/TP), na.rm = T),
                     mean_c_n = mean(log(DOC/TN), na.rm = T),
                     mean_n_p = mean(log(TN/TP), na.rm = T),
                     max_c_p = max(log(DOC/TP), na.rm = T),
                     max_c_n = max(log(DOC/TN), na.rm = T),
                     max_n_p = max(log(TN/TP), na.rm = T),
                     min_c_p = min(log(DOC/TP), na.rm = T),
                     min_c_n = min(log(DOC/TN), na.rm = T),
                     min_n_p = min(log(TN/TP), na.rm = T),
                     .groups = 'drop') %>%
    left_join(select(label_id, -mean_gpp), by = "lake")

  # facet labeller
  lake_names <- c('Acton' = 'Acton Lake',
                  'Crampton' = 'Crampton Lake',
                  'EastLong' = 'East Long Lake',
                  'Feeagh' = 'Lough Feeagh',
                  'Harp' = 'Harp Lake',
                  'Langtjern' = 'Lake Langtjern',
                  'Lillinonah' = 'Lake Lillinonah',
                  'Lillsjoliden' = 'Lillsjölidtjärnen',
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

  # keeping x and y axis scales the same for every plot
  mod_c_p = summary(lm(data = dplyr::filter(load_plot, !is.na(mean_c_p), !is.na(mean_c_p_load)),
                       formula = mean_c_p~mean_c_p_load))
  c_p_pval = round(mod_c_p$coefficients[8], digits = 2)
  c_p_r2 = round(mod_c_p$r.squared, digits = 2)

  c_p_load_vs_lake_stoich <- ggplot(load_plot, aes(x = exp(mean_c_p_load),
                                                   y = exp(mean_c_p))) +
    geom_point(size = 5, alpha = 0.5) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(C:P~Load~(mol:mol))) +
    ylab(expression(C:P~Lake~(mol:mol))) +
    geom_abline(slope = 1, color = 'black', linetype = 'dashed') +
    geom_line(stat = "smooth", size = 1, alpha = 0.5,
              method = 'lm', color = 'black', se = F) +
    annotate(geom = 'text',
             x = 200, y = 250, angle = 45, size = 4.5,
             label = '1:1') +
    annotate(geom = 'text',
             x = 250, y = 5000,
             label = paste('p-val < 0.01','\n','R2:', c_p_r2),
             size = 4.5) +
    scale_x_log10(limits = range(c(exp(load_plot$mean_c_p_load),
                                   exp(load_plot$mean_c_p)), na.rm = T)) +
    scale_y_log10(limits = range(c(exp(load_plot$mean_c_p_load),
                                   exp(load_plot$mean_c_p)), na.rm = T))+
    ggrepel::geom_text_repel(aes(label=label_id))

  mod_c_n = summary(lm(data = dplyr::filter(load_plot, !is.na(mean_c_n), !is.na(mean_c_n_load)),
                       formula = mean_c_n~mean_c_n_load))
  c_n_pval = round(mod_c_n$coefficients[8], digits = 2)
  c_n_r2 = round(mod_c_n$r.squared, digits = 2)

  c_n_load_vs_lake_stoich <- ggplot(load_plot, aes(x = exp(mean_c_n_load),
                                                   y = exp(mean_c_n))) +
    geom_point(size = 5, alpha = 0.5,
               color = '#D55E00') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(C:N~Load~(mol:mol))) +
    ylab(expression(C:N~Lake~(mol:mol))) +
    geom_abline(slope = 1, color = 'black', linetype = 'dashed')  +
    geom_line(stat = "smooth", size = 1, alpha = 0.5,
              method = 'lm', color = '#D55E00', se = F) +
    annotate(geom = 'text',
             x = 2.4, y = 3, angle = 45, size = 4.5,
             label = '1:1') +
    annotate(geom = 'text',
             x = 3, y = 50,
             label = paste('p-val < 0.01','\n','R2:', c_n_r2),
             size = 4.5) +
    scale_x_log10(limits = range(c(exp(load_plot$mean_c_n_load),
                             exp(load_plot$mean_c_n)),na.rm = T)) +
    scale_y_log10(limits = range(c(exp(load_plot$mean_c_n_load),
                             exp(load_plot$mean_c_n)), na.rm = T))+
    ggrepel::geom_text_repel(aes(label=label_id))

  # load_vs_lake_stoich
  mod_n_p = summary(lm(data = dplyr::filter(load_plot, !is.na(mean_n_p), !is.na(mean_n_p_load)),
                       formula = mean_n_p~mean_n_p_load))
  n_p_pval = round(mod_n_p$coefficients[8], digits = 2)
  n_p_r2 = round(mod_n_p$r.squared, digits = 2)

  n_p_load_vs_lake_stoich <- ggplot(load_plot, aes(x = exp(mean_n_p_load),
                                                   y = exp(mean_n_p))) +
    geom_point(size = 5, alpha = 0.5,
               color ='#CC79A7') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 13),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(N:P~Load~(mol:mol))) +
    ylab(expression(N:P~Lake~(mol:mol))) +
    geom_abline(slope = 1, color = 'black', linetype = 'dashed')  +
    geom_line(stat = "smooth", size = 1, alpha = 0.5,
              method = 'lm', color = '#CC79A7', se = F, linetype = 'dashed') +
    annotate(geom = 'text',
             x = 20, y = 25, angle = 45, size = 4.5,
             label = '1:1') +
    annotate(geom = 'text',
             x = 20, y = 140,
             label = paste('p-val:',n_p_pval,'\n','R2:', n_p_r2),
             size = 4.5) +
    scale_x_log10(limits = range(c(exp(load_plot$mean_n_p_load),
                                   exp(load_plot$mean_n_p)), na.rm=T)) +
    scale_y_log10(limits = range(c(exp(load_plot$mean_n_p_load),
                                   exp(load_plot$mean_n_p)), na.rm=T))+
    ggrepel::geom_text_repel(aes(label=label_id))

  g = cowplot::plot_grid(c_n_load_vs_lake_stoich, c_p_load_vs_lake_stoich, n_p_load_vs_lake_stoich,
                labels = c('A', 'B', 'C'), align = 'hv',nrow = 1)

  ggsave(out_file,
         plot = g,
         width = 12, height = 4)
  return(out_file)
}


plot_obs_pred_metab_inlake <- function(
  annual_data,
  label_id,
  out_file
){
  # top GPP model for inlake vs. metab
  summary(lm(annual_data$mean_gpp ~ annual_data$mean_p))
  annual_data <- left_join(annual_data,
                           select(label_id, -mean_gpp), by = "lake")

  annual_data$gpp_preds= exp(predict(lm(annual_data$mean_gpp ~ annual_data$mean_p)))

  # gpp plot
  gpp = ggplot(annual_data, aes(x = gpp_preds, y = exp(mean_gpp))) +
    geom_point(size = 6, alpha = .5) +
    ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
    xlab(expression(Predicted~GPP~(mg~O[2]~L^-1~day^-1))) +
    ylim(range(c(annual_data$gpp_preds, exp(annual_data$mean_gpp)))) +
    xlim(range(c(annual_data$gpp_preds, exp(annual_data$mean_gpp)))) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
    annotate(geom = 'text',
             x = 4, y = 4.6, angle = 45, size = 6,
             label = '1:1')+
    ggrepel::geom_text_repel(aes(label=label_id))

  gpp

  # top R model for inlake vs. metab
  summary(lm(annual_data$mean_r ~ annual_data$mean_p))

  annual_data$r_preds= -exp(predict(lm(annual_data$mean_r ~ annual_data$mean_p)))

  # r plot
  r = ggplot(annual_data, aes(x = r_preds, y = -exp(mean_r))) +
    geom_point(size = 6, alpha = .5) +
    ylab(expression(Observed~R~(mg~O[2]~L^-1~day^-1))) +
    xlab(expression(Predicted~R~(mg~O[2]~L^-1~day^-1))) +
    ylim(range(c(annual_data$r_preds, -exp(annual_data$mean_r)))) +
    xlim(range(c(annual_data$r_preds, -exp(annual_data$mean_r)))) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
    annotate(geom = 'text',
             x = -4, y = -3.6, angle = 45, size = 6,
             label = '1:1')+
    ggrepel::geom_text_repel(aes(label=label_id))

  r

  # top NEP model for inlake vs. metab
  summary(lm(annual_data$mean_nep ~ annual_data$mean_n_p))

  annual_data$nep_preds= predict(lm(annual_data$mean_nep ~ annual_data$mean_n_p))

  # nep plot
  nep = ggplot(annual_data, aes(x = nep_preds, y = mean_nep)) +
    geom_point(size = 6, alpha = .5) +
    ylab(expression(Observed~NEP~(mg~O[2]~L^-1~day^-1))) +
    xlab(expression(Predicted~NEP~(mg~O[2]~L^-1~day^-1))) +
    ylim(range(c(annual_data$nep_preds, annual_data$mean_nep))) +
    xlim(range(c(annual_data$nep_preds, annual_data$mean_nep))) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
    annotate(geom = 'text',
             x = .3, y = .4, angle = 45, size = 6,
             label = '1:1')+
    ggrepel::geom_text_repel(aes(label=label_id))

  nep

  plot_out = cowplot::plot_grid(gpp, r, nep, nrow = 1)

  ggsave(filename = out_file,
         plot = plot_out, width = 14, height =5 )

  return(out_file)
}


plot_obs_resid_metab_inlake <- function(
  annual_data,
  out_file
){
  # residuals for top GPP model for inlake vs. metab
  annual_data$gpp_resids = resid(lm(annual_data$mean_gpp ~ annual_data$mean_p))

  summary(lm(annual_data$gpp_resids ~ annual_data$mean_c))

  # gpp plot
  gpp = ggplot(annual_data, aes(x = mean_c, y = gpp_resids)) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(DOC~(mol~C~m^-3))) +
    ylab(expression(GPP~predicted~by~TP~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    # adding labels for outliers
    geom_text(aes(label=lake), vjust = 0, hjust = 0) +
    geom_smooth(se = F, method = "lm")

  gpp

  # residuals for top R model for inlake vs. metab
  annual_data$r_resids = resid(lm(annual_data$mean_r ~ annual_data$mean_p))

  summary(lm(annual_data$r_resids ~ annual_data$mean_c))

  # r plot
  r = ggplot(annual_data, aes(x = mean_c, y = r_resids)) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(DOC~(mol~C~m^-3))) +
    ylab(expression(R~predicted~by~TP~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    # adding labels for outliers
    geom_text(aes(label=lake), vjust = 0, hjust = 0) +
    geom_smooth(se = F, method = "lm")

  r

  # residuals for top NEP model for inlake vs. metab
  annual_data$nep_resids = resid(lm(annual_data$mean_nep ~ annual_data$mean_n_p))

  # nep plot
  nep = ggplot(annual_data, aes(x = mean_c, y = nep_resids)) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(DOC~(mol~C~m^-3))) +
    ylab(expression(NEP~predicted~by~N:P~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    # adding labels for outliers
    geom_text(aes(label=lake), vjust = 0, hjust = 0) +
    geom_smooth(se = F, method = "lm")

  nep

  plot_out = cowplot::plot_grid(gpp, r, nep, nrow = 1)

  ggsave(filename = out_file,
         plot = plot_out, width = 14, height =5 )

  return(out_file)
}

plot_obs_pred_metab_load <- function(
  annual_data,
  label_id,
  out_file
){
  options(na.action = 'na.omit')

  annual_data <- left_join(annual_data,
                           select(label_id, -mean_gpp), by = "lake")

  # top GPP model for load vs. metab
  summary(lm(annual_data$mean_gpp ~ annual_data$mean_c_load +
               annual_data$mean_p_load + annual_data$mean_n_p_load))

  gpp_preds = exp(predict(lm(annual_data$mean_gpp ~ annual_data$mean_c_load +
                               annual_data$mean_p_load + annual_data$mean_n_p_load))) %>%
    {
      rows = names(.)
      gpp_preds = .
      tibble(rows = rows, gpp_preds = gpp_preds)
    }

  # merging to get missing lakes in here
  annual_data$rows = as.character(1:16)
  annual_data = left_join(annual_data, gpp_preds)

  # gpp plot
  gpp = ggplot(annual_data, aes(x = gpp_preds, y = exp(mean_gpp))) +
    geom_point(size = 6, alpha = .5) +
    ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
    xlab(expression(Predicted~GPP~(mg~O[2]~L^-1~day^-1))) +
    ylim(range(c(annual_data$gpp_preds, exp(annual_data$mean_gpp)),na.rm = T)) +
    xlim(range(c(annual_data$gpp_preds, exp(annual_data$mean_gpp)),na.rm = T)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
    annotate(geom = 'text',
             x = 4, y = 4.6, angle = 45, size = 6,
             label = '1:1')+
    ggrepel::geom_text_repel(aes(label=label_id))

  gpp

  # top R model for load vs. metab
  summary(lm(annual_data$mean_r ~ annual_data$mean_c_load +
               annual_data$mean_p_load + annual_data$mean_n_p_load))

  r_preds= -exp(predict(lm(annual_data$mean_r ~ annual_data$mean_c_load +
                             annual_data$mean_p_load + annual_data$mean_n_p_load))) %>%
    {
      rows = names(.)
      r_preds = .
      tibble(rows = rows, r_preds = r_preds)
    }

  # merging to get missing lakes in here
  annual_data = left_join(annual_data, r_preds)

  # r plot
  r = ggplot(annual_data, aes(x = r_preds, y = -exp(mean_r))) +
    geom_point(size = 6, alpha = .5) +
    ylab(expression(Observed~R~(mg~O[2]~L^-1~day^-1))) +
    xlab(expression(Predicted~R~(mg~O[2]~L^-1~day^-1))) +
    ylim(range(c(annual_data$r_preds, -exp(annual_data$mean_r)),na.rm = T)) +
    xlim(range(c(annual_data$r_preds, -exp(annual_data$mean_r)),na.rm = T)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
    annotate(geom = 'text',
             x = -4, y = -3.6, angle = 45, size = 6,
             label = '1:1')+
    ggrepel::geom_text_repel(aes(label=label_id))

  r

  # top NEP model for load vs. metab
  summary(lm(annual_data$mean_nep ~ annual_data$mean_n_p_load))

  nep_preds= predict(lm(annual_data$mean_nep ~ annual_data$mean_n_p_load)) %>%
    {
      rows = names(.)
      nep_preds = .
      tibble(rows = rows, nep_preds = nep_preds)
    }

  # merging to get missing lakes in here
  annual_data = left_join(annual_data, nep_preds)

  # nep plot
  nep = ggplot(annual_data, aes(x = nep_preds, y = mean_nep)) +
    geom_point(size = 6, alpha = .5) +
    ylab(expression(Observed~NEP~(mg~O[2]~L^-1~day^-1))) +
    xlab(expression(Predicted~NEP~(mg~O[2]~L^-1~day^-1))) +
    ylim(range(c(annual_data$nep_preds, annual_data$mean_nep),na.rm = T)) +
    xlim(range(c(annual_data$nep_preds, annual_data$mean_nep),na.rm = T)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
    annotate(geom = 'text',
             x = .3, y = .4, angle = 45, size = 6,
             label = '1:1')+
    ggrepel::geom_text_repel(aes(label=label_id))

  nep

  plot_out = cowplot::plot_grid(gpp, r, nep, nrow = 1)

  ggsave(filename = out_file,
         plot = plot_out, width = 14, height =5 )

  return(out_file)
}


plot_obs_resid_metab_load <- function(
  annual_data,
  out_file
){
  options(na.action = 'na.omit')
  # residuals for GPP model for load vs. metab
  gpp_resids = resid(lm(annual_data$mean_gpp ~ annual_data$mean_p_load +
                          annual_data$mean_n_p_load)) %>%
    {
      rows = names(.)
      gpp_resids = .
      tibble(rows = rows, gpp_resids = gpp_resids)
    }

  # merging to get missing lakes in here
  annual_data$rows = as.character(1:16)
  annual_data = left_join(annual_data, gpp_resids)

  # gpp plot
  gpp = ggplot(annual_data, aes(x = mean_c_load, y = gpp_resids)) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(C~Load~(mol~C~m^-3~day^-1))) +
    ylab(expression(GPP~predicted~by~P~and~N:P~loads~Resids)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    # adding labels for outliers
    geom_text(aes(label=lake), vjust = 0, hjust = 0) +
    geom_smooth(se = F, method = "lm")

  gpp

  # residuals for R model for load vs. metab
  r_resids = resid(lm(annual_data$mean_r ~ annual_data$mean_p_load +
                        annual_data$mean_n_p_load)) %>%
  {
      rows = names(.)
      r_resids = .
      tibble(rows = rows, r_resids = r_resids)
    }

  # merging to get missing lakes in here
  annual_data = left_join(annual_data, r_resids)

  # r plot
  r = ggplot(annual_data, aes(x = mean_c_load, y = r_resids)) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(C~Load~(mol~C~m^-3~day^-1))) +
    ylab(expression(R~predicted~by~P~and~N:P~loads~Resids)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    # adding labels for outliers
    geom_text(aes(label=lake), vjust = 0, hjust = 0) +
    geom_smooth(se = F, method = "lm")

  r

  # top NEP model for load vs. metab
  summary(lm(annual_data$mean_nep ~ annual_data$mean_n_p_load))

  nep_resids= resid(lm(annual_data$mean_nep ~ annual_data$mean_n_p_load)) %>%
    {
      rows = names(.)
      nep_resids = .
      tibble(rows = rows, nep_resids = nep_resids)
    }

  # merging to get missing lakes in here
  annual_data = left_join(annual_data, nep_resids)

  # nep plot
  nep = ggplot(annual_data, aes(x = mean_c_load, y = nep_resids)) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(C~Load~(mol~C~m^-3~day^-1))) +
    ylab(expression(NEP~predicted~by~N:P~loads~Resids)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    # adding labels for outliers
    geom_text(aes(label=lake), vjust = 0, hjust = 0) +
    geom_smooth(se = F, method = "lm")

  nep

  plot_out = cowplot::plot_grid(gpp, r, nep, nrow = 1)

  ggsave(filename = out_file,
         plot = plot_out, width = 14, height =5 )

  return(out_file)
}

plot_models_inlake <- function(
    annual_data,
    label_id,
    out_file
){

  annual_data <- left_join(annual_data, select(label_id, -mean_gpp), by = "lake")
  # residuals for top GPP model for inlake vs. metab
  annual_data$gpp_resids = resid(lm(annual_data$mean_gpp ~ annual_data$mean_p))

  summary(lm(annual_data$gpp_resids ~ annual_data$mean_c))

  # gpp plot
  gpp_1 = ggplot(annual_data,
                 aes(x = exp(mean_p) * 31 * 1000, # converting from mol/m3 to ug P / L
                     y = exp(mean_gpp))) +
    geom_smooth(se = F, method = "lm",
                color = "black", alpha = 0.5) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(Lake~TP~(mu*g~P~L^-1))) +
    ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ggrepel::geom_text_repel(aes(label=label_id))  +
    scale_y_log10() + scale_x_log10(limits = c(3.7,100))

  gpp_2 = ggplot(annual_data,
                 aes(x = mean_c * 12, # converting from mol/m3 to mg C / L
                     y = gpp_resids)) +
    geom_smooth(se = F, method = "lm", linetype = "dashed",
                color = "grey50", alpha = 0.5) +
    geom_point(size = 6, alpha = .5,
               color = "grey50") +
    xlab(expression(DOC~(mg~C~L^-1))) +
    ylab(expression(GPP~predicted~by~TP~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    # geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    ggrepel::geom_text_repel(aes(label=label_id))

  # r plot
  r_1 = ggplot(annual_data,
                 aes(x = exp(mean_p) * 31 * 1000, # converting from mol/m3 to ug P / L
                     y = exp(mean_r))) +
    geom_smooth(se = F, method = "lm",
                color = "black", alpha = 0.5) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(Lake~TP~(mu*g~P~L^-1))) +
    ylab(expression(Observed~R~(mg~O[2]~L^-1~day^-1))) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ggrepel::geom_text_repel(aes(label=label_id))  +
    scale_x_log10(limits = c(3.7,100)) + scale_y_log10()

  # residuals for top NEP model for inlake vs. metab
  annual_data$nep_resids = resid(lm(annual_data$mean_nep ~ annual_data$mean_n_p))

  # nep plot
  nep_1 = ggplot(annual_data, aes(x = exp(mean_n_p), y = mean_nep)) +
    geom_smooth(se = F, method = "lm", color = "black") +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(N:P~Lake~(mol:mol))) +
    ylab(expression(Observed~NEP~(mg~O[2]~L^-1~day^-1))) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ggrepel::geom_text_repel(aes(label=label_id)) +
    scale_x_log10()

  nep_2 = ggplot(annual_data,
                 aes(x = exp(mean_p) * 31 * 1000, # converting from mol/m3 to ug P / L
                     y = nep_resids)) +
    geom_smooth(se = F, method = "lm",
                color = "grey50", linetype = "dashed") +
    geom_point(size = 6, alpha = .5,
               color = "grey50") +
    xlab(expression(Lake~TP~(mu*g~P~L^-1))) +
    ylab(expression(NEP~predicted~by~N:P~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ggrepel::geom_text_repel(aes(label=label_id)) +
    scale_x_log10(limits = c(3.7,100))

  plot_out = cowplot::plot_grid(gpp_1, gpp_2,
                                r_1, NULL,
                                nep_1, nep_2,
                                nrow = 3,
                                labels = c("A", "B",
                                           "C", "",
                                           "D", "E"))

  ggsave(filename = out_file,
         plot = plot_out, width = 9, height = 14 )

  return(out_file)
}

plot_models_load <- function(
    annual_data,
    label_id,
    out_file
){

  options(na.action = "na.omit")

  annual_data <- left_join(annual_data, select(label_id, -mean_gpp), by = "lake")
  # residuals for top GPP model for load vs. metab
  annual_data <- left_join(annual_data,
                           tibble(gpp_resids = resid(lm(annual_data$mean_gpp ~ annual_data$mean_p_load)),
                                  lake = annual_data$lake[!is.na(annual_data$mean_p_load)]),
                           by = "lake")

  summary(lm(annual_data$gpp_resids ~ annual_data$mean_n_p_load))

  # gpp plot
  gpp_1 = ggplot(annual_data,
                 aes(x = exp(mean_p_load) * 31 * 1000 * 1000 * 1000, # converting from kmol/m3/day to ug P/m3/day
                     y = exp(mean_gpp))) +
    geom_smooth(se = F, method = "lm",
                color = "black", alpha = 0.5) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(TP~Load~(mu*g~P~(m^3~lake~water)^-1~day^-1))) +
    ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    ggrepel::geom_text_repel(aes(label=label_id))  +
    scale_y_log10() + scale_x_log10()

  gpp_2 = ggplot(annual_data,
                 aes(x = exp(mean_n_p_load),
                     y = gpp_resids)) +
    geom_smooth(se = F, method = "lm", linetype = "dashed",
                color = "grey50", alpha = 0.5) +
    geom_point(size = 6, alpha = .5,
               color = "grey50") +
    xlab(expression(N:P~Load~(mol:mol))) +
    ylab(expression(GPP~by~TP~Load~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    # geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    ggrepel::geom_text_repel(aes(label=label_id)) +
    scale_x_log10()

  annual_data <- left_join(annual_data,
                           tibble(gpp_resids_2 = resid(lm(annual_data$mean_gpp ~
                                                            annual_data$mean_p_load + annual_data$mean_n_p_load)),
                                  lake = annual_data$lake[!is.na(annual_data$mean_p_load)]),
                           by = "lake")

  summary(lm(annual_data$gpp_resids_2 ~ annual_data$mean_c_load))

  gpp_3 = ggplot(annual_data,
                 aes(x = exp(mean_c_load) * 12 * 1000 * 1000, # converting from kmol/m3/day to mg C/m3/day
                     y = gpp_resids_2)) +
    geom_smooth(se = F, method = "lm", linetype = "dashed",
                color = "grey80", alpha = 0.5) +
    geom_point(size = 6, alpha = .5,
               color = "grey80") +
    xlab(expression(DOC~load~(mg~C~(m^3~lake~water)^-1~day^-1))) +
    ylab(expression(GPP~by~TP~and~N:P~Load~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    # geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    ggrepel::geom_text_repel(aes(label=label_id)) +
    scale_x_log10()

  # r plot
  # residuals for top R model for load vs. metab
  annual_data <- left_join(annual_data,
                           tibble(r_resids = resid(lm(annual_data$mean_r ~ annual_data$mean_p_load)),
                                  lake = annual_data$lake[!is.na(annual_data$mean_p_load)]),
                           by = "lake")
  summary(lm(annual_data$r_resids ~ annual_data$mean_n_p_load))

  r_1 = ggplot(annual_data,
               aes(x = exp(mean_p_load) * 31 * 1000 * 1000 * 1000, # converting from kmol/m3/day to ug P/m3/day
                   y = exp(mean_r))) +
    geom_smooth(se = F, method = "lm",
                color = "black", alpha = 0.5) +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(TP~Load~(mu*g~P~(m^3~lake~water)^-1~day^-1))) +
    ylab(expression(Observed~R~(mg~O[2]~L^-1~day^-1))) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    ggrepel::geom_text_repel(aes(label=label_id))  +
    scale_x_log10() + scale_y_log10()

  r_2 = ggplot(annual_data,
               aes(x = exp(mean_n_p_load),
                   y = r_resids)) +
    geom_smooth(se = F, method = "lm",
                color = "grey50", alpha = 0.5) +
    geom_point(size = 6, alpha = .5,
               color = "grey50") +
    xlab(expression(N:P~Load~(mol:mol))) +
    ylab(expression(R~by~TP~Load~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    # geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    ggrepel::geom_text_repel(aes(label=label_id)) +
    scale_x_log10()

  annual_data <- left_join(annual_data,
                           tibble(r_resids_2 = resid(lm(annual_data$mean_r ~
                                                          annual_data$mean_p_load + annual_data$mean_n_p_load)),
                                  lake = annual_data$lake[!is.na(annual_data$mean_p_load)]),
                           by = "lake")

  summary(lm(annual_data$r_resids_2 ~ annual_data$mean_c_load))

  r_3 = ggplot(annual_data,
                 aes(x = exp(mean_c_load) * 12 * 1000 * 1000, # converting from kmol/m3/day to mg C/m3/day
                     y = r_resids_2)) +
    geom_smooth(se = F, method = "lm", linetype = "dashed",
                color = "grey80", alpha = 0.5) +
    geom_point(size = 6, alpha = .5,
               color = "grey80") +
    xlab(expression(DOC~load~(mg~C~(m^3~lake~water)^-1~day^-1))) +
    ylab(expression(R~predicted~by~TP~Load~residuals)) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    # geom_abline(slope = 0, intercept = 0, linetype = 'dashed', size = 1) +
    ggrepel::geom_text_repel(aes(label=label_id)) +
    scale_x_log10()

  summary(lm(annual_data$mean_nep ~ annual_data$mean_n_p_load))
  # nep plot
  nep_1 = ggplot(annual_data, aes(x = exp(mean_n_p_load), y = mean_nep)) +
    geom_smooth(se = F, method = "lm", linetype = "dashed",
                color = "black") +
    geom_point(size = 6, alpha = .5) +
    xlab(expression(N:P~Load~(mol:mol))) +
    ylab(expression(Observed~NEP~(mg~O[2]~L^-1~day^-1))) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    ggrepel::geom_text_repel(aes(label=label_id)) +
    scale_x_log10()

  # inset_plot_gpp <- cowplot::plot_grid(gpp_2, gpp_3,
  #                                      nrow = 1, ncol = 2,
  #                                      labels = "B", "C")
  # inset_plot_r <- cowplot::plot_grid(r_2, r_3,
  #                                    nrow = 1, ncol = 2,
  #                                    labels = "E", "F")

  plot_out = cowplot::plot_grid(gpp_1, gpp_2, gpp_3,
                                r_1, r_2, r_3,
                                nep_1, NULL, NULL,
                                nrow = 3,
                                labels = c("A", "B", "C",
                                           "D", "E", "F",
                                           "G", "", ""))

  ggsave(filename = out_file,
         plot = plot_out, width = 16, height = 16 )

  return(out_file)
}



plot_gpp_r <- function(
  annual_data,
  out_file
){

  plot_out = ggplot(annual_data, aes(x = -exp(mean_r), y = exp(mean_gpp))) +
    geom_point(size = 6, alpha = .5) +
    ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
    xlab(expression(Observed~R~(mg~O[2]~L^-1~day^-1))) +
    scale_x_reverse() +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18)) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) +
    annotate(geom = 'text',
             x = -4, y = 4.6, angle = 45, size = 6,
             label = '1:1')+
    # adding labels for outliers
    geom_text(aes(label=lake), vjust = 0, hjust = 0)


  ggsave(filename = out_file,
         plot = plot_out, width = 6, height =6)

  return(out_file)
}


plot_doc_gpp <- function(
    annual_data,
    label_id,
    out_file
){

  annual_data <- left_join(annual_data, select(label_id, -mean_gpp), by = "lake")

  gpp = ggplot(annual_data,
               aes(x = mean_c * 12, # converting from mol/m3 to mg C / L
                   y = exp(mean_gpp))) +
    geom_point(size = 6, alpha = .5,
               color = "grey50") +
    xlab(expression(DOC~(mg~C~L^-1))) +
    ylab(expression(Observed~GPP~(mg~O[2]~L^-1~day^-1))) +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16)) +
    ggrepel::geom_text_repel(aes(label=label_id))

  ggsave(filename = out_file,
         plot = gpp, width = 6, height = 6)

  return(out_file)
}

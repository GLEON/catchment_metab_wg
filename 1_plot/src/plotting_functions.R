
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
                  'Lillsjoliden' = 'Lillsjöliden',
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
  metab <- ggplot(dplyr::filter(metab_data, !is.na(season)),
                  aes(x = plot_date, y = GPP, color = season)) +
    geom_line(size = linesize, alpha = line_alpha) +
    geom_point(size = pointsize) +
    geom_line(data = dplyr::filter(metab_data, !is.na(season)),
              aes( x= plot_date, y = R), size = linesize, alpha = line_alpha) +
    geom_point(data = dplyr::filter(metab_data, !is.na(season)),
               aes( x= plot_date, y = R), size = pointsize) +
    facet_wrap(~lake,labeller = as_labeller(lake_names), strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    scale_color_manual(name = 'season',
                       values = c('spring' = '#009E73',
                                  'summer' = '#56B4E9',
                                  'fall' = '#E69F00'),
                       labels = c('Spring', 'Summer', 'Fall')) +
    scale_fill_manual(name = 'season',
                      values = c('spring' = '#009E73',
                                 'summer' = '#56B4E9',
                                 'fall' = '#E69F00'),
                      labels = c('Spring', 'Summer', 'Fall')) +
    ylab(expression(Metabolism~(mg~O[2]~L^-1~day^-1))) +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey')

  ggsave(out_file,
         plot = metab,
         width = 10,
         height = 10)
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

  browser()
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
                  'Lillsjoliden' = 'Lillsjöliden',
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
  load_stoich <- ggplot(load_data, aes(x = plot_date, y = DOC_load/TP_load, group = lake)) +
    geom_line(aes(color = 'a'), size = 1) +
    geom_line(data = load_data, aes(x = plot_date, y = TN_load / TP_load, group = lake, color = 'b'), size = 1)+
    geom_line(data = load_data, aes(x = plot_date, y = DOC_load / TN_load, group = lake, color = 'c'), size = 1)+
    facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    scale_color_manual(name = '',
                       values = c('a' = 'black',
                                  'b' = '#CC79A7',
                                  'c' = '#D55E00'),
                       labels = c('C:P', 'N:P', 'C:N')) +
    ylab(expression(Load~Stoichiometry~(mol:mol))) +
    scale_y_log10() +
    geom_hline(yintercept = 106, color = 'black', linetype = 'dashed')+ # redfield ratios
    geom_hline(yintercept = 16, color ='#CC79A7', linetype = 'dashed')+
    geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed')+
    scale_x_date(date_labels = '%b')


  in_lake_stoich = ggplot(load_data, aes(x = plot_date, y = DOC/TP, group = lake)) +
    geom_point(aes(color = 'a'), size = 1) +
    geom_point(data = load_data, aes(x = plot_date, y = TN / TP, group = lake, color = 'b'), size = 1)+
    geom_point(data = load_data, aes(x = plot_date, y = DOC / TN, group = lake, color = 'c'), size = 1)+
    facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    scale_color_manual(name = '',
                       values = c('a' = 'black',
                                  'b' = '#CC79A7',
                                  'c' = '#D55E00'),
                       labels = c('C:P', 'N:P', 'C:N')) +
    ylab(expression(Lake~Stoichiometry~(mol:mol))) +
    scale_y_log10() +
    geom_hline(yintercept = 106, color = 'black', linetype = 'dashed')+ # redfield ratios
    geom_hline(yintercept = 16, color ='#CC79A7', linetype = 'dashed')+
    geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed')+
    scale_x_date(date_labels = '%b')

  lake_load_stoich <- ggplot(load_data, aes(x = plot_date, y = DOC_load/TP_load, group = lake)) +
    geom_line(aes(color = 'a'), size = 1) +
    geom_line(data = load_data, aes(x = plot_date, y = TN_load / TP_load, group = lake, color = 'b'), size = 1)+
    geom_line(data = load_data, aes(x = plot_date, y = DOC_load / TN_load, group = lake, color = 'c'), size = 1)+
    geom_point(data = load_data, aes(x = plot_date, y = DOC / TP, group = lake, color = 'a'), size = 1)+
    geom_point(data = load_data, aes(x = plot_date, y = TN / TP, group = lake, color = 'b'), size = 1)+
    geom_point(data = load_data, aes(x = plot_date, y = DOC / TN, group = lake, color = 'c'), size = 1)+
    facet_wrap(~lake, labeller = as_labeller(lake_names), strip.position = 'top') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    scale_color_manual(name = '',
                       values = c('a' = 'black',
                                  'b' = '#CC79A7',
                                  'c' = '#D55E00'),
                       labels = c('C:P', 'N:P', 'C:N')) +
    ylab(expression(Load~or~Lake~Stoichiometry~(mol:mol))) +
    scale_y_log10() +
    geom_hline(yintercept = 106, color = 'black', linetype = 'dashed')+ # redfield ratios
    geom_hline(yintercept = 16, color ='#CC79A7', linetype = 'dashed')+
    geom_hline(yintercept = 6.6, color = '#D55E00', linetype = 'dashed') +
    scale_x_date(date_labels = '%b')

  ggsave(out_file,
         plot = lake_load_stoich,
         width = 10, height = 10)
}

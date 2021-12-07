
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


plot_stream_lake_nutrient <- function(
  load_data,
  inlake_data,
  config,
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

  browser()
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
                     .groups = 'drop')

  doc <- ggplot(summary_df, aes(y = mean_lake_doc, x = mean_doc_conc_load_mg_L)) +
    geom_point(size = 5) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~DOC~(mg~L^-1))) +
    xlab(expression(Stream~DOC~(mg~L^-1))) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
    annotate(geom = 'text',
             x = 15, y = 20, size = 6,
             label = '1:1')

  tn <- ggplot(summary_df, aes(y = mean_lake_tn, x = mean_tn_conc_load_ug_L)) +
    geom_point(size = 5) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~TN~(mu*g~L^-1))) +
    xlab(expression(Stream~TN~(mu*g~L^-1))) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
    annotate(geom = 'text',
             x = 2500, y = 3000, size = 6,
             label = '1:1')

  tp <- ggplot(summary_df, aes(y = mean_lake_tp, x = mean_tp_conc_load_ug_L)) +
    geom_point(size = 5) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    ylab(expression(Lake~TP~(mu*g~L^-1))) +
    xlab(expression(Stream~TP~(mu*g~L^-1)))+
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed')+
    annotate(geom = 'text',
             x = 50, y = 60, size = 6,
             label = '1:1')

  g = plot_grid(doc, tn, tp,
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
                     .groups = 'drop')

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

  doc_tp <- ggplot(load_plot, aes(x = mean_doc_load *1000*1000, y = mean_tp_load *1000*1000*1000)) +
    geom_line(data = redfield_line(ratio = 'c_p',
                                   x_axis_element = 'c',
                                   x_axis_range = range(load_plot$mean_doc_load *1000*1000, na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5) +
    annotate(geom = 'text',
             x = 10, y = 420, angle = 42, size = 6,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(DOC~Load~(mg~C~m^-3~day^-1))) +
    ylab(expression(TP~Load~(mu*g~P~m^-3~day^-1)))+ scale_y_log10() + scale_x_log10()


  doc_tn <- ggplot(load_plot, aes(x = mean_doc_load *1000*1000, y = mean_tn_load *1000*1000*1000)) +
    # geom_abline(slope = (16*14)/(106*12), intercept = 0, linetype = 'dashed') +
    geom_line(data = redfield_line(ratio = 'c_n',
                                   x_axis_element = 'c',
                                   x_axis_range = range(load_plot$mean_doc_load *1000*1000, na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, color = '#D55E00') +
    annotate(geom = 'text',
             x = 10, y = 5000, angle = 42, size = 6,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(DOC~Load~(mg~C~m^-3~day^-1))) +
    ylab(expression(TN~Load~(mu*g~P~m^-3~day^-1))) + scale_y_log10() + scale_x_log10()

  tn_tp <- ggplot(load_plot, aes(x = mean_tn_load *1000*1000*1000, y = mean_tp_load *1000*1000*1000)) +
    # geom_abline(slope = (1*31)/(16*14), intercept = 0, linetype = 'dashed') +
    geom_line(data = redfield_line(ratio = 'n_p',
                                   x_axis_element = 'n',
                                   x_axis_range = range(load_plot$mean_tn_load *1000*1000*1000, na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, color ='#CC79A7') +
    annotate(geom = 'text',
             x = 1000, y = 600, angle = 47, size = 6,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(TN~Load~(mu*g~m^-3~day^-1))) +
    ylab(expression(TP~Load~(mu*g~m^-3~day^-1))) +
    scale_y_log10() + scale_x_log10()

  lake_doc_tp <- ggplot(load_plot, aes(x = mean_lake_doc, y = mean_lake_tp)) +
    geom_line(data = redfield_line(ratio = 'c_p',
                                   x_axis_element = 'c',
                                   x_axis_range = range(load_plot$mean_lake_doc, na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5) +
    annotate(geom = 'text',
             x = 10, y = 320, angle = 26, size = 6,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(Lake~DOC~(mg~C~L^-1))) +
    ylab(expression(Lake~TP~(mu*g~P~L^-1))) +
    scale_y_log10() + scale_x_log10()


  lake_doc_tn <- ggplot(load_plot, aes(x = mean_lake_doc, y = mean_lake_tn)) +
    geom_line(data = redfield_line(ratio = 'c_n',
                                   x_axis_element = 'c',
                                   x_axis_range = range(load_plot$mean_lake_doc, na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, color = '#D55E00') +
    annotate(geom = 'text',
             x = 10, y = 2300, angle = 37, size = 6,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(Lake~DOC~(mg~C~L^-1))) +
    ylab(expression(Lake~TN~(mu*g~N~L^-1)))  +
    scale_y_log10() + scale_x_log10()

  lake_tn_tp <- ggplot(load_plot, aes(x = mean_lake_tn, y = mean_lake_tp)) +
    geom_line(data = redfield_line(ratio = 'n_p',
                                   x_axis_element = 'n',
                                   x_axis_range = range(load_plot$mean_lake_tn, na.rm = T)),
              aes(x = x, y = y),
              linetype = 'dashed') +
    geom_point(size = 5, color ='#CC79A7') +
    annotate(geom = 'text',
             x = 1000, y = 230, angle = 35, size = 6,
             label = 'Redfield Ratio') +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.placement = 'inside',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size =12)) +
    xlab(expression(Lake~TN~(mu*g~N~L^-1))) +
    ylab(expression(Lake~TP~(mu*g~P~L^-1))) +
    scale_y_log10() + scale_x_log10()


  g = plot_grid(doc_tn, doc_tp, tn_tp, lake_doc_tn, lake_doc_tp, lake_tn_tp,
                labels = c('A', 'B', 'C', 'D', 'E', 'F'), align = 'hv',nrow = 2)

  ggsave(out_file,
         plot = g,
         width = 12, height = 8)
  return(out_file)
}


plot_stoich_stream_vs_lake <- function(
  load_data,
  inlake_data,
  config,
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
                     .groups = 'drop')

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
  mod_c_p = summary(lm(data = dplyr::filter(load_plot, !is.na(mean_c_p), !is.na(mean_c_p_load)),
                       formula = mean_c_p~mean_c_p_load))
  c_p_pval = round(mod_c_p$coefficients[8], digits = 2)
  c_p_r2 = round(mod_c_p$r.squared, digits = 2)

  c_p_load_vs_lake_stoich <- ggplot(load_plot, aes(x = exp(mean_c_p_load),
                                                   y = exp(mean_c_p))) +
    geom_point(size = 5) +
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
    geom_smooth(method = 'lm', color = 'black', se = F) +
    annotate(geom = 'text',
             x = 200, y = 250, angle = 45, size = 6,
             label = '1:1') +
    annotate(geom = 'text',
             x = 250, y = 5000,
             label = paste('p-val < 0.01','\n','R2:', c_p_r2),
             size = 6) +
    scale_x_log10(limits = range(c(exp(load_plot$mean_c_p_load),
                                   exp(load_plot$mean_c_p)), na.rm = T)) +
    scale_y_log10(limits = range(c(exp(load_plot$mean_c_p_load),
                                   exp(load_plot$mean_c_p)), na.rm = T))

  mod_c_n = summary(lm(data = dplyr::filter(load_plot, !is.na(mean_c_n), !is.na(mean_c_n_load)),
                       formula = mean_c_n~mean_c_n_load))
  c_n_pval = round(mod_c_n$coefficients[8], digits = 2)
  c_n_r2 = round(mod_c_n$r.squared, digits = 2)

  c_n_load_vs_lake_stoich <- ggplot(load_plot, aes(x = exp(mean_c_n_load),
                                                   y = exp(mean_c_n))) +
    geom_point(size = 5, color = '#D55E00') +
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
    geom_smooth(method = 'lm', color = '#D55E00', se = F) +
    annotate(geom = 'text',
             x = 2.4, y = 3, angle = 45, size = 6,
             label = '1:1') +
    annotate(geom = 'text',
             x = 3, y = 50,
             label = paste('p-val < 0.01','\n','R2:', c_n_r2),
             size = 6) +
    scale_x_log10(limits = range(c(exp(load_plot$mean_c_n_load),
                             exp(load_plot$mean_c_n)),na.rm = T)) +
    scale_y_log10(limits = range(c(exp(load_plot$mean_c_n_load),
                             exp(load_plot$mean_c_n)), na.rm = T))

  # load_vs_lake_stoich
  mod_n_p = summary(lm(data = dplyr::filter(load_plot, !is.na(mean_n_p), !is.na(mean_n_p_load)),
                       formula = mean_n_p~mean_n_p_load))
  n_p_pval = round(mod_n_p$coefficients[8], digits = 2)
  n_p_r2 = round(mod_n_p$r.squared, digits = 2)

  n_p_load_vs_lake_stoich <- ggplot(load_plot, aes(x = exp(mean_n_p_load),
                                                   y = exp(mean_n_p))) +
    geom_point(size = 5, color ='#CC79A7') +
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
    geom_smooth(method = 'lm', color = '#CC79A7', se = F, linetype = 'dashed') +
    annotate(geom = 'text',
             x = 20, y = 25, angle = 45, size = 6,
             label = '1:1') +
    annotate(geom = 'text',
             x = 20, y = 140,
             label = paste('p-val:',n_p_pval,'\n','R2:', n_p_r2),
             size = 6) +
    scale_x_log10(limits = range(c(exp(load_plot$mean_n_p_load),
                                   exp(load_plot$mean_n_p)), na.rm=T)) +
    scale_y_log10(limits = range(c(exp(load_plot$mean_n_p_load),
                                   exp(load_plot$mean_n_p)), na.rm=T))

  g = plot_grid(c_n_load_vs_lake_stoich, c_p_load_vs_lake_stoich, n_p_load_vs_lake_stoich,
                labels = c('A', 'B', 'C'), align = 'hv',nrow = 1)

  ggsave(out_file,
         plot = g,
         width = 12, height = 4)
  return(out_file)
}


plot_obs_pred_metab_inlake <- function(
  annual_data,
  out_file
){
  # top GPP model for inlake vs. metab
  summary(lm(annual_data$mean_gpp ~ annual_data$mean_p))

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
             label = '1:1')

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
             label = '1:1')

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
             label = '1:1')

  nep

  plot_out = cowplot::plot_grid(gpp, r, nep, nrow = 1)

  ggsave(filename = out_file,
         plot = plot_out, width = 14, height =5 )

  return(out_file)
}


plot_obs_pred_metab_load <- function(
  annual_data,
  out_file
){
  options(na.action = 'na.omit')

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
             label = '1:1')

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
             label = '1:1')

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
             label = '1:1')
  nep

  plot_out = cowplot::plot_grid(gpp, r, nep, nrow = 1)

  ggsave(filename = out_file,
         plot = plot_out, width = 14, height =5 )

  return(out_file)
}


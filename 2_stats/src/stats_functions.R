


summarize_to_annual_and_normalize <- function(
  load_data,
  inlake_data,
  metab_data,
  season_data,
  config
){

  metab_data = filter_metab_timeseries(metab_data = metab_data,
                                       season_data = season_data,
                                       config = config)

  # summarize to annual mean
  metab_data_annual <- metab_data %>%
    group_by(lake) %>%
    summarise(mean_gpp = mean(GPP, na.rm=T),
              mean_r = mean(R, na.rm =T),
              mean_nep = mean(NEP, na.rm=T),
              .groups = 'drop')

  all_load = left_join(load_data,
                       inlake_data,
                       by = c('lake' = 'lake', 'date' = 'date'))

  cv_cutoff = config$cv_cutoff
  min_doy = config$min_doy
  max_doy = config$max_doy

  load_data_annual <- dplyr::filter(all_load, doy > min_doy, doy < max_doy) %>%
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
    dplyr::summarise(mean_p_load = mean(TP_load / Volume..m3., na.rm=T), # mol/(m3 lake water) / day
                     mean_n_load = mean(TN_load / Volume..m3., na.rm =T),
                     mean_c_load = mean(DOC_load / Volume..m3., na.rm=T),
                     # need to log transform ratios following Isles 2020 https://doi.org/10.1002/ecy.3153
                     mean_c_p_load = mean(log(DOC_load/TP_load), na.rm = T),
                     mean_c_n_load = mean(log(DOC_load/TN_load), na.rm = T),
                     mean_n_p_load = mean(log(TN_load/TP_load), na.rm = T),
                     max_c_p_load = max(log(DOC_load/TP_load), na.rm = T),
                     max_c_n_load = max(log(DOC_load/TN_load), na.rm = T),
                     max_n_p_load = max(log(TN_load/TP_load), na.rm = T),
                     min_c_p_load = min(log(DOC_load/TP_load), na.rm = T),
                     min_c_n_load = min(log(DOC_load/TN_load), na.rm = T),
                     min_n_p_load = min(log(TN_load/TP_load), na.rm = T),
                     # inlake nutrients
                     mean_p = mean(TP, na.rm = T), # mol/(m3 lake water)
                     mean_n = mean(TN, na.rm = T),
                     mean_c = mean(DOC, na.rm = T),
                     # need to log transform ratios following Isles 2020 https://doi.org/10.1002/ecy.3153
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

  all_data_annual <- left_join(load_data_annual,
                               metab_data_annual,
                               by = 'lake')

  # testing for normality
  # metab
  shapiro.test(all_data_annual$mean_gpp) # GPP needs to be transformed ****
  shapiro.test(all_data_annual$mean_r * -1) # R needs to be transformed ****
  shapiro.test(all_data_annual$mean_nep) # NEP meets normality

  shapiro.test(log(all_data_annual$mean_gpp)) # ln(GPP) meets normality
  shapiro.test(log(all_data_annual$mean_r * -1)) # ln(R) meets normality

  # in-lake
  shapiro.test(all_data_annual$mean_c) # lake C concentration meets normality
  shapiro.test(all_data_annual$mean_n) # lake N concentration needs to be transformed ****
  shapiro.test(all_data_annual$mean_p) # lake P concentration needs to be transformed ****

  shapiro.test(rcompanion::transformTukey(all_data_annual$mean_n))
  shapiro.test(-1 * all_data_annual$mean_n ^ -0.65) # -1 * lake_n^-0.65 meets normality
  shapiro.test(log(all_data_annual$mean_p)) # ln(lake_p) meets normality

  # in-lake stoich
  shapiro.test(all_data_annual$mean_c_p) # ln(c:p) doesn't meet normality but is close (pval >0.01)****
  shapiro.test(all_data_annual$mean_c_n) # ln(c:n) doesn't meet normality but is close (pval >0.01)****
  shapiro.test(all_data_annual$mean_n_p) # ln(n:p) meets normality

  # loads
  shapiro.test(all_data_annual$mean_p_load) # P load / lake vol needs to be transformed ****
  shapiro.test(all_data_annual$mean_n_load) # N load / lake vol needs to be transformed ****
  shapiro.test(all_data_annual$mean_c_load) # C load / lake vol needs to be transformed ****

  shapiro.test(log(all_data_annual$mean_p_load)) # ln(P load / lake vol) meets normality
  shapiro.test(log(all_data_annual$mean_n_load)) # ln(N load / lake vol) meets normality
  shapiro.test(log(all_data_annual$mean_c_load)) # ln(C load / lake vol) meets normality

  # load stoich
  shapiro.test(all_data_annual$mean_c_p_load) # ln(c:p load) meets normality
  shapiro.test(all_data_annual$mean_c_n_load) # ln(c:n load) doesn't meet normality but is close (pval >0.01)****
  shapiro.test(all_data_annual$mean_n_p_load) # ln(n:p load) doesn't meet normality but is close (pval >0.01)****

  all_data_annual <- all_data_annual %>%
    # transform based on transformations checked above ^
    mutate(mean_gpp = log(mean_gpp),
           mean_r = log(mean_r * -1),
           mean_n = -1 * mean_n ^ -0.65,
           mean_p = log(mean_p),
           mean_p_load = log(mean_p_load),
           mean_n_load = log(mean_n_load),
           mean_c_load = log(mean_c_load))

  return(all_data_annual)
}

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


lm_inlake_vs_metab <- function(
  annual_data,
  out_file
){

  ## multi model selection based on AIC
  gpp_out = tibble()
  r_out = tibble()
  nep_out = tibble()

  # GPP
  options(na.action = 'na.fail')

  predictors = c('mean_c', 'mean_p', 'mean_n',
                 'mean_c_p', 'mean_c_n', 'mean_n_p')
  gpp_data = annual_data %>%
    select(c('mean_gpp', predictors)) %>%
    na.omit()
  gpp_corr_matrix = gpp_data %>% as.matrix() %>% Hmisc::rcorr()

  global_model_gpp = lm(mean_gpp ~ ., data = gpp_data)
  # testing for multicollinearity
  gpp_vif = car::vif(global_model_gpp)
  # high variance inflation factor with all the predictor variables; so let's take out
  #  the predictors with the highest values; in general, we want VIF to be below
  #  5 for all variables - less than 2.5 would be great
  car::vif(lm(mean_gpp ~ ., data = select(gpp_data, -mean_n, -mean_c_p, -mean_c_n)))
  reduced_gpp_data = select(gpp_data, -mean_n, -mean_c_p, -mean_c_n)

  global_model_gpp = lm(mean_gpp ~ ., data = reduced_gpp_data)

  all_gpp_mods = dredge(global_model_gpp, extra = 'R^2') %>%
    # only keeping models with delta AIC less than or equal to 2
    dplyr::filter(delta <= 2) %>% as_tibble()
  all_gpp_mods = mutate(all_gpp_mods, lakes = nrow(gpp_data))
  gpp_p_1 = lmp(lm(mean_gpp ~ mean_p, data = reduced_gpp_data))
  gpp_p_2 = lmp(lm(mean_gpp ~ mean_p + mean_c, data = reduced_gpp_data))
  all_gpp_mods = mutate(all_gpp_mods,
                        pval = c(gpp_p_1, gpp_p_2))

  # ER
  r_data = annual_data %>%
    select(c('mean_r',predictors)) %>%
    na.omit()

  global_model_r = lm(mean_r ~ ., data = r_data)
  reduced_r_data = select(r_data, -mean_n, -mean_c_p, -mean_c_n)

  global_model_r = lm(mean_r ~ ., data = reduced_r_data)

  all_r_mods = dredge(global_model_r, extra = 'R^2') %>%
    dplyr::filter(delta <= 2) %>% as_tibble()
  all_r_mods = mutate(all_r_mods, lakes = nrow(r_data))
  r_p_1 = lmp(lm(mean_r ~ mean_p, data = reduced_r_data))
  all_r_mods = mutate(all_r_mods,
                        pval = r_p_1)

  # NEP
  nep_data = annual_data %>%
    select(c('mean_nep',predictors)) %>%
    na.omit()

  global_model_nep = lm(mean_nep ~ ., data = nep_data)
  reduced_nep_data = select(nep_data, -mean_n, -mean_c_p, -mean_c_n)

  global_model_nep = lm(mean_nep ~ ., data = reduced_nep_data)

  all_nep_mods = dredge(global_model_nep, extra = 'R^2') %>%
    dplyr::filter(delta <= 2) %>% as_tibble()
  all_nep_mods = mutate(all_nep_mods, lakes = nrow(nep_data))
  nep_p_1 = lmp(lm(mean_nep ~ mean_n_p, data = reduced_nep_data))
  nep_p_2 = lmp(lm(mean_nep ~ mean_n_p + mean_p, data = reduced_nep_data))
  all_nep_mods = mutate(all_nep_mods,
                        pval = c(nep_p_1, nep_p_2))

  all_out = bind_rows(all_gpp_mods, all_r_mods, all_nep_mods) %>%
    mutate(metab_response = c(rep('GPP',nrow(all_gpp_mods)),
                              rep('R', nrow(all_r_mods)),
                              rep('NEP', nrow(all_nep_mods))))

  saveRDS(all_out,
          out_file)

  return(out_file)
}


lm_load_vs_metab <- function(
  annual_data,
  out_file
){

  ## multi model selection based on AIC
  gpp_out = tibble()
  r_out = tibble()
  nep_out = tibble()

  # GPP
  options(na.action = 'na.fail')

  predictors = c('mean_c_load', 'mean_n_load', 'mean_p_load',
                 'mean_c_p_load', 'mean_c_n_load', 'mean_n_p_load')

  gpp_data = annual_data %>%
    select(c('mean_gpp', predictors)) %>%
    na.omit()
  gpp_corr_matrix = gpp_data %>% as.matrix() %>% Hmisc::rcorr()

  global_model_gpp = lm(mean_gpp ~ ., data = gpp_data)
  # testing for multicollinearity
  gpp_vif = car::vif(global_model_gpp)
  # high variance inflation factor with all the predictor variables; so let's take out
  #  the predictors with the highest values
  car::vif(lm(mean_gpp ~ ., data = select(gpp_data, -mean_n_load, -mean_c_p_load, -mean_c_n_load)))
  reduced_gpp_data = select(gpp_data, -mean_n_load, -mean_c_p_load, -mean_c_n_load)

  global_model_gpp = lm(mean_gpp ~ ., data = reduced_gpp_data)

  all_gpp_mods = dredge(global_model_gpp, extra = 'R^2') %>%
    # only keeping models with delta AIC less than or equal to 2
    dplyr::filter(delta <= 2) %>% as_tibble()
  all_gpp_mods = mutate(all_gpp_mods, lakes = nrow(gpp_data))
  gpp_p_1 = lmp(lm(mean_gpp ~ mean_n_p_load + mean_p_load + mean_c_load, data = reduced_gpp_data))
  all_gpp_mods = mutate(all_gpp_mods,
                        pval = gpp_p_1)

  # ER
  r_data = annual_data %>%
    select(c('mean_r',predictors)) %>%
    na.omit()

  global_model_r = lm(mean_r ~ ., data = r_data)
  reduced_r_data = select(r_data, -mean_n_load, -mean_c_p_load, -mean_c_n_load)

  global_model_r = lm(mean_r ~ ., data = reduced_r_data)

  all_r_mods = dredge(global_model_r, extra = 'R^2') %>%
    dplyr::filter(delta <= 2) %>% as_tibble()
  all_r_mods = mutate(all_r_mods, lakes = nrow(r_data))
  r_p_1 = lmp(lm(mean_r ~ mean_n_p_load + mean_p_load + mean_c_load, data = reduced_r_data))
  r_p_2 = lmp(lm(mean_r ~ mean_n_p_load + mean_p_load, data = reduced_r_data))
  all_r_mods = mutate(all_r_mods,
                        pval = c(r_p_1, r_p_2))

  # NEP
  nep_data = annual_data %>%
    select(c('mean_nep',predictors)) %>%
    na.omit()

  global_model_nep = lm(mean_nep ~ ., data = nep_data)
  reduced_nep_data = select(nep_data, -mean_n_load, -mean_c_p_load, -mean_c_n_load)

  global_model_nep = lm(mean_nep ~ ., data = reduced_nep_data)

  all_nep_mods = dredge(global_model_nep, extra = 'R^2') %>%
    dplyr::filter(delta <= 2) %>% as_tibble()
  all_nep_mods = mutate(all_nep_mods, lakes = nrow(nep_data))
  nep_p_1 = lmp(lm(mean_nep ~ mean_n_p_load, data = reduced_nep_data))
  nep_p_2 = summary(lm(mean_nep ~ 1, data = reduced_nep_data))$coefficient[4]
  all_nep_mods = mutate(all_nep_mods,
                        pval = c(nep_p_1, nep_p_2))

  all_out = bind_rows(all_gpp_mods, all_r_mods, all_nep_mods) %>%
    mutate(metab_response = c(rep('GPP',nrow(all_gpp_mods)),
                              rep('R', nrow(all_r_mods)),
                              rep('NEP', nrow(all_nep_mods))))

  saveRDS(all_out,
          out_file)

  return(out_file)
}


generate_aic_table_inlake <- function(
  aic_file,
  out_file
){
  aic <- readRDS(aic_file)

  table_out = aic %>%
    select('(Intercept)', mean_c, mean_p, mean_n_p, AICc, 'R^2', pval, metab_response) %>%
    mutate(across(c(1:4,6), ~ round_tab(.x, digits = 2)),
           across(c(5), ~ round_tab(.x, digits = 1)),
           across(c(7), ~ round_tab(.x, digits = 3))) %>%
    imputeTS::na_replace(fill = '') %>%
    mutate(metab_response = case_when(metab_response == 'GPP' ~ 'ln(GPP)',
                                      metab_response == 'R' ~ 'ln(-R)',
                                      TRUE ~ metab_response)) %>%
    relocate(metab_response) %>%
    select(-pval)

  out = sjPlot::tab_df(x = table_out, digits = 2,
               col.header = c('Response Variable', 'Intercept', 'DOC', 'ln(TP)',
                              'ln(TN:TP)', 'AIC', 'r²'),
               alternate.rows = T,
               file = out_file)

  print(out)

  return(out_file)
}

round_tab = function(x, digits){round(x, digits = digits)}


generate_aic_table_load <- function(
  aic_file,
  out_file
){
  aic <- readRDS(aic_file)

  table_out = aic %>%
    select('(Intercept)', mean_c_load, mean_p_load, mean_n_p_load,
           AICc, 'R^2', pval, metab_response) %>%
    mutate(across(c(1:4,6), ~ round_tab(.x, digits = 2)),
           across(c(5), ~ round_tab(.x, digits = 1)),
           across(c(7), ~ round_tab(.x, digits = 3))) %>%
    imputeTS::na_replace(fill = '') %>%
    mutate(metab_response = case_when(metab_response == 'GPP' ~ 'ln(GPP)',
                                      metab_response == 'R' ~ 'ln(-R)',
                                      TRUE ~ metab_response)) %>%
    relocate(metab_response) %>%
    select(-pval)

  out = sjPlot::tab_df(x = table_out, digits = 2,
               col.header = c('Response Variable', 'Intercept', 'ln(DOC Load)', 'ln(TP Load)',
                              'ln(TN:TP Load)', 'AIC', 'r²'),
               alternate.rows = T,
               file = out_file)

  print(out)

  return(out_file)
}



generate_annual_ave_table <- function(
    annual_data,
    label_id,
    out_file
){

  annual_data <- left_join(annual_data,
                           select(label_id, -mean_gpp), by = "lake")

  lake_names <- tribble(~lake, ~lake_name,
                        'Acton', 'Acton Lake',
                        'Crampton', 'Crampton Lake',
                        'EastLong', 'East Long Lake',
                        'Feeagh', 'Lough Feeagh',
                        'Harp', 'Harp Lake',
                        'Langtjern', 'Lake Langtjern',
                        'Lillinonah', 'Lake Lillinonah',
                        'Lillsjoliden', 'Lillsjölidtjärnen',
                        'Mangstrettjarn', 'Mångstrettjärn',
                        'Mendota', 'Lake Mendota',
                        'Morris', 'Morris Lake',
                        'Nastjarn', 'Nästjärn',
                        'Ovre', 'Övre Björntjärn',
                        'Struptjarn', 'Struptjärn',
                        'Trout', 'Trout Lake',
                        'Vortsjarv', 'Lake Võrtsjärv')

  annual_data <- left_join(annual_data, lake_names)

  round_tab = function(x, digits = 2){round(x, digits = digits)}

  # have to unscale all data
  table_out = annual_data %>%
    select(lake_name,
           mean_c_load, mean_n_load, mean_p_load,
           mean_c, mean_n, mean_p,
           mean_gpp, mean_r, mean_nep,
           label_id) %>%
    mutate(mean_c_load = exp(mean_c_load) * 12 * 1000 * 1000, # mg C m-3 (lake water) day-1
           mean_n_load = exp(mean_n_load) * 14 * 1000 * 1000 * 1000, # ug N m-3 (lake water) day-1
           mean_p_load = exp(mean_p_load) * 31 * 1000 * 1000 * 1000, # ug P m-3 (lake water) day-1
           mean_c = mean_c * 12, # mg C L-1
           mean_n = (-1 * mean_n) ^ -1.538462 * 14 * 1000, # ug N L-1
           mean_p = exp(mean_p) * 31 * 1000, # ug P L-1
           mean_gpp = exp(mean_gpp), # mg O2 L-1 day-1
           mean_r = exp(mean_r) * -1) %>% # mg O2 L-1 day-1
    mutate(across(c(mean_c_load, mean_n_load, mean_p_load,
                    mean_c, mean_n, mean_p),
                  ~ round_tab(.x, digits = 1))) %>%
    mutate(across(c(mean_gpp, mean_r, mean_nep),
                  ~ round_tab(.x, digits = 2))) %>%
    mutate(across(.fns = as.character)) %>%
    imputeTS::na_replace(fill = '')

  out = sjPlot::tab_df(x = table_out,
                       col.header = c("Lake",
                                      "DOC Load", "TN Load", "TP Load",
                                      "Lake DOC", "Lake TN", "Lake TP",
                                      "GPP", "R", "NEP",
                                      "Plot ID"),
                       alternate.rows = T,
                       file = out_file)

  print(out)

  return(out_file)
}

calc_metab_coupling <- function(
    annual_data
){

  out <- tribble(~var1, ~var2, ~cor, ~p_val,
                 "GPP", "R", cor(annual_data$mean_gpp, annual_data$mean_r), round(summary(lm(annual_data$mean_gpp~annual_data$mean_r))$coefficients[8], 4),
                 "GPP", "NEP", cor(annual_data$mean_gpp, annual_data$mean_nep), round(summary(lm(annual_data$mean_gpp~annual_data$mean_nep))$coefficients[8], 4),
                 "R", "NEP", cor(annual_data$mean_r, annual_data$mean_nep), round(summary(lm(annual_data$mean_r~annual_data$mean_nep))$coefficients[8], 4)
                 )

  return(out)
}




# need to be explicit about encoding when sourcing and trying to plot
#  special characters - see here https://github.com/ropensci/targets/issues/249#issuecomment-750423361
source('2_stats/src/stats_functions.R', encoding = 'UTF-8')

p2_targets <- list(

  # summarize to annual averages, check for normality, and transform if needed.
  #  All ratios are log transformed before calculating the annual average.
  tar_target(
    p2_annual_data,
    summarize_to_annual_and_normalize(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      metab_data = p0_metab_timeseries_data,
      season_data = p0_season_cutoff_data,
      config = p0_config
    )
  ),

  tar_target(
    p2_lm_inlake_vs_metab_rds,
    lm_inlake_vs_metab(
      annual_data = p2_annual_data,
      out_file = '2_stats/out/inlake_vs_metab_aic.rds'
    ),
    format = 'file'
  ),

  tar_target(
    p2_lm_load_vs_metab_rds,
    lm_load_vs_metab(
      annual_data = p2_annual_data,
      out_file = '2_stats/out/load_vs_metab_aic.rds'
    ),
    format = 'file'
  ),

  tar_target(
    p2_inlake_vs_metab_aic_table,
    generate_aic_table_inlake(
      aic_file = p2_lm_inlake_vs_metab_rds,
      out_file = '2_stats/out/inlake_vs_metab_aic.doc'
    )
  ),

  tar_target(
    p2_load_vs_metab_aic_table,
    generate_aic_table_load(
      aic_file = p2_lm_load_vs_metab_rds,
      out_file = '2_stats/out/load_vs_metab_aic.doc'
    )
  ),

  tar_target(
    p2_annual_average_supp_table,
    generate_annual_ave_table(
      annual_data = p2_annual_data,
      label_id = p1_lake_label,
      out_file = "2_stats/out/annual_ave_supp.doc"
    )
  ),

  tar_target(
    p2_metab_coupling,
    calc_metab_coupling(
      annual_data = p2_annual_data
    )
  )


)




# need to be explicit about encoding when sourcing and trying to plot
#  special characters - see here https://github.com/ropensci/targets/issues/249#issuecomment-750423361
source('1_plot/src/plotting_functions.R', encoding = 'UTF-8')

p1_targets <- list(

  # metabolism time series plot
  tar_target(
    p1_metab_timeseries_plot_png,
    plot_metab_timeseries(
      metab_data = p0_metab_timeseries_data,
      season_data = p0_season_cutoff_data,
      config = p0_config,
      out_file = '1_plot/out/metab_timeseries_plot.png'
    )
  ),

  tar_target(
    p1_stoich_load_timeseries_png,
    plot_stoich_timeseries(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      config = p0_config,
      out_file = '1_plot/out/stoich_timeseries_plot.png'
    )
  )

)


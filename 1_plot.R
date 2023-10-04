

# need to be explicit about encoding when sourcing and trying to plot
#  special characters - see here https://github.com/ropensci/targets/issues/249#issuecomment-750423361
source('1_plot/src/plotting_functions.R', encoding = 'UTF-8')

p1_targets <- list(

  tar_target(
    p1_lake_label,
    {
      metab_data = filter_metab_timeseries(metab_data = p0_metab_timeseries_data,
                                           season_data = p0_season_cutoff_data,
                                           config = p0_config)
      #ordering by mean GPP
      lakes_sorted <- mutate(metab_data, lake = as.character(lake)) %>%
        group_by(lake) %>%
        summarise(mean_gpp = mean(mean_gpp), .groups = "drop") %>%
        arrange(mean_gpp) %>%
        mutate(label_id = n():1)

      return(lakes_sorted)
    }
  ),

  # metabolism time series plot
  tar_target(
    p1_metab_timeseries_plot_png,
    plot_metab_timeseries(
      metab_data = p0_metab_timeseries_data,
      season_data = p0_season_cutoff_data,
      config = p0_config,
      out_file = "1_plot/out/fig_5_metab_timeseries_plot.tiff"
    ),
    format = "file"
  ),

  tar_target(
    p1_doc_timeseries_plot_png,
    plot_doc_timeseries(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      config = p0_config,
      out_file = "1_plot/out/fig_S3_doc_timeseries_plot.tiff"
    ),
    format = "file"
  ),

  tar_target(
    p1_tp_timeseries_plot_png,
    plot_tp_timeseries(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      config = p0_config,
      out_file = "1_plot/out/fig_S1_tp_timeseries_plot.tiff"
    ),
    format = "file"
  ),

  tar_target(
    p1_tn_timeseries_plot_png,
    plot_tn_timeseries(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      config = p0_config,
      out_file = "1_plot/out/fig_S2_tn_timeseries_plot.tiff"
    ),
    format = "file"
  ),
  tar_target(
    p1_stoich_load_timeseries_png,
    plot_stoich_timeseries(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      config = p0_config,
      out_file = '1_plot/out/fig_S4_stoich_timeseries_plot.tiff'
    ),
    format = 'file'
  ),

  tar_target(
    p1_stream_lake_nutrient_png,
    plot_stream_lake_nutrient(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      config = p0_config,
      label_id = p1_lake_label,
      out_file = '1_plot/out/fig_2_stream_lake_nutrient_1_1.tiff'
    ),
    format = 'file'
  ),

  tar_target(
    p1_stream_lake_stoich_scatter_png,
    plot_stream_lake_stoich_scatter(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      config = p0_config,
      label_id = p1_lake_label,
      out_file = '1_plot/out/fig_3_stream_lake_stoich_scatter.tiff'
    ),
    format = 'file'
  ),

  tar_target(
    p1_plot_stoich_stream_vs_lake_png,
    plot_stoich_stream_vs_lake(
      load_data = p0_load_timeseries_data,
      inlake_data = p0_inlake_nutrient_timeseries_data,
      config = p0_config,
      label_id = p1_lake_label,
      out_file = '1_plot/out/fig_4_plot_stoich_stream_vs_lake.tiff'
    )
  ),

  tar_target(
    p1_plot_obs_pred_metab_inlake_png,
    plot_obs_pred_metab_inlake(
      annual_data = p2_annual_data,
      label_id = p1_lake_label,
      out_file = '1_plot/out/fig_S5_plot_obs_pred_metab_inlake.tiff'
    )
  ),

  tar_target(
    p1_plot_obs_pred_metab_load_png,
    plot_obs_pred_metab_load(
      annual_data = p2_annual_data,
      label_id = p1_lake_label,
      out_file = '1_plot/out/fig_S6_plot_obs_pred_metab_load.tiff'
    )
  ),

  tar_target(
    p1_plot_gpp_r_png,
    plot_gpp_r(
      annual_data = p2_annual_data,
      out_file = '1_plot/out/plot_gpp_r.png'
    )
  ),

  tar_target(
    p1_plot_obs_resid_inlake_png,
    plot_obs_resid_metab_inlake(
      annual_data = p2_annual_data,
      out_file = '1_plot/out/plot_obs_resid_metab_inlake.png'
    )
  ),

  tar_target(
    p1_plot_obs_resid_load_png,
    plot_obs_resid_metab_load(
      annual_data = p2_annual_data,
      out_file = '1_plot/out/plot_obs_resid_metab_load.png'
    )
  ),

  tar_target(
    p1_plot_inlake_models_png,
    plot_models_inlake(
      annual_data = p2_annual_data,
      label_id = p1_lake_label,
      out_file = '1_plot/out/fig_6_inlake_models.tiff'
    )
  ),

  tar_target(
    p1_plot_load_models_png,
    plot_models_load(
      annual_data = p2_annual_data,
      label_id = p1_lake_label,
      out_file = '1_plot/out/fig_7_load_models.tiff'
    )
  ),

  tar_target(
    p1_plot_doc_gpp_png,
    plot_doc_gpp(
      annual_data = p2_annual_data,
      label_id = p1_lake_label,
      out_file = '1_plot/out/fig_S7_doc_gpp.tiff'
    )
  )
)


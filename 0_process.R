

source('0_process/src/process_functions.R', encoding = 'UTF-8')

p0_targets <- list(

  # this file holds important analysis info such as CV cutoff
  tar_target(
    p0_config,
    yaml.load_file('lib/cfg/analysis_cfg.yml')
  ),

  tar_target(
    p0_metab_dir,
    'results/metab/20161107/' # directory of metabolism data
  ),

  tar_target(
    p0_load_dir,
    'results/nutrient load/'
  ),

  tar_target(
    p0_metab_timeseries_data,
    collate_metab_timeseries(
      metab_dir = p0_metab_dir
    )
  ),

  tar_target(
    p0_season_cutoff_data,
    load_season_cutoff(
      in_file = 'results/z_scored_schmidt.rds'
    )
  ),

  tar_target(
    p0_load_timeseries_data,
    collate_load_timeseries(
      load_dir = p0_load_dir,
      config = p0_config,
      metab_data = p0_metab_timeseries_data,
      metadata_file = 'data/metadataLookUp.csv',
      season_cutoff_file = 'results/z_scored_schmidt.rds'
    )
  ),

  tar_target(
    p0_inlake_nutrient_timeseries_data,
    collate_inlake_nutrient_timeseries(
      inlake_nutrient_file = 'data/in_lake_nutrients/GLEON_nutrient_inlake.csv'
    )
  )

)

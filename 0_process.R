

source('0_process/src/process_functions.R')

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
    p0_metab_timeseries_data,
    collate_metab_timeseries(
      metab_dir = p0_metab_dir,
      config = p0_config
    )
  )

)

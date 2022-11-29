source("3_data_release/src/data_release_files.R", encoding = 'UTF-8')
source("R_code/metabolism_code/fillHoles.R", encoding = 'UTF-8')
source("R_code/metabolism_code/floorMins.R", encoding = 'UTF-8')

library(rLakeAnalyzer)

p3_targets <- list(

  tar_target(
    p3_load_metab_csv,
    load_metab_data_release(
      load_dir = p0_load_dir,
      config = p0_config,
      metab_data = p0_metab_timeseries_data,
      metadata_file = "data/metadataLookUp.csv",
      out_file = "3_data_release/out/load_metab_results.csv"
    ),
    format = "file"
  ),

  tar_target(
    p3_lakes,
    c("Acton", "Crampton", "EastLong", "Feeagh", "Harp", "Langtjern", "Lillinonah",
    "Lillsjoliden", "Mangstrettjarn", "Mendota", "Morris", "Nastjarn", "Ovre",
    "Struptjarn", "Trout", "Vortsjarv")
  ),

  tar_target(
    p3_metab_input_files_csv,
    metab_inputs_data_release(
      lake = p3_lakes,
      metab_input_dir = "data/metab_data",
      out_file = sprintf("3_data_release/out/%s_metab_inputs.csv", p3_lakes)
    ),
    pattern = map(p3_lakes),
    format = "file"
  ),

  tar_target(
    p3_metab_input_files_all_csv,
    {
      all_input_files <- read_csv(p3_metab_input_files_csv) %>% bind_rows()
      write_csv(all_input_files, "3_data_release/out/metab_inputs.csv")
      return("3_data_release/out/metab_inputs.csv")
    },
    format = "file"
  )

  # TODO: make one file for stream nutrient data - see dropbox folder
  # TODO: make one file for stream discharge data - see dropbox folder
  # TODO: put all files in google drive
  #

)

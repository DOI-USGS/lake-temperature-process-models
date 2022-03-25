source('4_visualize/src/plot_results.R')

p4 <- list(
  # Plots for each lake-gcm combo
  tar_target(
    p4_20yr_gcm_preds_ice,
    plot_20yr_gcm_preds_ice(p2_glm_uncalibrated_run_groups, 
                    outfile_template <- '4_visualize/out/Site_20yr_preds_ice_%s_%s.png'),
    pattern = map(p2_glm_uncalibrated_run_groups),
    format='file'),
  
  # Plots for each lake
  tar_target(
    p4_20yr_average_preds_ice,
    plot_20yr_average_preds_ice(p2_glm_uncalibrated_lake_groups,
                                outfile_template <- '4_Visualize/out/Site_20yr_preds_ice_%s.png'),
    pattern = map(p2_glm_uncalibrated_lake_groups),
    format = 'file'
  ),
  
  tar_target(
    p4_20yr_average_profiles,
    plot_20yr_average_profiles(p2_glm_uncalibrated_lake_groups,
                               plot_month_days = c('02-15','05-15','08-15','11-15'),
                               outfile_template <- '4_Visualize/out/Site_20yr_profiles_%s.png'),
    pattern = map(p2_glm_uncalibrated_lake_groups),
    format = 'file'
  ),
  
  # Plots for all lakes (diagnostic plots)
  tar_target(
    p4_temp_violin_gcms,
    plot_temp_violin_gcms(p2_glm_uncalibrated_run_groups,
                            outfile = '4_visualize/out/temperature_violin_gcms.png'),
    format='file'
  ),
  
  tar_target(
    p4_temp_violin_months,
    plot_temp_violin_months(p2_glm_uncalibrated_run_groups,
                             outfile = '4_visualize/out/temperature_violin_months.png'),
    format='file'
  )
)
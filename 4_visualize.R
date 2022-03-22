source('4_visualize/src/plot_results.R')

p4 <- list(
  # 
  tar_target(
    p4_20yr_preds,
    plot_20yr_preds(p2_glm_uncalibrated_run_groups, 
                    outfile_template <- '4_visualize/out/Site_20yr_preds_ice_%s_%s.png'),
    pattern = map(p2_glm_uncalibrated_run_groups),
    format='file'),
  
  tar_target(
    p4_temp_density_gcms,
    plot_temp_density_gcms(p2_glm_uncalibrated_run_groups,
                        outfile = '4_visualize/out/temperature_density_gcms.png'),
    format='file'
  ),
  
  tar_target(
    p4_temp_density_months,
    plot_temp_density_months(p2_glm_uncalibrated_run_groups,
                             outfile = '4_visualize/out/temperature_density_months.png'),
    format='file'
  ),
  
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
source('4_visualize/src/plot_results.R')
source('4_visualize/src/plot_data_utility_fxns.R')

p4 <- list(
  ### Lake-specific plots
  tar_target(p4_plot_site_ids,
             p1_lake_cell_tile_xwalk_df %>%
               filter(site_id %in% c('nhdhr_105567868','nhdhr_105569520', 'nhdhr_114336097', 'nhdhr_120019185','nhdhr_114544667')) %>%
               pull(site_id)),
  
  # Plots for each lake-gcm combo
  # Subset p2_glm_uncalibrated_run_groups to selected plotting site_ids
  tar_target(p4_subset_run_groups,
             p2_glm_uncalibrated_run_groups %>%
               filter(site_id %in% p4_plot_site_ids),
             iteration = 'group'),
  
  tar_target(
    p4_20yr_gcm_preds_ice,
    plot_20yr_gcm_preds_ice(p4_subset_run_groups,
                            outfile_template <- '4_visualize/out/Site_20yr_preds_ice_%s_%s.png'),
    pattern = map(p4_subset_run_groups),
    format='file'),
  
  # Plots for each lake
  # Subset p2_glm_uncalibrated_lake_groups to selected plotting site_ids
  tar_target(p4_subset_lake_groups,
             p2_glm_uncalibrated_lake_groups %>%
               filter(site_id %in% p4_plot_site_ids),
             iteration = 'group'),
  
  # Pull glm preds for all 6 GCMs for each of the plot site ids
  tar_target(p4_lake_glm_preds,
             get_site_preds(unique(p4_subset_lake_groups$site_id),
                            p4_subset_lake_groups$raw_meteo_fl, 
                            p4_subset_lake_groups$export_fl, 
                            p4_subset_lake_groups$gcm),
             pattern = map(p4_subset_lake_groups)),
  
  # Munge the glm preds to long format
  tar_target(p4_lake_glm_preds_long,
             munge_long(p4_lake_glm_preds),
             pattern = map(p4_lake_glm_preds)),
  
  tar_target(p4_lake_glm_mean_long,
             p4_lake_glm_preds_long %>%
               group_by(site_id, depth, doy, period) %>%
               summarize(mean_temp = mean(temperature, na.rm=TRUE)) %>%
               filter(!is.na(mean_temp)),
             pattern = map(p4_lake_glm_preds_long)),
  
  tar_target(
    p4_20yr_average_preds_ice,
    plot_20yr_average_preds_ice(p4_plot_site_ids,
                                p4_lake_glm_preds,
                                p4_lake_glm_preds_long,
                                p4_lake_glm_mean_long,
                                outfile_template <- '4_Visualize/out/Site_20yr_preds_ice_%s.png'),
    pattern = map(p4_plot_site_ids, p4_lake_glm_preds, p4_lake_glm_preds_long, p4_lake_glm_mean_long),
    format = 'file'
  ),
  
  tar_target(
    p4_20yr_average_profiles,
    plot_20yr_average_profiles(p4_plot_site_ids,
                               p4_lake_glm_preds_long,
                               p4_lake_glm_mean_long,
                               plot_month_days = c('02-15','05-15','08-15','11-15'),
                               outfile_template <- '4_Visualize/out/Site_20yr_profiles_%s.png'),
    pattern = map(p4_plot_site_ids, p4_lake_glm_preds_long, p4_lake_glm_mean_long),
    format = 'file'
  ),
  
  ### Diagnostic plots
  # Plots for all lakes
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
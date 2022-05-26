source('4_visualize/src/plot_results.R')
source('4_visualize/src/plot_data_utility_fxns.R')

p4 <- list(
  ### Site-specific plots
  # Manually specify subset of sites for which to generate site-specific plots
  # Setting up by filtering p1_lake_cell_tile_xwalk_df to keep site order consistent
  # with previously built targets
  tar_target(p4_plot_site_ids,
             p1_lake_cell_tile_xwalk_df %>%
               filter(site_id %in% c('nhdhr_105567868','nhdhr_105569520', 'nhdhr_114336097', 'nhdhr_120019185','nhdhr_114544667')) %>%
               pull(site_id)),
  
  # Plots for each lake-gcm combo
  # Subset p2_glm_uncalibrated_run_groups to selected plotting site_ids
  tar_target(p4_subset_gcm_run_groups,
             p2_gcm_glm_uncalibrated_run_groups %>%
               filter(site_id %in% p4_plot_site_ids)  %>%
               group_by(site_id, driver) %>% 
               tar_group(),
             iteration = 'group'),
  
  # Pull GLM preds for each lake-gcm combo for each of the plot site ids
  tar_target(p4_run_gcm_glm_preds,
             get_site_preds(p4_subset_gcm_run_groups),
             pattern = map(p4_subset_gcm_run_groups)),
  
  # Munge the GLM preds to long format
  tar_target(p4_run_gcm_glm_preds_long,
             munge_long(p4_run_gcm_glm_preds),
             pattern = map(p4_run_gcm_glm_preds)),
  
  # Get mean of GLM predictions for each lake-gcm combo for each DOY within 
  # each period, averaging across all years within each period
  tar_target(p4_run_gcm_glm_mean_long,
             p4_run_gcm_glm_preds_long %>%
               group_by(site_id, driver, depth, doy, period) %>%
               summarize(mean_temp = mean(temperature, na.rm=TRUE)) %>%
               filter(!is.na(mean_temp)),
             pattern = map(p4_run_gcm_glm_preds_long)),
  
  # Plot predicted surface, middle, and bottom temperatures in each 
  # lake, for each GCM
  tar_target(
    p4_20yr_gcm_preds_ice,
    plot_20yr_preds_ice(p4_run_gcm_glm_preds_long,
                        p4_run_gcm_glm_mean_long,
                        all_gcms = FALSE,
                        outfile_template <- '4_visualize/out/GCM_site_20yr_preds_ice_%s_%s.png'),
    pattern = map(p4_run_gcm_glm_preds, p4_run_gcm_glm_preds_long, p4_run_gcm_glm_mean_long),
    format='file'),
  
  # Plots for each lake
  # Subset p2_glm_uncalibrated_run_groups to selected plotting site_ids
  tar_target(p4_subset_gcm_lake_groups,
             p2_gcm_glm_uncalibrated_run_groups %>%
               filter(site_id %in% p4_plot_site_ids) %>%
               group_by(site_id) %>% 
               tar_group(),
             iteration = 'group'),
  
  # Pull GLM preds for all 6 GCMs for each of the plot site ids
  tar_target(p4_lake_gcm_glm_preds,
             get_site_preds(p4_subset_gcm_lake_groups),
             pattern = map(p4_subset_gcm_lake_groups)),
  
  # Munge the GLM preds to long format
  tar_target(p4_lake_gcm_glm_preds_long,
             munge_long(p4_lake_gcm_glm_preds),
             pattern = map(p4_lake_gcm_glm_preds)),
  
  # Get mean of GLM predictions for each lake for each DOY within each period,
  # averaging across all 6 GCMs and all years within each period
  tar_target(p4_lake_gcm_glm_mean_long,
             p4_lake_gcm_glm_preds_long %>%
               group_by(site_id, depth, doy, period) %>%
               summarize(mean_temp = mean(temperature, na.rm=TRUE)) %>%
               filter(!is.na(mean_temp)),
             pattern = map(p4_lake_gcm_glm_preds_long)),
  
  # Plot predicted surface, middle, and bottom temperatures in each 
  # lake, for all 6 GCMs
  tar_target(
    p4_20yr_average_gcm_preds_ice,
    plot_20yr_preds_ice(p4_lake_gcm_glm_preds_long,
                        p4_lake_gcm_glm_mean_long,
                        all_gcms = TRUE,
                        outfile_template <- '4_Visualize/out/GCM_site_20yr_preds_ice_%s.png'),
    pattern = map(p4_lake_gcm_glm_preds, p4_lake_gcm_glm_preds_long, p4_lake_gcm_glm_mean_long),
    format = 'file'
  ),
  
  # Plot predicted temperature profiles in a given lake, for all 6 GCMs,
  # across all years, for 4 specified dates
  tar_target(
    p4_20yr_average_gcm_profiles,
    plot_20yr_average_profiles(p4_plot_site_ids,
                               p4_lake_gcm_glm_preds_long,
                               p4_lake_gcm_glm_mean_long,
                               plot_month_days = c('02-15','05-15','08-15','11-15'),
                               outfile_template <- '4_Visualize/out/GCM_site_20yr_profiles_%s.png'),
    pattern = map(p4_plot_site_ids, p4_lake_gcm_glm_preds_long, p4_lake_gcm_glm_mean_long),
    format = 'file'
  ),
  
  ### Diagnostic plots
  # Plots for all lakes
  # Get surface, middle, and bottom predictions for all lakes, across all GCMs
  tar_target(
    p4_gcm_glm_preds,
    get_surface_middle_bottom_preds(p2_gcm_glm_uncalibrated_run_groups)
  ),
  
  # Create a violin chart of predicted surface, middle, and bottom 
  # temperatures in all lakes for all GCMs, faceted by driver (gcm name)
  tar_target(
    p4_gcm_temp_violin_gcms,
    plot_temp_violin(p4_gcm_glm_preds,
                     faceting_variable = 'driver',
                     outfile = '4_visualize/out/GCM_temperature_violin_gcms.png'),
    format='file'
  ),
  
  # Create a violin chart of predicted surface, middle, and bottom 
  # temperatures in all lakes for all GCMs, faceted by month
  tar_target(
    p4_gcm_temp_violin_months,
    plot_temp_violin(p4_gcm_glm_preds,
                     faceting_variable = 'month',
                     outfile = '4_visualize/out/GCM_temperature_violin_months.png'),
    format='file'
  )
)
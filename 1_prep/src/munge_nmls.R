#' @title Make nml edits
#' @description prepare edits that can be made the same way
#' for all lakes. Applies to one lake's nml list at at time
#' @param nml_args lake-specific nml parameters loaded in from
#' the nml_list.rds from lake-temperature-model-prep
#' @return a list of modified nml arguments
adjust_depth_args_nml <- function(nml_args) {
  nml_edits <- with(nml_args, c(
    # &glm_setup
    max_layers = max(30, ceiling(7 * lake_depth)),
    
    # &morphometry
    lake_name = site_id,
    resample_H_A(H, A),
    # bsn_vals = length(H),
    
    # &init_profiles
    tibble(
      the_depths = c(0, floor(lake_depth * 100)/100)
    )
  ))
  return(nml_edits)
}

#' @Title Munge default nml lists
#' @decription Replace the NLDAS filename stored as 'meteo_fl'
#' with 'NULL', remove used nml parameter 'site_id'
#' @param nml_list_rds rds file of lake-specific nml parameters
#' @param site_ids vector of lakes from lake_cell_xwalk
#' @param base_nml glm3 nml template
#' @return complete nml objects
munge_nmls <- function(nml_list_rds, site_ids, base_nml) {
  nml_list <- readr::read_rds(nml_list_rds)[site_ids]
  nml_template <- read_nml(base_nml)
  # create the munged nml objects
  nml_objs <- purrr::map(nml_list, function(nml) {
      # make edits to nml depth-related parameters
      nml <- purrr::list_modify(nml, !!!adjust_depth_args_nml(nml))
      # set meteo_fl value to NULL
      nml <- purrr::list_modify(nml, !!!c('meteo_fl' = 'NULL'))
      # disable evaporation
      nml <- purrr::list_modify(nml, !!!c('disable_evap' = TRUE))
      # remove helpful but non-nml values
      nml <- nml[!(names(nml) %in% c('site_id'))]
      # merge into base nml, checking arguments along the way
      nml_obj <- set_nml(nml_template, arg_list = nml)
    })
  
  return(nml_objs)
}

#' @Title Resample incoming hypsography to 1-m intervals
#' @decription Resample the H and A values to 1-meter intervals; calc bsn_vals
#' @param H the vector of H (elevation) values from the lake-specific 
#' nml parameters loaded in from the nml_list.rds from lake-temperature-model-prep
#' @param A the vector of A (area) values from the lake-specific nml 
#' parameters loaded in from the nml_list.rds from lake-temperature-model-prep
#' @return a list containing H and A vectors and a bsn_vals scaler
resample_H_A <- function(H, A) {
  ha_df <- tibble(H = H,A = A) %>%
    arrange(H)
  
  # Resample hypso to 1-meter intervals
  # Add an additional row for the deepest (final) raw H value so we don’t 
  # end up with with a lake shallower than the lake depth param. 
  # Then remove duplicate rows if any exist, which they will if the
  # final H value from the raw H vector is an integer
  ha_df_resampled <- bind_rows(tibble(H=seq.int(floor(min(ha_df$H)), floor(max(ha_df$H))),
                  A=approx(ha_df$H, ha_df$A, xout=seq.int(floor(min(ha_df$H)), floor(max(ha_df$H))), rule=2)$y),
                  tibble(A=approx(H, A, xout=max(ha_df$H), rule=2)$y, H=max(ha_df$H))) %>%
                  distinct()

  c(ha_df_resampled, bsn_vals = nrow(ha_df_resampled))
}

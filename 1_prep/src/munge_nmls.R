#' @Title Make nml edits
#' @description prepare edits that can be made the same way
#' for all lakes. Applies to one lake's nml list at at time
#' @param nml_args lake-specific nml parameters loaded in from
#' the nml_list.rds from lake-temperature-model-prep
#' @return a list of modified nml arguments
make_nml_edits <- function(nml_args) {
  nml_edits <- with(nml_args, c(
    # &morphometry
    lake_name = site_id,
    
    # &init_profiles
    tibble(
      the_depths = c(0, floor(lake_depth * 100)/100)
    ),
    
    # &meteorology
    meteo_fl = 'NULL'
  ))
  
  return(nml_edits)
}

#' @Title Munge default nml lists
#' @decription Replace the NLDAS filename stored as 'meteo_fl'
#' with 'NULL', remove used nml parameter 'site_id'
#' @param nml_list_rds rds file of lake-specific nml parameters
#' @param lake_ids vector of lakes from lake_cell_xwalk
#' @param base_nml glm3 nml template
#' @return complete nml objects
munge_nmls <- function(nml_list_rds, lake_ids, base_nml) {
  nml_list <- readr::read_rds(nml_list_rds)[lake_ids]
  
  # create the munged nml objects
  nml_objs <- purrr::map(nml_list, function(nml) {
      # set meteo_fl to NULL, since default value = NLDAS filename
      # and set depths based on lake depth
      nml <- purrr::list_modify(nml, !!!make_nml_edits(nml))
      # remove helpful but non-nml values
      nml <- nml[!(names(nml) %in% c('site_id'))]
      
      # merge into base nml, checking arguments along the way
      nml_template <- read_nml(base_nml)
      nml_obj <- set_nml(nml_template, arg_list = nml)
    })
  
  return(nml_objs)
}
# ncdfgeom function https://github.com/USGS-R/ncdfgeom/blob/main/R/0_ncdfgeom.R
# NOT MODIFIED
add_var <- function(nc, name, dim, type, units = NA, missing = NA, long_name = NA, char_dim_len = NULL, data = NULL) {
  
  if(type == "NC_CHAR") {
    suppressWarnings(if(is.null(char_dim_len) & is.null(data)) stop("can't determine character dim length"))
    if(is.null(char_dim_len)) suppressWarnings(char_dim_len <- max(sapply(data, function(x) max(nchar(x), 
                                                                                                na.rm = TRUE)), 
                                                                   na.rm = TRUE))
    char_dim <- paste0(name,"_char")
    dim.def.nc(nc, char_dim, char_dim_len, unlim = FALSE)
    dim <- c(char_dim, dim)
  }
  var.def.nc(nc, name, type, dim)
  if(!any(is.na(units)))
    att.put.nc(nc, name, "units", "NC_CHAR", units)
  if(!is.na(missing))
    att.put.nc(nc, name, "missing_value", type, missing)
  if(!is.na(long_name))
    att.put.nc(nc, name, "long_name", "NC_CHAR", long_name)
}
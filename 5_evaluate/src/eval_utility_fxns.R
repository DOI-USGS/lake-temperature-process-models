# matching fxn
match_pred_obs <- function() {
  
}

# bias fxn
calc_bias <- function(pred_obs, grouping_var) {
  pred_obs %>%
    group_by(!!grouping_var) %>%
    summarize(bias = median(pred_diff))
}

# RMSE fxn
calc_rmse <- function(pred_obs, grouping_var) {
  pred_obs %>%
    group_by(!!grouping_var) %>%
    summarize(rmse = sqrt(mean((pred_diff)^2, na.rm=TRUE)))
}

# plot fxn
plot_barplot <- function(plot_df, y_var, x_var, depth, outfile) {
  
  ggsave(filename=outfile, plot=bar_plot, dpi=300, width=10, height=6)
  return(outfile)
}
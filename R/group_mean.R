group_mean <-function(data, group_var, mean_var) {
  if(!is.numeric(data[[deparse(substitute(mean_var))]])) {
    stop('Sorry, this function requires a numeric variable')
  }
  data %>%
    group_by({{group_var}}) %>%
    summarise(mean = mean({{mean_var}}, na.rm = TRUE))
}

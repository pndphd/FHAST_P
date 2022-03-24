survival_benefit_params  <-  list(
  funcs = c(process_cover_data, process_temp_data, process_length_data),
  variables = c('temperature', 'length', 'cover'),
  var_ranges = list(temp = seq(0,30,0.01),
                 length = seq(0, 100, 0.01),
                 cover = seq(0,1,0.001))
)
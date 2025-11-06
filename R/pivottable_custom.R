#' Create pivot table
#'

createPivotTable <- function(df, row_var, col_var) {
  
  pivot_data <- df |>
    tidyr::pivot_longer(cols = matches(value_var),
                 names_to = col_var,
                 values_to = "Value") |>
    tidyr::pivot_wider(names_from = col_var,
                values_from = "Value")
  
  return(pivot_data)
}

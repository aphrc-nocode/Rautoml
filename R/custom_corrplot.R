#' Custom Correlation Plot using DataExplorer and ColorBrewer
#'
#' @description Generates a correlation plot for selected features of a dataframe,
#' with a specified ColorBrewer palette applied to a continuous fill scale.
#'
#' @param df A data frame containing the data.
#' @param features A character vector specifying the names of features to include.
#' @param colorbrewer A string specifying the ColorBrewer palette to use (default is "Dark2").
#'
#' @return A ggplot2 object representing the correlation plot. If input is invalid, an empty plot is returned.
#' @export
#'
#' @examples
#' custom_corrplot(df = mtcars, features = c("mpg", "hp", "wt"), colorbrewer = "YlGnBu")
custom_corrplot <- function(df = NULL, features = NULL, colorbrewer = "Dark2"){
  if (!is.null(df) & !is.null(features) & length(features) > 1) {
    colors <- colorRampPalette(RColorBrewer::brewer.pal(9, colorbrewer))(100)
    p <- DataExplorer::plot_correlation(
      data = df[, features],
      type = "all",
      ggtheme = ggplot2::theme_minimal()
    ) + ggplot2::scale_fill_gradientn(colors = colors)
  } else {
    p <- ggplot2::ggplot() + ggplot2::theme_minimal()
  }
  return(p)
}

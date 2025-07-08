#' Bivariate Plot using GGally with ColorBrewer
#'
#' @description Creates a bivariate plot of one outcome variable against multiple features
#' using `ggbivariate` from the GGally package and applies a ColorBrewer palette.
#'
#' @param df A data frame containing the variables.
#' @param outcome A string specifying the outcome (dependent) variable name.
#' @param features A character vector of explanatory (independent) variable names.
#' @param title A string title for the plot.
#' @param colorbrewer A string specifying the ColorBrewer palette to use (default is "Dark2").
#'
#' @return A ggplot2 object with the bivariate plots. Returns an empty plot if outcome or features are not provided.
#' @export
#'
#' @examples
#' bivariate_plot(df = mtcars, outcome = "mpg", features = c("hp", "wt"), title = "MPG vs Features")
bivariate_plot <- function(df, outcome = NULL, features = NULL,
                           title = "", colorbrewer = "Dark2", custom_theme = theme_minimal()){
  if (!is.null(outcome) && !is.null(features)) {
    p <- ggbivariate(df, outcome = outcome, explanatory = features, title = title) +
      custom_theme +
      ggplot2::scale_fill_brewer(palette = colorbrewer) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 18, hjust = 0.5))
  } else {
    p <- ggplot2::ggplot() + custom_theme
  }
  
  return(p)
}

#mtcars1 <- mtcars
#mtcars1$gear <- as.factor(mtcars1$gear)
#mtcars1$am <- as.factor(mtcars1$am)
#bivariate_plot(df = mtcars1, outcome = "am", features = names(mtcars1)[names(mtcars1)!="am"])
#bivariate_plot(df = mtcars1, outcome = "am", features = names(mtcars1)[names(mtcars1)!="am"], colorbrewer = "Accent")
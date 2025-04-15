#' Customizable Boxplot Function
#'
#' This function creates a customizable boxplot using ggplot2.
#'
#' @param df A data frame containing the data.
#' @param xvar Optional. A categorical variable for the x-axis.
#' @param yvar Required. A numerical variable for the y-axis.
#' @param colorVar Optional. A categorical variable for color grouping.
#' @param plot_title Optional. Title for the plot.
#' @param vertical Logical. If TRUE (default), plots a vertical boxplot; otherwise, a horizontal one.
#' @param xlab Optional. Label for the x-axis.
#' @param ylab Optional. Label for the y-axis.
#' @param title_pos Numeric. Position of the title (default is 0.5).
#' @param title_size Integer. Font size of the title (default is 25).
#' @param default_col Character. Default fill color (default is "#0077B6").
#' @param axis_title_size Integer. Font size for axis titles (default is 20).
#' @param axis_text_size Integer. Font size for axis text (default is 16).
#' @param axistext_angle Numeric. Angle of x-axis text labels (default is 0).
#' @param legend_title Character. Title of the legend (default is "").
#' @param colorbrewer Character. Color palette for fill aesthetics (default is "Dark2").
#'
#' @return A ggplot2 object representing the boxplot.
#' @export
#'
#' @examples
#' df <- data.frame(Category = rep(c("A", "B"), each = 50), Value = rnorm(100))
#' boxplot_custom(df, xvar = "Category", yvar = "Value")

custom_boxplot <- function(df, xvar = NULL, yvar, colorVar = NULL, plot_title = NULL, 
                           vertical = TRUE, xlab = NULL, ylab = NULL, title_pos = 0.5, 
                           title_size = 25, default_col = "#0077B6", axis_title_size = 20, 
                           axis_text_size = 16, axistext_angle = 0, legend_title = "", 
                           colorbrewer = "Dark2") {
  
  vars_col <- c(xvar, yvar, colorVar)
  existing_vars <- c()
  for (var in vars_col) {
    if (var %in% names(df)) {
      existing_vars <- c(existing_vars, var)
    }
  }
  
  df <- na.omit(df[, existing_vars])
  
  if (vertical == TRUE) {
    if ((is.null(xvar) || xvar == "") && (is.null(colorVar) || colorVar == "")) {
      df1 <- as.data.frame(df[, c(yvar)])
      names(df1) <- c("yvar1")
      df1$yvar1 <- as.numeric(df1$yvar1)
      
      p <- ggplot(data = df1, aes(y = yvar1), fill = default_col) +
        geom_boxplot(position = "dodge2", fill = default_col) +
        stat_boxplot(geom = "errorbar")
    } else if ((!is.null(xvar) && xvar != "") && (is.null(colorVar) || colorVar == "")) {
      df1 <- as.data.frame(df[, c(yvar, xvar)])
      names(df1) <- c("yvar1", "xvar1")
      df1$yvar1 <- as.numeric(df1$yvar1)
      df1$xvar1 <- as.factor(df1$xvar1)
      
      p <- ggplot(data = df1, aes(y = yvar1, x = xvar1)) +
        geom_boxplot(position = "dodge2", fill = default_col) +
        stat_boxplot(geom = "errorbar")
    } else if ((!is.null(colorVar) && colorVar != "") && (is.null(xvar) || xvar == "")) {
      df1 <- as.data.frame(df[, c(yvar, colorVar)])
      names(df1) <- c("yvar1", "colorvar1")
      df1$yvar1 <- as.numeric(df1$yvar1)
      df1$colorvar1 <- as.factor(df1$colorvar1)
      
      p <- ggplot(data = df1, aes(y = yvar1, fill = colorvar1)) +
        geom_boxplot(position = "dodge2") +
        stat_boxplot(geom = "errorbar") +
        guides(fill = guide_legend(title = ifelse(is.null(legend_title), "Legend", legend_title)))
    } else if ((!is.null(colorVar) && colorVar != "") && (!is.null(xvar) && xvar != "")) {
      df1 <- as.data.frame(df[, c(xvar, yvar, colorVar)])
      names(df1) <- c("xvar1", "yvar1", "colorvar1")
      df1$yvar1 <- as.numeric(df1$yvar1)
      df1$colorvar1 <- as.factor(df1$colorvar1)
      df1$xvar1 <- as.factor(df1$xvar1)
      
      p <- ggplot(data = df1, aes(y = yvar1, x = xvar1, fill = colorvar1)) +
        geom_boxplot(position = "dodge2") +
        stat_boxplot(geom = "errorbar") +
        guides(fill = guide_legend(title = ifelse(is.null(legend_title), "Legend", legend_title)))
    }
  }
  else{
    if ((is.null(xvar) || xvar == "") && (is.null(colorVar) || colorVar == "")) {
        df1 <- as.data.frame(df[, c(yvar)])
        names(df1) <- c("yvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        
        p <- ggplot(data = df1, aes(y = yvar1), fill = default_col) +
          geom_boxplot(position = "dodge2", fill = default_col) +
          stat_boxplot(geom = "errorbar")+coord_flip()
      } else if ((!is.null(xvar) && xvar != "") && (is.null(colorVar) || colorVar == "")) {
        df1 <- as.data.frame(df[, c(yvar, xvar)])
        names(df1) <- c("yvar1", "xvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$xvar1 <- as.factor(df1$xvar1)
        
        p <- ggplot(data = df1, aes(y = yvar1, x = xvar1)) +
          geom_boxplot(position = "dodge2", fill = default_col) +
          stat_boxplot(geom = "errorbar")+coord_flip()
      } else if ((!is.null(colorVar) && colorVar != "") && (is.null(xvar) || xvar == "")) {
        df1 <- as.data.frame(df[, c(yvar, colorVar)])
        names(df1) <- c("yvar1", "colorvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$colorvar1 <- as.factor(df1$colorvar1)
        
        p <- ggplot(data = df1, aes(y = yvar1, fill = colorvar1)) +
          geom_boxplot(position = "dodge2") +
          stat_boxplot(geom = "errorbar") +
          guides(fill = guide_legend(title = ifelse(is.null(legend_title), "Legend", legend_title)))+coord_flip()
      } else if ((!is.null(colorVar) && colorVar != "") && (!is.null(xvar) && xvar != "")) {
        df1 <- as.data.frame(df[, c(xvar, yvar, colorVar)])
        names(df1) <- c("xvar1", "yvar1", "colorvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$colorvar1 <- as.factor(df1$colorvar1)
        df1$xvar1 <- as.factor(df1$xvar1)
        
        p <- ggplot(data = df1, aes(y = yvar1, x = xvar1, fill = colorvar1)) +
          geom_boxplot(position = "dodge2") +
          stat_boxplot(geom = "errorbar") +
          guides(fill = guide_legend(title = ifelse(is.null(legend_title), "Legend", legend_title)))+coord_flip()
      }
    }
  
  
  p <- p + theme_minimal() +
    xlab(ifelse(!is.null(xlab), xlab, "")) +
    ylab(ifelse(!is.null(ylab), ylab, "")) +
    ggtitle(ifelse(!is.null(plot_title), plot_title, "")) +
    theme(
      plot.title = element_text(hjust = title_pos, size = title_size, colour = "black"),
      plot.caption = element_text(hjust = 1, color = "#808080"),
      axis.text.x = element_text(size = axis_text_size, colour = "black", angle = axistext_angle),
      axis.text.y = element_text(size = axis_text_size, colour = "black"),
      axis.title = element_text(size = axis_title_size, colour = "black"),
      legend.position = ifelse(is.null(colorVar), "none", "bottom")
    ) +
    scale_fill_brewer(palette = colorbrewer)
  
  return(p)
}

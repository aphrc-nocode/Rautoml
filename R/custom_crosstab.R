#' Table Summary Function
#'
#' This function generates a table summary using the gtsummary package.
#'
#' @param df A data frame containing the data.
#' @param vars Character vector of variables to summarize.
#' @param by Optional. A categorical variable to group by.
#' @param add.p Logical. If TRUE (default), adds p-values.
#' @param add.ci Logical. If TRUE, adds confidence intervals.
#' @param report_numeric Character. Method to report numeric data ("mean" or "median").
#' @param numeric_summary Character. Summary method for numeric data ("sd" or "min-max").
#' @param drop_na Logical. If TRUE, drops NA values.
#' @param caption Optional. Table caption.
#'
#' @return A gtsummary table object.
#' @import gtsummary
#' @import dplyr
#' @export
#'
#' @examples
#' df <- data.frame(Group = rep(c("A", "B"), each = 50), Value = rnorm(100))
#' tablefun(df, vars = "Value", by = "Group")

custom_crosstab <- function(df, vars, by = NULL, strata = NULL, add.p = TRUE, add.ci = FALSE, 
                            report_numeric = c("mean", "median"), numeric_summary = c("sd", "min-max"), 
                            drop_na = FALSE, caption = NULL) {
  
  vars_col <-  c(vars, strata, by)
  existing_vars <- c()
  for(var in vars_col) {
    if(var %in% names(df)) {
      existing_vars <- c(existing_vars, var)
    }
  }
  
  df <- na.omit(df[, existing_vars, drop = FALSE])
  report_numeric = match.arg(report_numeric)
  numeric_summary = match.arg(numeric_summary)
  if (add.ci) {
    if (report_numeric=="mean") {
      statistic = list(all_continuous() ~ "{mean}", all_categorical() ~ "{p}%")
    } else {
      statistic = list(all_continuous() ~ "{median}", all_categorical() ~ "{p}%")
    }
  } else {
    if (report_numeric=="mean") {
      if (numeric_summary=="sd") {
        statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)")
      } else {
        statistic = list(all_continuous() ~ "{mean} [{min, max}]", all_categorical() ~ "{n} ({p}%)")
      }
    } else {
      if (numeric_summary=="sd") {
        statistic = list(all_continuous() ~ "{median} ({sd})", all_categorical() ~ "{n} ({p}%)")
      } else {
        statistic = list(all_continuous() ~ "{median} [{min, max}]", all_categorical() ~ "{n} ({p}%)")
      }
    }
  }
  
    allvars <- c(by, vars)

  #print(allvars)
  if (is.null(caption)) {
    caption <- paste0(names(df[vars[1]]), " by ", names(df[by]))
  }
  tab <- (df
          %>% ungroup()
          %>% select(all_of(allvars))
  )
  if (drop_na) {
    tab <- (tab
            %>% filter(., if_any(all_of(vars), function(x)!is.na(x)))
    )
  }
  tab <- (tab
          %>% sjmisc::to_label(drop.levels=TRUE)
  )
  
  ttfun <- function(tab) {
    tab <- (tab	
            %>% tbl_summary(
              by = all_of(by)
              , statistic = statistic
            )
    )
    if (add.ci) {
      tab <- (tab
              %>% add_ci(pattern = "{stat} ({ci})")
      )
    }
    if (add.p==TRUE && !is.null(by) && by != "") {
      tab <- (tab 
              %>% add_p()
      )
    }
    if (!is.null(by) && by !="") {
      tab <- (tab
              %>% add_overall()
      )
    }
    tab <- (tab
            %>% modify_header(label = "**Variable**")
            %>% modify_caption(paste0("**", caption, "**"))
    )
    return(tab)
  }
  
    tab <- (tab
            %>% ttfun()
    )
  return(tab)
}

#custom_crosstab(df=mtcars, vars = c("wt", "disp", "cyl"), by = "qsec")



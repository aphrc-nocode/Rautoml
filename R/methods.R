#' Utilities to upload various data formats
#'
#' This function uses the file extension to determine data type and read data appropriately.
#'
#' @param path data path or url.
#'
#' @return a dataframe.
#'
#' @export

upload_data = function(path)UseMethod("upload_data")

#' Utilities saves data frame into various file formats
#'
#' This function uses the file extension to determine data type and save data appropriately into the local disk.
#'
#' @param df dataframe.
#' @param path data path or url.
#'
#' @return NULL.
#'
#' @export

write_data = function(path, df)UseMethod("write_data")


#' Collect model summary
#'
#' @details This function extracts and summarizes model estimates from careList object.
#'
#' @param models Models from caretList
#' @param summary_fun a function to summarize metrics
#'
#'
#' @export
#'

extract_summary = function(models, summary_fun)UseMethod("extract_summary")


#' Create ensemble model
#'
#' @export
#'

create_ensemble = function(all.models, excluded_class_id = 0L, tuneLength = 100, ctrl=NULL, metric=NULL)UseMethod("create_ensemble")


#' Compute predicitve metrics for multiple caret models
#'
#' @export 

boot_estimates_multiple = function(models, df, outcome_var, problem_type, nreps, model_name, type, report, summary_fun)UseMethod("boot_estimates_multiple")

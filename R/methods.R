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

boot_estimates_multiple = function(models, df, outcome_var, problem_type, nreps, model_name, type, report, summary_fun, save_model, model_id, model_folder, recipe_folder, preprocesses)UseMethod("boot_estimates_multiple")


#' Generate confussion matrix and variable importance for caretEnsemble
#'
#' @export 

post_model_metrics = function(models, outcome, df=NULL, task=NULL)UseMethod("post_model_metrics")

#' Get trained model metrics
#'
#' @export 
#'

get_metrics_names = function(object)UseMethod("get_metrics_names")


#' Extract sprcific metric for a particular model
#'
#' @export 
#'

extract_more_metrics = function(object, model_name, metric_name)UseMethod("extract_more_metrics")

#' Compute SHAP values for several caretEnsemble models
#'
#' @export

compute_shap = function(models, model_names=NULL, newdata, response, task, nsim = 50, max_n=1000, top_n_rank=5, total_n_rank=20)UseMethod("compute_shap")


#' Compute calibration data for caretList or caretEnsemble models
#'
#' @details For classification models, computes binned calibration
#'   (reliability) data -- mean predicted probability vs. observed event rate
#'   per bin, with Wilson score confidence intervals. For regression models,
#'   computes the same binned structure using raw predictions against the
#'   observed outcome, with a Student-t interval per bin instead (the same
#'   formula as `student_t_summary()`). Either way the result also carries
#'   the raw predictions used for a smoothed calibration curve. Problem type
#'   is read from `model$modelType` unless overridden with `problem_type`.
#'   For `caretEnsemble` models the stacked ensemble is included alongside
#'   its base learners, following the same convention as `compute_shap()`.
#'
#' @param models a `caretList` or `caretEnsemble` object
#' @param df test/validation data.frame containing predictors and the outcome
#' @param outcome_var name of the outcome column in `df`
#' @param n_bins number of calibration bins (default 10)
#' @param strategy `"quantile"` (equal-count bins, default) or `"uniform"` (equal-width bins)
#' @param type passed to `predict.train(..., type=)` for classification models
#' @param problem_type `"Classification"` or `"Regression"`; if `NULL`
#'   (default), auto-detected per model from `model$modelType`
#'
#' @export

compute_calibration = function(models, df, outcome_var, n_bins=10, strategy=c("quantile", "uniform"), type="prob", problem_type=NULL)UseMethod("compute_calibration")


#' Extract hyperparameters
#'
#'
#' @export 
#'

get_tuned_params = function(models)UseMethod("get_tuned_params")

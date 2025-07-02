#' Start cluster for parallel processing
#'
#' @export

start_cluster = function() {
  Rcl_xxx_ <<- parallel::makeCluster(detectCores() - 1)
  doParallel::registerDoParallel(Rcl_xxx_)
}

#' Stop cluster 
#'
#' @details `start_cluster()` must be running.
#'
#' @export

stop_cluster = function() {
  parallel::stopCluster(Rcl_xxx_)
  foreach::registerDoSEQ()
}

#' Setup model list 
#'
#' Needed for trainig multiple models using caret list
#'
#' @importFrom caret trainControl
#'
#' @export
#'

setup_caret = function(model, param, param_set=NULL) {
  if (isTRUE(param) & (isTRUE(!is.null(param))) & isTRUE(!is.null(param_set))) {
    model_name = list(
      model_name = caretEnsemble::caretModelSpec(
        method = model
        , tuneGrid = do.call("expand.grid", param_set)
      )
    )
  } else {
    model_name = list(caretEnsemble::caretModelSpec(model))
  }
  names(model_name) = model
  return(model_name)
}

#' Train multiple caret models using caret list
#'
#'
#' @export
 
train_caret_models = function(df, model_form, ctrl, model_list, metric) {
  ctrl = do.call(caret::trainControl, ctrl)
  model = caretEnsemble::caretList(
    model_form,
    data = df,
    trControl = ctrl,
    tuneList = model_list,
    continue_on_fail = TRUE,
    metric = metric
  )
  return(model)
}

#' Create ensemble model
#'
#' @export
#'

create_ensemble.caretList = function(all.models, excluded_class_id = 0L, tuneLength = 100, ctrl=NULL, metric=NULL) {
   ctrl = do.call(caret::trainControl, ctrl)
	ens_model = caretEnsemble::caretEnsemble(
		all.models = all.models
		, excluded_class_id = excluded_class_id
		, tuneLength = tuneLength
		, trControl = ctrl
		, metric = metric
	)
	return(ens_model)
}

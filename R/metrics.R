#' Generate prediction prototype data
#'
#' @param df data.frame used to train the model
#' @param outcome_var outcome variable
#'
#' @export
#'

create_pred_prototype = function(df, outcome_var=NULL) {
  if (isTRUE(!is.null(outcome_var))) {
    df = (df
      |> dplyr::select(-dplyr::all_of(outcome_var))
    )
  }
  x = sapply(df, function(x){
    l = get_type(x)
    if (any(l %in% c("factor", "character"))) {
      m = levels(as.factor(x))
    } else if (l == "numeric") {
      m = mean(x, na.rm=TRUE)
    } else if (l == "logical") {
      m = c(TRUE, FALSE)
    } else {
      m = x[[1]]
    }
    return(m)
  }, simplify = FALSE)
  return(x)
}

#' Use endpoint to predict
#'
#'
#' @export

predict_endpoint = function(url, new_data, recipes=NULL, model_name=model) {
  if (!is.null(recipes)) {
    new_data = preprocess_new_data(recipes, new_data)
  }
  x = lapply(1:length(url), function(i) {
  	  u = url[i]
	  if (!grepl("\\/predict", u, ignore.case = TRUE)) {
		 u = paste0(u, "/predict")
	  }
	  endpoint = vetiver::vetiver_endpoint(u)
	  pred = predict(endpoint, new_data)
	  pred = cbind.data.frame(pred, new_data)
	  pred[["model_name"]] = model_name[i]
	  pred = pred[, union(c("model_name", "value"), colnames(pred))]
	  return(pred)
  })
  x = do.call("rbind", x)
  return(x)
}

#' Get endpoint objects or data
#'
#' @export 
#'

get_endpoint_data = function(url, endpoint=c("ping", "metadata", "prototype")) {
  endpoint = match.arg(endpoint)
  res = httr::GET(paste0(url, "/", endpoint))
  meta = httr::content(res, as = "parsed", type = "application/json")
  if (endpoint=="metadata") {
    template = meta$user$template
    template = as.data.frame(lapply(template, function(x) unlist(x))) # as.data.frame(do.call("cbind", template))
    prototype = meta$user$prototype
    recipes = meta$user$recipes
    meta = list(template=template, prototype=prototype, recipes=recipes)
  }
  return(meta)
}

#' Preprocess new data
#'
#' @details Preprocess new (incoming) data for prediction.
#'
#' @param recipes preprocessed recipes object
#' @param new_data new data
#'
#' @export 

preprocess_new_data = function(recipes, new_data) {
  df = (recipes
    |> recipes::bake(new_data = new_data)
  )
  return(df)
}

#' Save trained model
#'
#' @details Save a trained model into local storage
#'
#' @param model trained model object
#' @param name character specifying the model
#' @param folder local folder to store the models
#'
#' @export 
#'

save_model = function(model, name, folder="models", metadata=list()) {
  Rautoml::create_dir(folder)
  board = pins::board_folder(folder)
  v = vetiver::vetiver_model(
    model = model
    , model_name = name # digest::digest(name, algo = "sha256")
	 , metadata=metadata
  )
  vetiver::vetiver_pin_write(board, v)
}

#' Save recipes
#'
#' @export
#'

save_recipes = function(recipes, name, folder="recipes") {
  Rautoml::create_dir(folder)
  board = pins::board_folder(folder)
  pins::pin_write(board=board, x=recipes, name=name, type="rds")
}

#' Read saved recipes 
#'
#' @export
#'

get_recipes = function(name, folder="recipes") {
  board = pins::board_folder(folder)
  x = pins::pin_read(board, name=name)
  return(x)
}

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


extract_summary.list = function(models, summary_fun = quantile_summary) {
  if (length(models)==1) {
    res = models[[1]]$resample
    res$model = models[[1]]$method
    res = (res
      |> tidyr::gather(-Resample, -model, key="metric", value = "score")
    )
  } else {
    res = (caret::resamples(models)$values
      |> tidyr::gather(-Resample, key="metric", value = "score")
      |> tidyr::separate(col=metric, into=c("model", "metric"), sep="~")
    )
  }
  res = (res
    |> group_by(model, metric)
    |> summarise(summary_fun(score), .groups = "drop")
    |> ungroup()
  )
  class(res) = c("Rautomlmetric", class(res))
  return(res)
}

#' Collect model summary
#'
#' @details This function extracts and summarizes model estimates from careList object.
#'
#' @param models Models from caretEnsemble
#' @param summary_fun a function to summarize metrics
#'
#'
#' @export
#'


extract_summary.caretEnsemble = function(models, summary_fun = quantile_summary) {

  score1 = extract_summary(models$models)
  score2 = models$ens_model$resample
  score2$model = "Ensemble"
  score2 = (score2
    |> tidyr::gather(-Resample, -model, key="metric", value = "score")
    |> dplyr::group_by(model, metric)
    |> dplyr::summarise(summary_fun(score), .groups = "drop")
    |> dplyr::ungroup()
  )
  res = do.call("rbind", list(score1, score2))
  class(res) = c(class(res), "Rautomlmetric")
  return(res)
}




#' Quantile based summary function
#'
#' @param x vector of numbers
#'
#'
#' @export

quantile_summary = function(x, probs = c(0.025, 0.5, 0.975), names = c("lower", "estimate", "upper")) {
  x = quantile(x, probs = probs, na.rm=TRUE, digits=4)
  x = as.data.frame(t(x))
  colnames(x) = names
  return(x)
}


#' Student-t based summary function
#'
#' @param x vector of numbers
#'
#' @export
#'

student_t_summary = function(x, conf.level = 0.95, names = c("lower", "estimate", "upper")) {
  n = length(x)
  mean_x = mean(x, na.rm = TRUE)
  se = sd(x, na.rm = TRUE) / sqrt(n)
  error_margin = qt(1 - (1 - conf.level) / 2, df = n - 1) * se

  lower = mean_x - error_margin
  upper = mean_x + error_margin
  d = data.frame(lower, mean_x, upper)
  colnames(d) = names
  return(d)
}




#' Estimate predictive scores for a single model
#'
#' @param df test data.frame
#'
#' @export


boot_measures = function(model, df, outcome_var, problem_type, type="prob") {
  x_df = df[, setdiff(colnames(df), outcome_var), drop=FALSE]
  y = df[[outcome_var]]

  if (problem_type == "Classification") {
    if (inherits(model, "caretEnsemble")) {
      preds = predict(model, newdata=x_df)
    } else {
      preds = predict(model, newdata=x_df, type=type)
    }

	 if (Rautoml::get_type(y)!="factor") {
	 	y = as.factor(y)
	 }

    # Predicted class
    .nn = ncol(preds)
    preds$pred = factor(apply(preds[,1:.nn], 1, function(x) colnames(preds)[which.max(x)]),
                         levels = levels(y))
    preds$obs = y

	 mt = caret::multiClassSummary(preds, lev = levels(preds$obs))
    scores_df = as.data.frame(as.list(mt))
    colnames(scores_df) = gsub(".*Mean_", "", colnames(scores_df))
	 all = c("Accuracy", "AUC", "prAUC", "Kappa", "Sensitivity", "Specificity", "Precision", "Recall", "F1", "Pos_Pred_Value", "Neg_Pred_Value", "Detection_Rate", "Balanced_Accuracy", "logLoss")
	 scores_df[, all] = scores_df[, all]

    # --- Handle Binary vs Multiclass ---
    if (nlevels(y) == 2) {
      base_lev = levels(y)[2]  

      # ROC
      rocr_pred = ROCR::prediction(preds[[base_lev]], preds$obs)
      model_roc = ROCR::performance(rocr_pred, "tpr", "fpr")
      roc_df = data.frame(x = model_roc@x.values[[1]], y = model_roc@y.values[[1]])

    } else {
      base_lev = NULL
      
		# Multiclass ROC (one-vs-rest)
      y_mat = model.matrix(~y-1)
      colnames(y_mat) = levels(y)
      aucs = c()
      roc_df_list = list()
      for (lev in levels(y)) {
        rocr_pred = ROCR::prediction(preds[[lev]], y_mat[,lev])
        auc_perf = ROCR::performance(rocr_pred, "auc")
        aucs[lev] = auc_perf@y.values[[1]]

        model_roc = ROCR::performance(rocr_pred, "tpr", "fpr")
        roc_df_list[[lev]] = data.frame(
          x = model_roc@x.values[[1]],
          y = model_roc@y.values[[1]],
          Class = lev
        )
      }
      roc_df = do.call(rbind, roc_df_list)
    }

  } else if (problem_type == "Regression") {
    preds = predict(model, x_df)
    scores_df = data.frame(as.list(caret::postResample(pred = preds, obs = y)))
    roc_df = data.frame(x=y, y=preds, .check_="pred_roc")
    base_lev = NULL
  }

  return(list(scores_df=scores_df, roc_df=roc_df, positive_cat=base_lev))
}


#' Bootstrap estimate for the predictive measures
#'
#' @export


boot_estimates = function(model, df, outcome_var, problem_type, nreps = 100, type="prob", model_name=NULL, report = c("Accuracy", "AUC", "prAUC", "Kappa", "Sensitivity", "Specificity", "Precision", "Recall", "F1", "Pos_Pred_Value", "Neg_Pred_Value", "Detection_Rate", "Balanced_Accuracy", "logLoss", "RMSE", "Rsquared", "MAE"), summary_fun=quantile_summary, save_model=FALSE, model_id=paste0(model_name, "-", Sys.time()), model_folder="models", preprocesses=NULL) {
	if (problem_type=="Classification") {
		all = c("Accuracy", "AUC", "prAUC", "Kappa", "Sensitivity", "Specificity", "Precision", "Recall", "F1", "Pos_Pred_Value", "Neg_Pred_Value", "Detection_Rate", "Balanced_Accuracy", "logLoss")
	} else if (problem_type=="Regression") {
		all = c("RMSE", "Rsquared", "MAE") 
	}

  if (is.null(model_name)) {
    model_name = model$method
	 model_id = paste0(model_name, "-", Sys.time())
  }
  
	if (!any(all %in% report)) {
		stop(c("The report options are ", paste0(all, collapse=", ")))
	}
	resamples = createResample(1:nrow(df), times = nreps, list = TRUE)
	est = lapply(resamples, function(x){
		boot_measures(model=model, df=df[x, ], outcome_var=outcome_var, problem_type=problem_type, type = type)$scores_df
	})
	out = do.call(rbind, est)
	out = sapply(out, summary_fun, simplify=FALSE)
	out = do.call("rbind", out)
	out = as.data.frame(out)
	out$metric = rownames(out)
	out$model = model_name
	model_id = digest::digest(model_id, algo = "xxhash32")
	out$model_id = model_id
	mm_name = c("model_id", "model", "metric")
	if (save_model) {
		dd = preprocesses$original_df
		metadata = list(
			template = dd |> head() |> dplyr::select(-dplyr::all_of(outcome_var))
			, prototype = create_pred_prototype(df=dd, outcome_var=outcome_var)
			, recipes = model_id
		)
		save_recipes(preprocesses$recipes, name=model_id, folder="recipes")
		save_model(model=model, name=model_id, folder=model_folder, metadata=metadata)
	}
	out_metric = out[out$metric==report,]
	out_metric = out_metric[, union(mm_name, colnames(out_metric))]
	out = out[, union(mm_name, colnames(out))]
	out = list(out_metric, out)
	names(out) = c("specifics", "all")
	## Generate ROC
	roc = boot_measures(model, df, outcome_var, problem_type)
	roc_df = roc$roc_df
	if (NROW(roc_df)) {
	  roc_df$model_id = model_id
	  roc_df$model = model_name
	  roc_df = roc_df[, union(c("model_id", "model"), colnames(roc_df))]
	}
	out$roc_df = roc_df
	positive_cat = roc$positive_cat
	out$positive_cat = positive_cat
	return(out)
}


#' Compute predicitve metrics for multiple caret models in a list
#'
#' @export 


boot_estimates_multiple.caretList = function(models, df, outcome_var, problem_type, nreps = 100, model_name=NULL, type="prob", report = c("Accuracy", "AUCROC", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F", "RMSE", "Rsquared", "MAE"), summary_fun=quantile_summary, save_model=FALSE, model_id=paste0(model_name, "-", Sys.time()), model_folder="models", preprocesses=NULL) {
  est = lapply(models, function(x){
    est = boot_estimates(
      model=x
      , df=df
      , outcome_var=outcome_var
      , problem_type=problem_type
      , nreps=nreps
      , model_name=model_name
    	, type=type
    	, report=report
	 	, summary_fun=summary_fun
		, save_model=save_model
		, model_id=model_id
		, model_folder=model_folder
		, preprocesses=preprocesses
    )
    return(est)
  })
  positive_cat = est[[1]]$positive_cat
  est = sapply(c("specifics", "all", "roc_df"), function(x){
    est = lapply(est, `[[`, x)
    est = do.call("rbind", est)
    return(est)
  }, simplify = FALSE)
  est$positive_cat = positive_cat
  class(est) = c("Rautomlmetric2", class(est))
  return(est)
}

#' Compute predicitve metrics for multiple caretEnsemble models
#'
#' @export 


boot_estimates_multiple.caretEnsemble = function(models, df, outcome_var, problem_type, nreps = 100, model_name=NULL, type="prob", report = c("Accuracy", "AUCROC", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F", "RMSE", "Rsquared", "MAE"), summary_fun=quantile_summary, save_model=FALSE, model_id=paste0(model_name, "-", Sys.time()), model_folder="models", preprocesses=NULL) {
  ens_model = models
  base_models = as.list(models$models)
  base_models$ensemble = ens_model
  base_models$ensemble$method = "ensemble"
  est = boot_estimates_multiple(base_models, df, outcome_var, problem_type, nreps, model_name, type, report, summary_fun, save_model, model_id, model_folder, preprocesses)
  class(est) = c("Rautomlmetric2", class(est))
  return(est)
}

#' Generate confussion matrix and variable importance
#'
#' @export 
#'

get_post_metrics = function(model, outcome, df=NULL, task=NULL) {
  preds = predict(model, newdata=df)
  
  if (isTRUE(task=="Classification")) {
	  if (inherits(model, "caretEnsemble")) {
		 preds = as.factor(colnames(preds)[max.col(preds, ties.method = "first")])
	  }
	  
	  ## Confussion matrix
	  cm = caret::confusionMatrix(preds, df[[outcome]])
	  cm = as.data.frame(cm$table)
	  colnames(cm) = c("Prediction", "Target", "N")
	  cm_plot = cvms::plot_confusion_matrix(cm)
  } else if (isTRUE(task=="Regression")) {
		preds = as.numeric(unlist(preds))
		actuals = df[[outcome]]
		cm_plot = (ggplot2::ggplot(data.frame(actual=df[[outcome]], pred=preds), aes(x=actual, y=pred))
			+ ggplot2::geom_point(alpha=0.6) 
      	+ geom_smooth(method = lm, color="green", se = TRUE)
			+ ggplot2::geom_abline(slope=1, intercept=0, color="red") 
			+ ggplot2::labs(title="Predicted vs Actual", x="Actual", y="Predicted")
			+ theme_minimal(base_size = 12)
		)
  }
  
  if (!inherits(model, "caretEnsemble")) {
    ## Overall variable importance
    var_imp = try(caret::varImp(model), silent = TRUE)
    if (inherits(var_imp, "try-error")) {
      var_imp_plot = NULL
    } else {
      var_imp_plot = plot(var_imp)
    }
  } else {
    var_imp_plot = NULL
  }
  return(list(cm_plot=cm_plot, var_imp_plot=var_imp_plot))
}


#' Generate confussion matrix and variable importance for caretlist
#'
#' @export 
#'


post_model_metrics.caretList = function(models, outcome, df=NULL, task=NULL) {
  mets = lapply(models, function(model){
    out = get_post_metrics(model=model, outcome=outcome, df=df, task=task)
    out = list(cm_plot=out$cm_plot, var_imp_plot=out$var_imp_plot)
    return(out)
  })
}


#' Generate confussion matrix and variable importance for caretEnsemble
#'
#' @export 
#'

post_model_metrics.caretEnsemble = function(models, outcome, df=NULL, task=NULL) {
  ensemble = get_post_metrics(model=models, outcome=outcome, df=df, task=task)
  models = models$models
  mets = lapply(models, function(model){
    out = get_post_metrics(model=model, outcome=outcome, df=df, task=task)
    out = list(cm_plot=out$cm_plot, var_imp_plot=out$var_imp_plot)
    return(out)
  })
  mets = c(mets, list(ensemble=ensemble))
  return(mets)
}

#' Get trained model metrics
#'
#' @param object metric object of class Rautomlmetric2
#'
#' @export 
#'

get_metrics_names.Rautomlmetric2 = function(object) {
	all_metrics = sort(unique(object$all$metric))
	return(all_metrics)
}

#' Extract sprcific metric for a particular model
#'
#' @param object metic object of class Rautomlmetric2
#'
#' @export 
#'

extract_more_metrics.Rautomlmetric2 = function(object, model_name, metric_name) {
	all_ = (object$all
		|> dplyr::filter(model %in% model_name)
		|> dplyr::filter(metric %in% metric_name)
	)	
	if (!is.null(object$roc_df)) {
		roc_df_ = (object$roc_df
			|> dplyr::filter(model %in% model_name)
		)
	} else {
		roc_df_ = NULL
	}

	object$specifics = NULL
	object$all = all_
	object$roc_df = roc_df_
	return(object)
}

#' Filter models to show 
#'
#' @param object from `get_post_metrics`
#'
#' @export
#'

post_metrics_model_filter = function(object, model_name) {
	object = object[names(object) %in% model_name]
	return(object)
}









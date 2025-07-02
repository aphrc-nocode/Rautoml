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


extract_summary.caretList = function(models, summary_fun = quantile_summary) {
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
    |> group_by(model, metric)
    |> summarise(summary_fun(score), .groups = "drop")
    |> ungroup()
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

#' Estimate predictive scores for a single model
#'
#' @param df test data.frame
#'
#' @export

boot_measures = function(model, df, outcome_var, problem_type, type="prob"){
	x_df <- df[, colnames(df)[!colnames(df) %in% outcome_var]]
	y <- df[, outcome_var, drop=TRUE]

	if (problem_type=="classification") {
	  if (inherits(model, "caretEnsemble")) {
	    preds <- predict(model, newdata=x_df)
	  } else {
	    preds <- predict(model, newdata=x_df, type = type)
	  }
		preds$pred <- factor(apply(preds, 1, function(x)colnames(preds)[which.max(x)]), levels=levels(y))
		preds$obs <- y
		ss <- twoClassSummary(preds, lev = levels(preds$obs))
		pp <- prSummary(preds, lev = levels(preds$obs))
		aa <- confusionMatrix(preds$pred, preds$obs)$overall[["Accuracy"]]
		scores_df <- data.frame(Accuracy = aa
			, AUCROC = ss[["ROC"]]
			, AUCRecall = pp[["AUC"]]
			, Sens = ss[["Sens"]]
			, Spec = ss[["Spec"]]
			, Precision = pp[["Precision"]]
			, Recall = pp[["Recall"]]
			, "F" = pp[["F"]]
		)

		## ROCs
#		base_lev <- levels(preds$pred)[1]
		base_lev <-  model$levels[[2]]
		rocr_pred <- prediction(preds[[base_lev]]
			, preds$obs
		)
		model_roc <- performance(rocr_pred, "tpr", "fpr")
		roc_df <- data.frame(x = model_roc@x.values[[1]], y = model_roc@y.values[[1]])
	} else if (problem_type=="regression") {
		preds <- predict(model, x_df)
		scores_df = data.frame(as.list(postResample(pred = preds, obs = y)))
		roc_df = NULL
		base_lev = NULL
	}
	return(list(scores_df=scores_df, roc_df=roc_df, positive_cat = base_lev))
}

#' Bootstrap estimate for the predictive measures
#'
#' @export

boot_estimates = function(model, df, outcome_var, problem_type, nreps = 100, type="prob", model_name=NULL, report = c("Accuracy", "AUCROC", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F", "RMSE", "Rsquared", "MAE"), summary_fun=quantile_summary) {
	if (problem_type=="classification") {
		all <- c("Accuracy", "AUCROC", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F") 
	} else if (problem_type=="regression") {
		all <- c("RMSE", "Rsquared", "MAE") 
	}

  if (is.null(model_name)) {
    model_name = model$method
  }
  
	if (!any(all %in% report)) {
		stop(c("The report options are ", paste0(all, collapse=", ")))
	}
	resamples <- createResample(1:nrow(df), times = nreps, list = TRUE)
	est <- lapply(resamples, function(x){
		boot_measures(model=model, df=df[x, ], outcome_var=outcome_var, problem_type=problem_type, type = type)$scores_df
	})
	out <- do.call(rbind, est)
	out <- sapply(out, summary_fun, simplify=FALSE)
	out <- do.call("rbind", out)
	out <- as.data.frame(out)
	out$metric <- rownames(out)
	out$model <- model_name
	out_metric <- out[out$metric==report,]
	out <- list(out_metric, out)
	names(out) <- c("specifics", "all")
	## Generate ROC
	roc <- boot_measures(model, df, outcome_var, problem_type)
	roc_df <- roc$roc_df
	if (NROW(roc_df)) {
	  roc_df$model = model_name
	}
	out$roc_df <- roc_df
	positive_cat <- roc$positive_cat
	out$positive_cat <- positive_cat
	return(out)
}


#' Compute predicitve metrics for multiple caret models in a list
#'
#' @export 


boot_estimates_multiple.caretList = function(models, df, outcome_var, problem_type, nreps = 100, model_name=NULL, type="prob", report = c("Accuracy", "AUCROC", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F", "RMSE", "Rsquared", "MAE"), summary_fun=quantile_summary) {
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


boot_estimates_multiple.caretEnsemble = function(models, df, outcome_var, problem_type, nreps = 100, model_name=NULL, type="prob", report = c("Accuracy", "AUCROC", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F", "RMSE", "Rsquared", "MAE"), summary_fun=quantile_summary) {
  ens_model = models
  base_models = as.list(models$models)
  base_models$ensemble = ens_model
  base_models$ensemble$method = "ensemble"
  est = boot_estimates_multiple(base_models, df, outcome_var, problem_type, nreps, model_name, type, report, summary_fun)
  class(est) = c("Rautomlmetric2", class(est))
  return(est)
}

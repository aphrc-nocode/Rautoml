#' Create directory for various artifacts
#'
#' @details Performs a number of ML preprocessing
#'
#' @param df a data frame.
#' @param model_form model formula. 
#' @param corr a value for the threshold of absolute correlation values. The step will try to remove the minimum number of columns so that all the resulting absolute correlations are less than this value.
#' @param impute_methods a character specifying how to handle missing values. 
#'
#' @return NULL
#'
#' @importFrom recipes recipe
#'
#' @export

preprocess = function(df, model_form, outcome_var, corr=0, impute=TRUE, impute_methods = c("omit", "missing_mean", "missing_median", "mode_median", "mode_mean", "knn", "knn_linear", "bag"), perform_fe=TRUE, perform_pca=FALSE, up_sample=FALSE, up_sample_type=c("random", "smote", "bsmote", "adasyn", "rose"), df_test=NULL, task=NULL, exclude=NULL) {
	df_out = recipes::recipe(formula=model_form, data=df)
	preprocess_result = list()
	df_out = (df_out
		|> recipes::step_rm(where(~all(is.na(.x))))
	)
	if (anyNA.data.frame(df) & isTRUE(impute)) {
		impute_methods = match.arg(impute_methods)
		if (impute_methods == "omit") {
			df_out = (df_out
				%>% recipes::step_naomit(recipes::all_predictors())
			)
			preprocess_result$impute_methods = "Deleted missing values"
		} else if (impute_methods== "missing_mean") {
			df_out = (df_out
				%>% recipes::step_novel(recipes::all_nominal(), skip=TRUE)
				%>% recipes::step_unknown(recipes::all_nominal(), new_level = "Missing_category", skip=TRUE)
				%>% recipes::step_impute_mean(recipes::all_numeric(), skip=TRUE)
			)
			preprocess_result$impute_methods = c("missing values in categorical variables recorded as Missing_category", "missing values in numeric variables replaced by the mean.")
		} else if (impute_methods=="missing_median") {
			df_out = (df_out
				%>% recipes::step_novel(recipes::all_nominal(), skip=TRUE)
				%>% recipes::step_unknown(recipes::all_nominal(), new_level = "Missing_category", skip=TRUE)
				%>% recipes::step_impute_median(recipes::all_numeric(), skip=TRUE)
			)
			preprocess_result$impute_methods = c("missing values in categorical variables recorded as Missing_category", "missing values in numeric variables replaced by the median.")
		} else if (impute_methods=="mode_median") {
			df_out = (df_out
				%>% recipes::step_novel(recipes::all_nominal(), skip=TRUE)
				%>% recipes::step_impute_mode(recipes::all_nominal(), skip=TRUE)
				%>% recipes::step_impute_median(recipes::all_numeric(), skip=TRUE)
			)
			preprocess_result$impute_methods = c("missing values in categorical variables replacesd by modal value", "missing values in numeric variables replaced by the median.")
		} else if (impute_methods=="mode_mean") {
			df_out = (df_out
				%>% recipes::step_novel(recipes::all_nominal(), skip=TRUE)
				%>% recipes::step_impute_mode(recipes::all_nominal(), skip=TRUE)
				%>% recipes::step_impute_mean(recipes::all_numeric(), skip=TRUE)
			)
			preprocess_result$impute_methods = c("missing values in categorical variables replacesd by modal value", "missing values in numeric variables replaced by the mean.")
		} else if (impute_methods=="bag") {
			df_out = (df_out
				%>% recipes::step_impute_bag(recipes::all_predictors(), recipes::all_outcomes(), skip=TRUE)
			)
			preprocess_result$impute_methods = c("missing values imputed using Bagging")
		} else if (impute_methods=="knn") {
			df_out = (df_out
				%>% recipes::step_impute_knn(recipes::all_predictors(), recipes::all_outcomes(), skip=TRUE)
			)
			preprocess_result$impute_methods = c("missing values imputed using KNN.")
		} else if (impute_methods=="knn_linear") {
			df_out = (df_out
				%>% recipes::step_impute_knn(recipes::all_nominal(), skip=TRUE)
				%>% recipes::step_impute_linear(recipes::all_numeric(), skip=TRUE)
			)
			preprocess_result$impute_methods = c("missing values in categorical variables replacesd by KNN", "missing values in numeric variables replaced by linear reg/ression.")
		}
	}

	df_out = (df_out
		|> step_mutate(across(where(is.factor), forcats::fct_drop), skip=TRUE)
	)

	if (perform_fe) {
		df_out = (df_out
			%>% recipes::step_center(recipes::all_numeric_predictors())
			%>% recipes::step_scale(recipes::all_numeric_predictors())
		)
		if (is.null(exclude)) {
			df_out = (df_out
				%>% recipes::step_nzv(recipes::all_predictors())
			)
			df_out = (df_out
				%>% recipes::step_dummy(recipes::all_nominal_predictors())
			)
			df_out = (df_out
				%>% recipes::step_nzv(recipes::all_predictors())
			)
		} else {
			df_out = (df_out
				%>% recipes::step_nzv(recipes::all_predictors(), -recipes::all_of(exclude))
			)
			df_out = (df_out
				%>% recipes::step_dummy(recipes::all_nominal_predictors(), -recipes::all_of(exclude))
			)
			df_out = (df_out
				%>% recipes::step_nzv(recipes::all_predictors(), -recipes::all_of(exclude))
			)
		}
	}
	
	if (corr > 0) {
		if (is.null(exclude)) {
			df_out = (df_out
				%>% recipes::step_corr(recipes::all_numeric_predictors(), threshold=corr)
			)
		} else {
			df_out = (df_out
				%>% recipes::step_corr(recipes::all_numeric_predictors(), -recipes::all_of(exclude), threshold=corr)
			)
		}
	}
	
	if (perform_pca) {
		if (is.null(exclude)) {
			df_out = (df_out
				|> recipes::step_pca(recipes::all_numeric_predictors(), num_comp=10, keep_original_cols=TRUE)
			)
		} else {
			df_out = (df_out
				|> recipes::step_pca(recipes::all_numeric_predictors(), -recipes::all_of(exclude), num_comp=10, keep_original_cols=TRUE)
			)
		}
	}
	

	if (isTRUE(!is.null(outcome_var)) & isTRUE(outcome_var!="")) {
		if (isTRUE(!is.null(task)) & isTRUE(task=="Classification")) {
			df_out = (df_out
				|> recipes::step_mutate_at(outcome_var, fn=function(x){
					x = forcats::fct_relabel(x, make.names)
					return(x)
				}, skip=TRUE)
			)
		}
		
		if (isTRUE(up_sample)) {
			up_sample_type = match.arg(up_sample_type)
			df_out = switch(
			  up_sample_type,
			  random = themis::step_upsample(df_out, dplyr::one_of(outcome_var), over_ratio = 0.5, skip=TRUE),
			  smote  = themis::step_smote(df_out,   dplyr::one_of(outcome_var), over_ratio = 0.5, skip=TRUE),
			  bsmote = themis::step_bsmote(df_out,  dplyr::one_of(outcome_var), over_ratio = 0.5, skip=TRUE),
			  adasyn = themis::step_adasyn(df_out,  dplyr::one_of(outcome_var), over_ratio = 0.5, skip=TRUE),
			  rose   = themis::step_rose(df_out,    dplyr::one_of(outcome_var), over_ratio = 0.5, skip=TRUE)
			)
		}
	}

	df_out = (df_out
		%>% recipes::prep(training=df, retain=TRUE)
	)
	prepped_recipe = df_out
	df_out = (df_out
		%>% recipes::bake(new_data=NULL)
	)

	## FIXME: Updating skipped processes. If using new data with outcome, this has to updated
	if (!is.null(df_test)) {
		df_test = preprocess_new_data(recipes=prepped_recipe, new_data=df_test, update_skip=TRUE)
		df_test = na.omit(df_test)
	}

	outcome_nlevels = NULL
	if (isTRUE(!is.null(outcome_var)) & isTRUE(outcome_var!="")) {
		if (isTRUE(!is.null(task)) & isTRUE(task=="Classification")) {
			outcome_nlevels = get_nlevels(df_out, outcome_var)
		}
	}

	if (!is.null(outcome_var) & outcome_var!="") {
		preprocess_result$predictors_for_analysis = paste0(outcome_var, " was used as an outcome variable, while the following were used as predictors: ", paste0(colnames(df_out)[!colnames(df_out) %in% outcome_var], collapse=", "), ".")
	} else {
		preprocess_result$predictors_for_analysis = paste0( "Unsupervised learning, all variables were used as predictors: ", paste0(colnames(df_out)[!colnames(df_out) %in% outcome_var], collapse=", "), ".")
	}

	removed_vars = colnames(df)[!colnames(df) %in% colnames(df_out)]
	if (length(removed_vars)>0) {
      preprocess_result$removed_vars = paste0("• ", length(removed_vars), " variabel(s) were removed after preprocessing: ", paste0(removed_vars, collapse=", "))
	}

	preprocess_result = c(extract_recipe_text(prepped_recipe), paste0("• ", preprocess_result$predictors_for_analysis), preprocess_result$removed_vars)
	return(list(train_dfdf=df_out, test_df=df_test, original_df=df, recipes=prepped_recipe, preprocess_steps=preprocess_result, outcome_nlevels=outcome_nlevels))
}


#' Extract text from recipe oabject
#'
#' @param recipe recipe object
#'
#' @return a vector
#'
#' @importFrom cli cli_fmt ansi_strip 
#'
#' @export
#'

extract_recipe_text = function(recipes) {
  raw_text = cli::cli_fmt(print(recipes))
  clean_text = cli::ansi_strip(raw_text)
  return(clean_text)
}


#' Simple train/test split
#'
#' @param data
#'
#' @param ... additional args
#'
#' @rdname train_test_split
#'
#' @return list
#'
#' @export
#'
#' @importFrom rsample initial_split initial_time_split group_initial_split training testing

train_test_split = function(data, type=c("single", "time", "group", "tseries"), prop = 3/4, group=NULL, strata = NULL, breaks = 4, pool = 0.1, ..., lag=0) {
	type = match.arg(type)
	switch(type
		, "single" = {
			dd = rsample::initial_split(data=data, prop=prop, strata=strata, breaks=breaks, pool=pool, ...)
		}
		, "time" = {
			dd = rsample::initial_time_split(data=data, prop=prop, lag=lag, ...)
		}
		, "group" = {
			dd = rsample::group_initial_split(data=data, group=group, prop=prop, ...=..., strata=strata, pool=pool)
		}
		, "tseries" = {
			dd = NULL # FIXME:: TODO
		}
	)
	train_df = rsample::training(dd)
	test_df = rsample::testing(dd)
	out = list(split=dd, train_df=train_df, test_df=test_df)
	return(out)
}



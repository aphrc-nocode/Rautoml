#' Compute SHAP values for a caretList or caretEnsemble model 
#'
#' @details Computes SHAP values for a single caret model. In case of cartList or caretEnsemble, call the specific model from the list.
#'
#' @importFrom shapviz sv_importance sv_waterfall sv_waterfall shapviz

## @importFrom fastshap explain 

compute_shap_single = function(model, newdata, response, task, positive_class=2, model_name="model", nsim = 50, max_n=1000) {
  
  get_all_sv = function(sv) {
    sv = list(
      sv_bw = shapviz::sv_importance(sv, kind = "beeswarm") + ggplot2::theme_minimal()
      , sv_wf = shapviz::sv_waterfall(sv)
      , sv_f = shapviz::sv_force(sv)
    )
    return(sv)
  }
  
  clean_sv = function(x, shap_data) {
    sv_viz = shapviz::shapviz(x, X=shap_data)
    sv = as.data.frame(x)
    sv = (sv
      |> tidyr::pivot_longer(
        cols = dplyr::everything()
        , names_to = "variable"
        , values_to = "score"
      )
    )
    sv_all = get_all_sv(sv_viz)
    
    ## variable dependence
    sv_dp_df = (sv
      |> dplyr::mutate(id=1:dplyr::n())
    )
    
    ## Variable importance
    sv = (sv
      |> group_by(variable)
      |> summarise(Rautoml::student_t_summary(abs(score)), .groups = "drop")
    )
    
    ## SHAP values long
    sv_long = (shap_data
      |> tidyr::pivot_longer(
        cols = dplyr::everything()
        , names_to = "variable"
        , values_to = "observed"
      )
      |> dplyr::mutate(id=1:dplyr::n())
    )
    
    ## Dependency plots
    sv_dp = dplyr::left_join(sv_dp_df, sv_long)
    
    out = list(sv=sv, sv_all=sv_all, sv_dp=sv_dp)
    return(out)
  }
  
  predictors_names = colnames(newdata)[!colnames(newdata) %in% response]
  shap_data = newdata[sample(1:nrow(newdata), size = min(max_n, nrow(newdata))), predictors_names, drop=FALSE]

  pred_wrapper = function(object, newdata) {
    return(predict(object, newdata = newdata))
  }
  
  if (isTRUE(task=="Regression")) {
  	 if (inherits(model, "caretEnsemble")) {
		 pred_wrapper = function(object, newdata) {
			return(predict(object, newdata = newdata)$pred)
		 }
	 }
    x = fastshap::explain(
      model,
      X = shap_data,
      pred_wrapper = pred_wrapper,
      nsim = nsim
    )
    x = clean_sv(x, shap_data = shap_data)
    sv = x$sv
    sv$Class = "class"
    sv_all = x$sv_all
    sv_dp = x$sv_dp
    sv_dp$Class = "class"
  } else if (isTRUE(task=="Classification")) {
    if (inherits(model, "caretEnsemble")) {
      pred_wrapper = function(object, newdata) {
        p = predict(object, newdata = newdata)
        p = as.data.frame(p)
        p = p[, cls]
        return(p)
      }
      # levs = model$ens_model$levels
    } else {
      pred_wrapper = function(object, newdata) {
        p = predict(object, newdata = newdata, type = "prob")[, cls]
        return(p)
      }
      # levs = model$levels
    }
    levs = model$levels
    if (length(levs) > 2) {
      sv = list()
      sv_all = list()
      sv_test_df = list()
      for (cls in levs) {
        x = fastshap::explain(
          model,
          X = shap_data,
          pred_wrapper = pred_wrapper,
          nsim = nsim
        )
        sv_viz = shapviz::shapviz(x, X=shap_data)
        sv_all[[cls]] = get_all_sv(sv_viz)
        x = as.data.frame(x)
        x["outcomexx__..xx"] = cls
        sv[[cls]] = x
        sv_test = shap_data
        sv_test["outcomexx__..xx"] = cls
        sv_test_df[[cls]] = sv_test
      }
      sv = do.call("rbind", sv)
      sv = (sv
        |> tidyr::pivot_longer(
          cols = -outcomexx__..xx
          , names_to = "variable"
          , values_to = "score"
        )
        |> dplyr::rename("Class"="outcomexx__..xx")
      )
      
      ## variable dependence
      sv_dp_df = (sv
        |> dplyr::mutate(id=1:dplyr::n())
      )
      sv = (sv
        |> group_by(Class, variable)
        |> summarise(Rautoml::student_t_summary(abs(score)), .groups = "drop")
      )
      
      ## SHAP values long
      sv_long = (do.call("rbind", sv_test_df)
        |> tidyr::pivot_longer(
          cols = -outcomexx__..xx #dplyr::everything()
          , names_to = "variable"
          , values_to = "observed"
        )
        |> dplyr::rename("Class"="outcomexx__..xx")
        |> dplyr::mutate(id=1:dplyr::n())
      )
      
      ## Dependency plots
      sv_dp = dplyr::left_join(sv_dp_df, sv_long)
      
    } else {
      if (inherits(model, "caretEnsemble")) {
        pred_wrapper = function(object, newdata) {
          p = predict(object, newdata = newdata)
          p = as.data.frame(p)
          return(p[, positive_class])
        }
      } else {
        pred_wrapper = function(object, newdata) {
          return(predict(object, newdata = newdata, type = "prob")[, positive_class])
        }
      }
      x = fastshap::explain(
        model,
        X = shap_data,
        pred_wrapper = pred_wrapper,
        nsim = nsim
      )
      x = clean_sv(x, shap_data = shap_data)
      sv = x$sv
      sv$Class = "class"
      sv_all = x$sv_all
      sv_dp = x$sv_dp
      sv_dp$Class = "class"
    }
  }
  sv$model = model_name
  sv_dp$model = model_name
  return(list(sv=sv, sv_viz=sv_all, sv_dp=sv_dp))
}

#' Extract SHAP measures from compute_shap_single
#'
#' @param x compute_shap_single object
#' @param top_n_rank the top ranking variables, the rest are categorized as others.
#'


extract_shap = function(x, top_n_rank, total_n_rank) {
  out = do.call("rbind", x)
  varimp_df = do.call("rbind", out[, "sv"])
  vardep_df = do.call("rbind", out[, "sv_dp"])
  sv_viz = out[, "sv_viz"]
  nn = names(x)
  if (length(nn)==1) {
  	names(sv_viz) = nn
  }
  
  ## Most frequent variables
  varfreq_df = (varimp_df
   |> dplyr::group_by(Class, model)
  	|> dplyr::arrange(desc(estimate), .by_group=TRUE)
  	|> dplyr::mutate(pos = 1:dplyr::n())
  	|> dplyr::ungroup()
  	|> dplyr::mutate(NULL
  		, pos=ifelse(pos<=top_n_rank, pos, top_n_rank+1)
  		, new_terms=forcats::fct_reorder(variable, pos, mean)
  	)
  	|> dplyr::filter(as.numeric(new_terms) <= total_n_rank)
  	|> dplyr::group_by(Class, new_terms, pos)
  	|> dplyr::count()
  	|> droplevels()
  )
  out = list(varimp_df=varimp_df, varfreq_df=varfreq_df, vardep_df=vardep_df, sv_viz=sv_viz)
  return(out)
}


#' Compute SHAP values for several caretList models
#'
#' @param model_names names of the models as in the `names(models)`.
#' @param models caretList models
#' @param newdata training data
#'
#' @export

compute_shap.caretList = function(models, model_names, newdata, response, task, nsim = 50, max_n=1000, top_n_rank=5, total_n_rank=20) {
  out = sapply(model_names, function(model_name){
    model =  models[[model_name]]
    if (task=="Classification") {
      positive_class = model$levels[[2]]
    } else {
      positive_class = NULL
    }
    out = compute_shap_single(
      model = model
      , newdata=newdata
      , response = response
      , task = task
      , positive_class = positive_class
      , model_name = model_name
      , nsim = nsim
      , max_n = max_n
    )
    return(out)
  }, simplify = FALSE)
  out = extract_shap(out, top_n_rank, total_n_rank)
  class(out) = c("Rautomlshap", class(out))
  return(out)
}


#' Compute SHAP values for several caretEnsemble models
#'
#' @param model_names names of the models as in the `names(models)`.
#' @param models caretList models
#' @param newdata training data
#'
#' @export

compute_shap.caretEnsemble = function(models, model_names=NULL, newdata, response, task, nsim = 50, max_n=1000, top_n_rank=5, total_n_rank=20) {
  ens_model = models
  base_models = as.list(models$models)
  base_models$ensemble = ens_model
  base_models$ensemble$method = "ensemble"
  if (is.null(model_names)) {
    model_names = names(base_models)
  } else {
  	model_names = unique(c("ensemble", model_names))
  }
  out = sapply(model_names, function(model_name){
    model =  base_models[[model_name]]
    if (task=="Classification") {
      if (inherits(model, "caretEnsemble")) {
        positive_class = model$ens_model$levels[[2]]
      } else{
        positive_class = model$levels[[2]]
      }
    } else {
      positive_class = NULL
    }
    out = compute_shap_single(
      model = model
      , newdata=newdata
      , response = response
      , task = task
      , positive_class = positive_class
      , model_name = model_name
      , nsim = nsim
      , max_n = max_n
    )
    return(out)
  }, simplify = FALSE)
  out = extract_shap(out, top_n_rank, total_n_rank)
  class(out) = c("Rautomlshap", class(out))
  return(out)
}

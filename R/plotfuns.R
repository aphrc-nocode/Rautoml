#' GGplot Theme for metrics
#' 
#' @import ggplot2

metric_theme = function(){
 theme_bw(base_size = 14) +
    theme(panel.spacing=grid::unit(0,"lines")
    	, plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
		, axis.ticks.y = element_blank()
		, axis.text.x = element_text(size = 12)
		, axis.text.y = element_text(size = 12)
		, axis.title.x = element_text(size = 12)
		, axis.title.y = element_text(size = 12)
		, legend.title = element_text(size = 12, hjust = 0.5)
		, legend.text = element_text(size=12)
		, panel.grid.major = element_blank()
		, legend.key.size = unit(0.8, "cm")
		, legend.key = element_rect(fill = "white")
		, panel.spacing.y = unit(0.3, "lines")
		, panel.spacing.x = unit(1, "lines")
		, strip.background = element_blank()
		, strip.text.x = element_text(size = 12
			, colour = "black"
			, face = "bold"
		)
  )
}

#' Round off estimates 

nice_round = function(x, y, z) {
  s = paste0(round(x, 3), "[", round(y, 3), ", ", round(z, 3), "]")
  return(s)
}



#' Visualize predictions
#'
#' @import ggplot2 dplyr
#' @export
#'
viz_pred = function(df) {
	x = get_type(df$value)
	if (x == "numeric") {
		p = (ggplot2::ggplot(df, aes(x = model_name, y = value, fill = model_name))
			+ ggplot2::geom_boxplot(alpha = 0.5)
			+ ggplot2::labs(x = "Model", y = "Predicted value")
			+ ggplot2::guides(fill = "none")
		)
	} else {
		p = (ggplot2::ggplot(df, aes(x = model_name, fill = value))
			+ ggplot2::geom_bar(position = "dodge", alpha = 0.8, color = "black")
			+ ggplot2::labs(x = "Model", y = "Count")
			+ ggplot2::theme(legend.position = "bottom")
		)
	}
	p = (p
		+ ggplot2::theme_minimal(base_size = 14)
	)
	return(p)
}


#' Variable importance plot for compute_shap object
#'
#' @param x compute_shap object
#'
#' @import ggplot2
#'

plot_varimp = function(x, ..., pos = 0.5, drop_zero = TRUE, top_n=NULL){

	x <- x[order(x$estimate), ]
	if (drop_zero){
		x <- x[x$estimate!=0, ]
	}
	x <- x[order(x$estimate, decreasing=TRUE), ]
	if (!is.null(top_n)) {
		x <- x[1:top_n, ]
	}
	x <- droplevels(x)

  estimate <- NULL
  lower <- NULL
  upper <- NULL
  nmods <- unique(x$model)
  pos <- position_dodge(width = pos)
  if (length(nmods)==1) {
    p0 <- ggplot(x, aes(x = reorder(variable, estimate), y = estimate))
  } else {
    p0 <- (ggplot(x, aes(x = reorder(variable, estimate), y = estimate, colour = model))
       + labs(colour = "Model")
    )
  }
	p0 <- (p0
		+ geom_point(position = pos)
		+ geom_linerange(aes(ymin=lower, ymax=upper), position=pos)
	)
	p1 <- (p0
		+ scale_colour_viridis_d(option = "inferno")
		+ labs(x = "", y = "|SHAP value|")
		+ theme_minimal(base_size = 14)
		+ coord_flip(clip = "off", expand = TRUE)
	)
	if (length(unique((x$Class)))>1) {
	  p1 = (p1
	    + facet_grid(Class~.)
	  )
	}
	return(p1)
}

#' Variable dependency plot for compute_shap object
#'
#' @param x compute_shap object
#'
#' @import ggplot2

plot_vardep = function(x) {
  nmods <- unique(x$model)
  if (length(nmods)>1) {
    p = (ggplot(x, aes(x=observed, y=score, color=model))
      + geom_smooth(aes(color=model), se = FALSE)
      + geom_point(color="grey", alpha = 0.5)
    )
  } else {
    p = (ggplot(x, aes(x=observed, y=score))
      + geom_smooth(color = "red", se = FALSE)
      + geom_point(alpha = 0.5, color = "steelblue")
    )
  }
  p = (p
    + labs(x = "Observed values", y="SHAP value")
    + theme_minimal(base_size = 14)
  )
	if (length(unique((x$Class)))>1) {
	  p = (p
	    + facet_grid(Class~variable, scales = "free")
	  )
	} else {
	  p = p + facet_wrap(~variable, scales = "free")
	}
  return(p)
}

#' Most frequently selected important variables plot for compute_shap object
#'
#' @param x compute_shap object
#'
#' @import ggplot2

plot_varfreq = function(x) {
  p = (ggplot(x, aes(x=pos, y=fct_reorder(new_terms, -pos, .fun=mean), fill=n))
  	+ geom_tile(color="black")
  	+ scale_fill_distiller(palette = "Greens", direction=1)
  	+ scale_y_discrete(expand=c(0,0))
  	+ scale_x_continuous(
  		breaks=function(x){1:max(x)}
  		, labels=function(x){
  			m = max(x)
  			v = as.character(1:m)
  			v[[m]] = paste0(">", m-1)
  			return(v)
  		}
  		, expand=c(0,0)
  	)
  	+ labs(y="", x="Rank", fill="Frequency")
  	+ theme_bw(base_size=14)
  	+ theme(
  		strip.background = element_blank()
  		, panel.border = element_rect(colour = "grey"
  			, fill = NA
  			, size = 0.8
  		)
  		, strip.text.x = element_text(size = 11
  			, colour = "black"
  			, face = "bold"
  		)
  	)
  )
  if (length(unique(x$Class))>1) {
    p = p + facet_wrap(~Class)
  }
  return(p)
}

#' Collect SHAP viz plots 
#'
#' @param vv compute_shap object with named models
#' @param type various sv objets, either "sv_bw",  "sv_wf" or "sv_f"
#'
#' @import ggplot2
#'

combine_shap_plots = function(vv, type) {
  find_type <- function(x, name_path = "") {
    plots <- list()
    if (is.list(x)) {
      # If this level contains the target element, capture it
      if (!is.null(x[[type]])) {
        plots[[name_path]] <- x[[type]]
      }
      # Recurse into sublists
      for (n in names(x)) {
        sub_name <- if (name_path == "") n else paste(name_path, n, sep = "$")
        plots <- c(plots, find_type(x[[n]], sub_name))
      }
    }
    return(plots)
  }

  all_plots <- find_type(vv)

  if ("" %in% names(all_plots)) names(all_plots)[names(all_plots) == ""] <- "root"

  plots <- Map(function(p, nm) {
    if (inherits(p, "gg")) {
      p + theme_bw(base_size = 14) + ggtitle(nm)
    } else NULL
  }, all_plots, names(all_plots))

  plots <- plots[!sapply(plots, is.null)]

  if (length(plots) > 0) {
    patchwork::wrap_plots(plots) +
      patchwork::plot_layout(guides = "collect", axis_titles = "collect")
  } else {
    message("No plots found for type = ", type)
    NULL
  }
}


## combine_shap_plots = function(vv, type) {
##   plots = Map(function(m, name) {
##     if (!is.null(m[[type]])) {
##       m[[type]] + theme_bw() + ggtitle(name)
##     } else {
##       NULL
##     }
##   }, vv, names(vv))
##   
##   plots = plots[!sapply(plots, is.null)]
##   plots = patchwork::wrap_plots(plots) + patchwork::plot_layout(guides = "collect", axis_titles = "collect")
##   return(plots)
## }

#' Plot variable importance, most frequently identfied variables, dependency plots, and the SHAP values plots
#'
#' @param x compute_shap object
#'
#' @import ggplot2
#'
#' @export

plot.Rautomlshap = function(x, pos = 0.5, drop_zero = TRUE, top_n=NULL, ...) {
  varimp_ = plot_varimp(x$varimp_df, pos=pos, drop_zero=drop_zero, top_n=top_n)
  varfreq_ = plot_varfreq(x$varfreq_df)
  vardep_ = plot_vardep(x$vardep_df)
  sv_vis_ = x$sv_viz
  beeswarm = combine_shap_plots(sv_vis_, "sv_bw")
  waterfall = combine_shap_plots(sv_vis_, "sv_wf")
  force  = combine_shap_plots(sv_vis_, "sv_f")
  out = list(varimp = varimp_
  	, varfreq=varfreq_
	, vardep=vardep_
	, beeswarm=beeswarm
	, waterfall=waterfall
	, force=force
  )
  return(out)
}




#' Univariate plots
#'
#' Plot univariate variable for a quick visualization.
#'
#' @param df data frame object.
#' @param var variable of interest.
#'
#' @return a ggplot object.
#'
#' @import ggplot2
#'
#' @export
#'

ggunivariate = function(df, vartype, max_nlevels=30) {
	var = colnames(df)
	df = (df
		|> dplyr::rename_at(var, ~c(".x_var"))
	)
	
	if (any(vartype %in% c("factor", "character"))) {
		df = (df
			|> group_by(.x_var)
			|> count(name = ".nxx")
			|> ungroup()
			|> mutate(prop = .nxx/sum(.nxx))
			|> arrange(desc(prop))
			|> mutate(pos = n():1, .gg="#31859C")
		)
		
		n_levels = length(unique(df |> pull(.x_var)))
		max_nlevels = n_levels - max_nlevels
		if (n_levels > max_nlevels) {
		  df = (df
				  |> dplyr::mutate(
					 .x_var = dplyr::if_else(pos <= max_nlevels, "Other categories (lumped)", .x_var)
				  )
				  |> ungroup()
				  |> group_by(.x_var)
				  |> mutate(
					 prop = dplyr::if_else(pos <= max_nlevels, sum(prop), prop)
					 , pos = dplyr::if_else(pos <= max_nlevels, max_nlevels, pos)
				  )
				  |> dplyr::distinct(.x_var, .keep_all = TRUE)
		  )
		}

		p1 = (ggplot(df, aes(x = reorder(.x_var, pos), y=prop, label = scales::percent(prop), fill=.gg))
			+ scale_fill_identity()
			+ geom_col(position="dodge")
			+ scale_y_continuous(expand = c(0, 0), limits=c(0, max(df$prop, na.rm=TRUE)+0.15))
			+ scale_x_discrete(expand = c(0, 0))
			+ geom_text(position=position_dodge(width=0.9), hjust=5, size=4)
			+ base_theme()
			+ coord_flip()
			+ theme(axis.ticks.x = element_blank()
				, axis.text.x = element_blank()
				, axis.line.x = element_blank()
				, axis.title.x = element_blank()
				, axis.title.y = element_blank()
				, legend.position="none"
			)
		)
	} else {
		p1 = (ggplot(df, aes(x=.x_var))
		  + geom_histogram(aes(y = after_stat(density), fill="#31859C"))
		  + scale_fill_identity()
		  + geom_density(size = 0.8, linetype=2, colour="#800000")
		  + scale_y_continuous(expand = c(0, 0))
		  + scale_x_continuous(expand = c(0, 0))
		  + base_theme()
		  + theme(
				axis.title.x = element_blank()
				, axis.title.y = element_blank()
				, legend.position="none"
		  )
		  
		)	
	}
	p1 = (plotly::ggplotly(p1, tooltip="none") 
		|> plotly::style(hoverinfo = "all", textposition = "right")
	)
	return(p1)
}

#' Base these for Rautoml
#'
#' @import ggplot2
#'

base_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank()
		, panel.grid.minor = element_blank()
		, axis.line = element_line(size = .1, color = "#BFBEBE")
		, axis.text = element_text(size=12, color = "black")
		, axis.ticks.x = element_line(size = 0.5, color = "#BFBEBE")
		, axis.ticks.y = element_line(size = 0.5, color = "#BFBEBE")
		, axis.title = element_text(color = "#555655")
		, axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15, "pt"))
		, axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 15, 0, "pt"))
		, plot.subtitle = element_text(color = "#646369", size= 8)
		, plot.title = element_text(color = "#646369", size= 12)
		, plot.title.position = "plot" # This aligns the plot title to the very left edge
		, plot.caption = element_text(hjust = 0, color = "#828282")
		, plot.caption.position = "plot"
		, plot.margin = margin(.5,.5,.5,.5,"cm")
		, strip.text = element_text(color = "#929497")) 
}

#' Visualise missing data
#' @details The function provides a summary of the missing data
#'
#' @param df data frame
#'
#' @importFrom naniar gg_miss_var
#'
#' @import ggplot2
#'
#' @export
#'

missing_data_plot = function(df, ...) {
	p1 = (gg_miss_var(df, show_pct = TRUE, ...) 
		+ ylim(0, 100)
		+ base_theme()
	)
	return(p1)
}

#' Plot metrics from model training
#'
#' @param metric
#'
#' @import ggplot2
#'
#' @export

plot.Rautomlmetric = function(metric) {
  p1 = (ggplot(metric, aes(x=reorder(model, -estimate), y=estimate, group=metric))
    + geom_point()
    + geom_pointrange(aes(ymin = lower, ymax = upper))
    + labs(x = "Model", y="Score")
    + metric_theme()
  )
  nclasses = length(unique(metric$model))
  if (nclasses>3) {
  	p1 = (p1
		+ coord_flip()
    	+ facet_wrap(~metric, scales = "free_x")
	)
  } else {
  	p1 = (p1
   	+ facet_wrap(~metric, scales = "free_y")
	)
  }
  return(p1)
}

#' Plot ROC curve
#'
#' @param est 
#'
#' @import ggplot2
#'
#' @export
#'

plot.Rautomlroc = function(est) {
roc_plot = (ggplot(est, aes(x = x, y = y, group = model, colour = reorder(model, -estimate)))
	+ geom_line()
	+ scale_x_continuous(expand=expansion(mult=0.01), limits = c(0, 1))
	+ scale_y_continuous(expand=expansion(mult=0.01), limits = c(0, 1))
	+ ggthemes::scale_colour_colorblind()
   + geom_abline(intercept = 0, slope = 1, colour = "darkgrey", linetype = 2)
	+ labs(x = "False positive rate"
		, y = "True positive rate"
		, colour = "Model"
	)
	+ metric_theme()
	+ theme(legend.position="right")
)
}



#' Plot calibration (reliability) curves
#'
#' @details Plots binned observed-vs-predicted calibration points (sized by
#'   bin count) with confidence intervals, a diagonal line of perfect
#'   calibration, and, by default, a smoothed calibration curve through the
#'   raw (unbinned) predictions, computed by `calibration_smooth()` (loess,
#'   falling back to a linear fit, with a warning naming any model it can't
#'   fit at all rather than just leaving its curve off silently). The smooth
#'   curve is mapped to both colour and linetype, so two models with
#'   identical or near-identical curves -- for instance a `caretEnsemble`
#'   stack whose greedy weighting assigned one base learner all the weight,
#'   making the ensemble numerically identical to it -- stay visually
#'   distinguishable rather than one line fully hiding the other.
#'
#'   For classification, axes are fixed to the `[0, 1]` probability scale.
#'   For regression, axes share a common range computed from the data, so
#'   the diagonal reference line means the same thing on both axes.
#'
#' @param x an object of class `Rautomlcalibration`, from `compute_calibration()`
#' @param show_smooth logical, add a smoothed calibration curve (default `TRUE`)
#' @param show_ci logical, show confidence intervals on the binned points (default `TRUE`)
#'
#' @import ggplot2
#'
#' @export
#'

plot.Rautomlcalibration = function(x, show_smooth = TRUE, show_ci = TRUE, ...) {
  bins = x$bins
  raw = x$raw

  if (is.null(bins) || nrow(bins) == 0) {
    stop("Nothing to plot: compute_calibration() returned no calibration data (see the warning it raised).")
  }

  is_regression = identical(x$problem_type, "Regression")

  if (is_regression) {
    lims = range(c(bins$x, bins$y, raw$x, raw$y, bins$lower, bins$upper), na.rm = TRUE)
    lims = lims + c(-1, 1) * diff(lims) * 0.03
    x_lab = "Mean predicted value"
    y_lab = "Mean observed value"
  } else {
    lims = c(0, 1)
    x_lab = "Mean predicted probability"
    y_lab = "Observed proportion"
  }

  p = (ggplot(bins, aes(x = x, y = y, colour = model))
    + geom_abline(intercept = 0, slope = 1, colour = "darkgrey", linetype = 2)
  )

  if (isTRUE(show_smooth)) {
    smooth_df = calibration_smooth(raw)
    if (!is.null(smooth_df)) {
      p = (p
        ## linetype (in addition to colour) so two models with identical or
        ## near-identical curves -- e.g. a greedy caretEnsemble stack that
        ## assigned one base learner 100% weight -- don't have one line fully
        ## hide the other
        + geom_line(data = smooth_df, aes(x = x, y = y, colour = model, linetype = model))
      )
    }
  }

  if (isTRUE(show_ci)) {
    p = (p
      + geom_linerange(aes(ymin = lower, ymax = upper), alpha = 0.6)
    )
  }

  p = (p
    + geom_point(aes(size = n))
    + scale_x_continuous(expand = expansion(mult = 0.01), limits = lims)
    + scale_y_continuous(expand = expansion(mult = 0.01), limits = lims)
    + ggthemes::scale_colour_colorblind()
    + labs(x = x_lab
      , y = y_lab
      , colour = "Model"
      , linetype = "Model"
      , size = "Bin size"
    )
    + metric_theme()
    + theme(legend.position = "right")
  )

  if ("Class" %in% colnames(bins)) {
    p = (p
      + facet_wrap(~Class)
    )
  }

  return(p)
}



#' Plot metrics from model test
#'
#' @param est
#'
#' @import ggplot2
#'
#' @export
#'

plot.Rautomlmetric2 = function(est) {
	specifics = est$specifics
	if (!is.null(specifics)) {
		class(specifics) = c("Rautomlmetric", class(specifics))
		specifics_plot = plot(specifics)
	} else {
		specifics_plot = NULL
	}
	all = est$all
	class(all) = c("Rautomlmetric", class(all))
	all_plot = plot(all)

	roc_df = est$roc_df
	if (isTRUE(!is.null(roc_df))) {
		if (isTRUE(is.null(roc_df$.check_))) {
			roc_df = (roc_df
				|> dplyr::left_join(
					all
					|> dplyr::filter(metric=="AUC")
					|> dplyr::arrange(desc(estimate))
					|> select(-metric)
					, by = c("model")
				)
				|> mutate(
				  model = paste0(model, ": ", nice_round(estimate, lower, upper))
				)
			)
			class(roc_df) = c("Rautomlroc", class(roc_df))
			roc_plot = plot(roc_df)
		} else {
			roc_plot = (ggplot2::ggplot(roc_df, aes(x=x, y=y, colour=model))
				+ ggplot2::geom_point(alpha=0.4) 
				+ geom_smooth(method = lm, aes(colour=model), se = TRUE)
				+ ggplot2::geom_abline(slope=1, intercept=0, color="black") 
				+ ggplot2::labs(title="Predicted vs Actual", x="Actual", y="Predicted")
				+ theme_minimal(base_size = 12)
			)
		}
		if (!is.null(roc_df$Class)) {
		 roc_plot = (roc_plot
			+ facet_wrap(~Class)
		 )	
		}
	} else {
		roc_plot = NULL
	}
	return(list(specifics=specifics_plot, all=all_plot, roc=roc_plot))
}



#' Plot Brier score and its calibration/discrimination decomposition
#'
#' @details Three side-by-side panels, one row per model in each: the Brier
#'   score itself, its reliability component (calibration error), and its
#'   resolution component (discrimination ability). Models are ordered by
#'   Brier score (best at top) consistently across all three panels, so
#'   scanning across a row shows whether a model's score comes from good
#'   calibration, good discrimination, or both.
#'
#'   Uncertainty (the fourth decomposition term) isn't plotted per model,
#'   since it depends only on the outcome's base rate and comes out the same
#'   for every model on the same test set -- it's reported once in the
#'   subtitle instead of as a row of identical points.
#'
#'   This is a horizontal layout (models on the y-axis) with no per-model
#'   colour legend, so it scales to many models by growing taller rather
#'   than wider or busier; axis text shrinks a little once there are more
#'   than about 8 models, and again past 15, to keep rows readable.
#'
#' @param x an object of class `Rautomlbrier`, from `compute_brier()`
#'
#' @import ggplot2
#'
#' @export
#'

plot.Rautomlbrier = function(x, ...) {
  scores = x$scores
  if (is.null(scores) || nrow(scores) == 0) {
    stop("Nothing to plot: compute_brier() returned no scores.")
  }

  n_models = length(unique(scores$model))
  model_order = scores$model[order(scores$brier)]

  long = (scores
    |> dplyr::select(model, brier, reliability, resolution)
    |> tidyr::pivot_longer(cols = c(brier, reliability, resolution), names_to = "metric", values_to = "value")
    |> dplyr::mutate(
      model = factor(model, levels = rev(model_order))
      , metric = factor(metric
        , levels = c("brier", "reliability", "resolution")
        , labels = c("Brier score\n(lower is better)", "Reliability\n(lower is better)", "Resolution\n(higher is better)")
      )
    )
  )

  axis_text_size = if (n_models > 15) 9 else if (n_models > 8) 10.5 else 12
  point_size = if (n_models > 15) 1.8 else 2.5

  uncertainty_val = signif(mean(scores$uncertainty, na.rm = TRUE), 4)

  p = (ggplot(long, aes(x = value, y = model))
    + geom_point(size = point_size, colour = "#2c5f7c")
    + facet_wrap(~metric, scales = "free_x", nrow = 1)
    + labs(x = NULL
      , y = NULL
      , title = "Brier score and its calibration / discrimination breakdown"
      , subtitle = paste0("Uncertainty (base-rate variance, same for every model): ", uncertainty_val)
    )
    + metric_theme()
    + theme(
      axis.text.y = element_text(size = axis_text_size)
      , axis.text.x = element_text(size = axis_text_size)
      , plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "grey30")
    )
  )

  return(p)
}

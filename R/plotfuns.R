#' Visualize predictions
#'
#'
#' @export
#'
viz_pred = function(df) {
	x = get_type(df$value)
	if (x == "numeric") {
		p = (ggplot(df, aes(x = model_name, y = value, fill = model_name))
			+ geom_boxplot(alpha = 0.5)
			+ labs(x = "Model", y = "Predicted value")
			+ guides(fill = "none")
		)
	} else {
		p = (ggplot(df, aes(x = model_name, fill = value))
			+ geom_bar(position = "dodge", alpha = 0.8, color = "black")
			+ labs(x = "Model", y = "Predicted categroy")
			+ theme(legend.position = "bottom")
		)
	}
	p = (p
		+ theme_minimal(base_size = 14)
	)
	return(p)
}


#' Variable importance plot for compute_shap object
#'
#' @param x compute_shap object

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
		+ theme_minimal()
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
    + theme_minimal()
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
  	+ theme_bw(base_size=12)
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

combine_shap_plots = function(vv, type) {
  plots = Map(function(m, name) {
    if (!is.null(m[[type]])) {
      m[[type]] + theme_bw() + ggtitle(name)
    } else {
      NULL
    }
  }, vv, names(vv))
  
  plots = plots[!sapply(plots, is.null)]
  plots = patchwork::wrap_plots(plots) + patchwork::plot_layout(guides = "collect", axis_titles = "collect")
  return(plots)
}

#' Plot variable importance, most frequently identfied variables, dependency plots, and the SHAP values plots
#'
#' @param x compute_shap object
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
			|> mutate(pos = n():1, .gg=GREEN3)
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


base_theme <- function() {
  theme_minimal(base_size = 12) +
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
#' @export
#'

plot.Rautomlroc = function(est) {
roc_plot = (ggplot(est, aes(x = x, y = y, group = model, colour = reorder(model, -estimate)))
	+ geom_line()
	+ scale_x_continuous(limits = c(0, 1))
	+ scale_y_continuous(limits = c(0, 1))
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



#' Plot metrics from model test
#'
#' @param est
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


#' GGplot Theme for metrics
#'

metric_theme = function(){
 theme_bw() +
    theme(panel.spacing=grid::unit(0,"lines")
    	, plot.title = element_text(hjust = 0.5)
		, legend.position = "bottom"
		, axis.ticks.y = element_blank()
		, axis.text.x = element_text(size = 12)
		, axis.text.y = element_text(size = 12)
		, axis.title.x = element_text(size = 12)
		, axis.title.y = element_text(size = 12)
		, legend.title = element_text(size = 12, hjust = 0.5)
		, legend.text = element_text(12)
		, panel.grid.major = element_blank()
		, legend.key.size = unit(0.8, "cm")
		, legend.key = element_rect(fill = "white")
		, panel.spacing.y = unit(0.3, "lines")
		, panel.spacing.x = unit(1, "lines")
		, strip.background = element_blank()
		, strip.text.x = element_text(size = 11
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


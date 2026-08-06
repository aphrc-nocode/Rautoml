#' Convert a two-valued outcome to a 0/1 indicator
#'
#' @details Used by `calibration_bin()` when `obs` isn't already numeric
#'   0/1 -- a logical vector, or a 2-level factor/character vector such as
#'   `c("No", "Yes")`. Picks the positive value the same way the rest of
#'   Rautoml picks a positive class: the second value once sorted (which,
#'   for a factor with its own level order such as `factor(x, levels =
#'   c("Yes", "No"))`, respects that order rather than resorting
#'   alphabetically, matching `levels(y)[2]` in `calibration_measures()`),
#'   unless `positive_class` says otherwise. Reports which value it picked
#'   so the choice isn't made silently.
#'
#' @param obs a vector with at most 2 distinct non-missing values
#' @param positive_class the value of `obs` to treat as the positive/event
#'   class (1); if `NULL` (default), the second value once sorted is used
#'
#' @return a 0/1 numeric vector
#'

calibration_as_binary = function(obs, positive_class = NULL) {
  vals = sort(unique(obs))
  if (length(vals) > 2) {
    stop("calibration_as_binary(): more than 2 distinct values in `obs` (", paste(vals, collapse = ", "), ").")
  }
  if (is.null(positive_class)) {
    pos = vals[length(vals)]
  } else {
    if (!(positive_class %in% vals)) {
      stop("calibration_as_binary(): positive_class '", positive_class, "' not found in `obs` (values present: ", paste(vals, collapse = ", "), ").")
    }
    pos = positive_class
  }
  message("calibration_bin(): treating obs == '", pos, "' as the positive class (1). Pass `positive_class` to choose a different one.")
  return(as.numeric(obs == pos))
}

#' Bin predictions and compute observed values, with a confidence interval
#'
#' @details Groups predictions into `n_bins` bins (equal-count `"quantile"`
#'   bins, or equal-width `"uniform"` bins) and computes the mean prediction
#'   and the mean observed outcome within each bin.
#'
#'   For `outcome_type = "binary"` (classification calibration), the
#'   interval on the observed rate is a Wilson score interval, which is more
#'   reliable than a normal-approximation interval when a bin is small or
#'   the rate is near 0/1. `obs` doesn't need to already be numeric 0/1: a
#'   logical vector, or any vector with exactly 2 distinct values (`c("No",
#'   "Yes")`, `c("control", "case")`, `0`/`1`, `1`/`2`, ...) is converted
#'   automatically via `calibration_as_binary()`.
#'
#'   For `outcome_type = "continuous"` (regression calibration, `obs` is the
#'   raw outcome), the interval is a Student-t interval on the mean, the same
#'   formula used by `student_t_summary()` elsewhere in Rautoml.
#'
#'   With the default `outcome_type = "auto"`, the type is inferred from
#'   `obs`: a vector with at most 2 distinct values is `"binary"`; a numeric
#'   vector with more is `"continuous"`. A *non-numeric* vector with more
#'   than 2 distinct values (multiclass labels, e.g. `c("Low", "Mid",
#'   "High")`) can't be summarized by a single call -- `calibration_bin()`
#'   computes one binary or continuous outcome at a time, and averaging
#'   unordered category labels isn't meaningful -- so this raises an
#'   informative error rather than silently returning `NA`s (which is what
#'   `mean()` on category labels does). For multiclass outcomes, call this
#'   once per class with a 0/1 one-vs-rest indicator: `calibration_measures()`
#'   / `compute_calibration()` already do exactly that automatically.
#'   Numeric `obs` with more than 2 distinct values is always treated as
#'   continuous -- if you have multiclass labels encoded as numbers (`1`,
#'   `2`, `3`, ...), convert to a factor first, or one-vs-rest indicators,
#'   rather than relying on auto-detection here.
#'
#'   This auto-detection is a convenience for calling `calibration_bin()`
#'   directly. `calibration_measures()` always passes `outcome_type`
#'   explicitly instead, since it already knows it with certainty from the
#'   model's `modelType` and shouldn't guess -- a regression outcome that
#'   happens to be 0/1-coded in a given sample (a linear probability model,
#'   for instance) would otherwise be misread as classification.
#'
#' @param pred numeric vector of predictions (predicted probability for
#'   classification, predicted value for regression)
#' @param obs vector of observed outcomes, same length as `pred`: for
#'   `outcome_type = "binary"` any 2-valued vector (0/1, logical, or labels
#'   like `"No"`/`"Yes"`); for `outcome_type = "continuous"` the raw numeric
#'   outcome
#' @param n_bins number of bins
#' @param strategy `"quantile"` (equal-count bins, default) or `"uniform"` (equal-width bins)
#' @param outcome_type `"auto"` (default, inferred from `obs`), `"binary"`, or `"continuous"`
#' @param positive_class for a binary `obs` that isn't already 0/1, which
#'   value counts as the positive/event class (1); if `NULL` (default), the
#'   second value once sorted is used (see `calibration_as_binary()`)
#' @param conf_level confidence level for the interval on the observed value
#'
#' @return a data.frame with columns `x` (mean prediction in the bin), `y`
#'   (mean observed outcome), `n` (bin size), `lower`, `upper` (confidence
#'   interval on `y`)
#'

calibration_bin = function(pred, obs, n_bins = 10, strategy = c("quantile", "uniform"), outcome_type = c("auto", "binary", "continuous"), positive_class = NULL, conf_level = 0.95) {
  strategy = match.arg(strategy)
  outcome_type = match.arg(outcome_type)

  keep = !is.na(pred) & !is.na(obs)
  pred = pred[keep]
  obs = obs[keep]

  n_distinct = length(unique(obs))
  obs_is_numlike = is.numeric(obs) || is.logical(obs)

  ## Non-numeric obs with more than 2 categories is multiclass-shaped data
  ## that a single call can't summarize -- error clearly instead of letting
  ## mean()/sd() silently turn category labels into NA.
  if (!obs_is_numlike && n_distinct > 2) {
    shown = utils::head(sort(unique(as.character(obs))), 5)
    stop(
      "calibration_bin(): `obs` has ", n_distinct, " categories (",
      paste(shown, collapse = ", "), if (n_distinct > length(shown)) ", ...", "). ",
      "calibration_bin() bins one binary or continuous outcome at a time, and ",
      "averaging unordered category labels isn't meaningful. For multiclass ",
      "outcomes, call this once per class with a 0/1 one-vs-rest indicator -- ",
      "calibration_measures()/compute_calibration() already do this ",
      "automatically for multiclass models."
    )
  }

  if (outcome_type == "auto") {
    outcome_type = if (n_distinct <= 2) "binary" else "continuous"
  }

  if (outcome_type == "binary") {
    if (n_distinct > 2) {
      stop("calibration_bin(): outcome_type = \"binary\" but `obs` has ", n_distinct, " distinct values; expected at most 2.")
    }
    if (is.logical(obs)) {
      obs = as.numeric(obs)
    } else if (!(is.numeric(obs) && all(obs %in% c(0, 1)))) {
      obs = calibration_as_binary(obs, positive_class = positive_class)
    }
  } else if (!is.numeric(obs)) {
    stop("calibration_bin(): outcome_type = \"continuous\" requires numeric `obs`.")
  }

  if (strategy == "quantile") {
    breaks = stats::quantile(pred, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE, type = 7)
  } else {
    breaks = seq(min(pred, na.rm = TRUE), max(pred, na.rm = TRUE), length.out = n_bins + 1)
  }
  breaks = unique(breaks)
  if (length(breaks) < 2) {
    breaks = c(breaks - .Machine$double.eps, breaks + .Machine$double.eps)
  }

  bin_df = data.frame(pred = pred, obs = obs, bin = cut(pred, breaks = breaks, include.lowest = TRUE))
  out = (bin_df
    |> dplyr::group_by(bin)
    |> dplyr::summarise(
      x = mean(pred, na.rm = TRUE)
      , y = mean(obs, na.rm = TRUE)
      , n = dplyr::n()
      , sdev = stats::sd(obs, na.rm = TRUE)
      , .groups = "drop"
    )
    |> dplyr::arrange(x)
  )

  if (outcome_type == "binary") {
    ## Wilson score interval on the observed rate y
    z = stats::qnorm(1 - (1 - conf_level) / 2)
    out = (out
      |> dplyr::mutate(
        denom = 1 + (z^2) / n
        , center = (y + (z^2) / (2 * n)) / denom
        , half_width = (z * sqrt((y * (1 - y) / n) + (z^2) / (4 * n^2))) / denom
        , lower = pmax(0, center - half_width)
        , upper = pmin(1, center + half_width)
      )
    )
  } else {
    ## Student-t interval on the mean observed value, same formula as
    ## student_t_summary() in metrics.R. A bin with a single observation has
    ## no estimable spread, so its interval is NA rather than a qt() warning.
    out = (out
      |> dplyr::mutate(
        se = sdev / sqrt(n)
        , t_crit = ifelse(n > 1, stats::qt(1 - (1 - conf_level) / 2, df = pmax(n - 1, 1)), NA_real_)
        , error_margin = t_crit * se
        , lower = y - error_margin
        , upper = y + error_margin
      )
    )
  }

  out = (out
    |> dplyr::select(x, y, n, lower, upper)
  )
  return(out)
}

#' Compute a smoothed calibration curve per model
#'
#' @details Used by `plot.Rautomlcalibration()` in place of
#'   `ggplot2::geom_smooth()`. `geom_smooth()` fits each colour group's curve
#'   internally during the plot build, and if that fit degenerates for one
#'   group -- most commonly a model whose predictions have very little
#'   variation -- ggplot2 can silently drop that group's line with no
#'   warning at all. The rest of the plot renders normally and the missing
#'   curve is easy to miss, since the binned points for that model still
#'   show up fine (they don't go through this code path).
#'
#'   This computes each model's (and, for multiclass, each model/Class
#'   combination's) curve explicitly instead, so a failure is visible rather
#'   than silent: loess first, falling back to a linear fit if loess errors
#'   or returns no usable values, and only skipping a model (with a warning
#'   naming it) if neither can be fit -- which only happens when a model's
#'   predictions have essentially no variation to draw a curve through.
#'
#' @param raw a data.frame with `x`, `y`, `model`, and (for multiclass) `Class`
#' @param n number of points along each fitted curve
#'
#' @return a data.frame with `x`, `y` (fitted), `model`, and `Class` if
#'   present, or `NULL` if no model could be fit
#'

calibration_smooth = function(raw, n = 100) {
  group_cols = intersect(c("model", "Class"), colnames(raw))
  groups = unique(raw[, group_cols, drop = FALSE])

  smoothed = lapply(seq_len(nrow(groups)), function(i) {
    grp = groups[i, , drop = FALSE]
    sub = merge(raw, grp, by = group_cols)
    sub = sub[stats::complete.cases(sub[, c("x", "y")]), ]
    label = paste(unlist(grp), collapse = "/")

    if (nrow(sub) < 2 || length(unique(sub$x)) < 2) {
      warning("calibration smooth: not enough variation in predictions to fit a curve for ", label, "; skipping its smooth line.", call. = FALSE)
      return(NULL)
    }

    grid = seq(min(sub$x), max(sub$x), length.out = n)
    pred = NULL

    fit = tryCatch(suppressWarnings(stats::loess(y ~ x, data = sub)), error = function(e) NULL)
    if (!is.null(fit)) {
      pred = tryCatch(suppressWarnings(as.numeric(stats::predict(fit, newdata = data.frame(x = grid)))), error = function(e) NULL)
    }

    if (is.null(pred) || all(!is.finite(pred))) {
      fit = tryCatch(stats::lm(y ~ x, data = sub), error = function(e) NULL)
      if (!is.null(fit)) {
        pred = tryCatch(as.numeric(stats::predict(fit, newdata = data.frame(x = grid))), error = function(e) NULL)
      }
    }

    if (is.null(pred) || all(!is.finite(pred))) {
      warning("calibration smooth: couldn't fit a curve for ", label, "; skipping its smooth line.", call. = FALSE)
      return(NULL)
    }

    cbind(grp[rep(1, n), , drop = FALSE], data.frame(x = grid, y = pred))
  })

  smoothed = smoothed[!vapply(smoothed, is.null, logical(1))]
  if (length(smoothed) == 0) {
    return(NULL)
  }
  return(do.call(rbind, smoothed))
}

#' Compute calibration data for a single model
#'
#' @details Extracts predictions for a single trained caret `train` model (or
#'   a `caretEnsemble` stack) and bins them against the observed outcome to
#'   build calibration data.
#'
#'   For classification, follows the same prediction and positive-class
#'   conventions used in `boot_measures()`: for binary outcomes the positive
#'   class is the second factor level (`levels(y)[2]`); for multiclass
#'   outcomes, one-vs-rest calibration data is returned for every class,
#'   mirroring the multiclass ROC computation in `boot_measures()`. As in
#'   `compute_shap()`, a `caretEnsemble` object's class levels are read from
#'   `model$ens_model$levels` rather than `model$levels`.
#'
#'   For regression, mirrors `boot_measures()`'s Regression branch (raw point
#'   predictions binned against the observed outcome directly), using a
#'   Student-t interval instead of a Wilson interval per bin.
#'
#'   The problem type is read from `model$modelType` (or
#'   `model$ens_model$modelType` for a `caretEnsemble`) unless overridden
#'   with `problem_type`.
#'
#' @param model a trained `train` (caret) or `caretEnsemble` object
#' @param df data.frame containing predictors and the outcome
#' @param outcome_var name of the outcome column in `df`
#' @param n_bins number of calibration bins
#' @param strategy `"quantile"` (default) or `"uniform"` binning
#' @param type passed to `predict.train(..., type=)` for non-ensemble classification models
#' @param problem_type `"Classification"` or `"Regression"`; if `NULL`
#'   (default), read from `model$modelType`
#'
#' @return a list with elements `bins` (binned reliability data, see
#'   `calibration_bin()`), `raw` (per-observation prediction/observed pairs,
#'   used for the smoothed calibration curve), and `problem_type`. `bins` and
#'   `raw` have a `Class` column added for multiclass classification.
#'

calibration_measures = function(model, df, outcome_var, n_bins = 10, strategy = c("quantile", "uniform"), type = "prob", problem_type = NULL) {
  strategy = match.arg(strategy)
  if (!(outcome_var %in% colnames(df))) {
    stop("calibration_measures(): outcome_var '", outcome_var, "' not found in df. Available columns: ", paste(colnames(df), collapse = ", "), ".")
  }
  x_df = df[, setdiff(colnames(df), outcome_var), drop = FALSE]
  y = df[[outcome_var]]
  is_ens = inherits(model, "caretEnsemble")

  if (is.null(problem_type)) {
    problem_type = if (is_ens) model$ens_model$modelType else model$modelType
  }

  if (identical(problem_type, "Regression")) {
    preds = predict(model, newdata = x_df)
    if (!is.null(dim(preds))) {
      ## Some predict() methods -- notably caretEnsemble's predict.caretStack(),
      ## which every caretEnsemble()/caretStack() object dispatches to since
      ## there's no predict.caretEnsemble() -- return a single-column
      ## data.frame/data.table for regression rather than a plain vector.
      ## as.numeric() on the whole object would error ("'list' object cannot
      ## be coerced to type 'double'"); take the column instead.
      if (ncol(preds) != 1) {
        stop("calibration_measures(): expected a single regression prediction column from predict(), got ", ncol(preds), ".")
      }
      preds = preds[[1]]
    }
    raw = data.frame(x = as.numeric(preds), y = as.numeric(y))
    bins = calibration_bin(pred = raw$x, obs = raw$y, n_bins = n_bins, strategy = strategy, outcome_type = "continuous")
    return(list(bins = bins, raw = raw, problem_type = "Regression"))
  }

  if (is_ens) {
    preds = predict(model, newdata = x_df)
    model_levels = model$ens_model$levels
  } else {
    preds = predict(model, newdata = x_df, type = type)
    model_levels = model$levels
  }

  if (Rautoml::get_type(y) != "factor") {
    y = as.factor(y)
  }

  if (nlevels(y) == 2) {
    base_lev = levels(y)[2]
    levs = c(base_lev, setdiff(levels(y), base_lev))
  } else {
    levs = model_levels
    base_lev = NULL
  }
  y = factor(y, levels = levs)

  if (nlevels(y) == 2) {
    obs = as.numeric(y == base_lev)
    raw = data.frame(x = preds[[base_lev]], y = obs)
    bins = calibration_bin(pred = raw$x, obs = raw$y, n_bins = n_bins, strategy = strategy, outcome_type = "binary")
  } else {
    y_mat = model.matrix(~y - 1)
    colnames(y_mat) = levels(y)
    raw_list = list()
    bins_list = list()
    for (lev in levels(y)) {
      raw_list[[lev]] = data.frame(x = preds[[lev]], y = y_mat[, lev], Class = lev)
      bin_out = calibration_bin(pred = preds[[lev]], obs = y_mat[, lev], n_bins = n_bins, strategy = strategy, outcome_type = "binary")
      bin_out$Class = lev
      bins_list[[lev]] = bin_out
    }
    raw = do.call(rbind, raw_list)
    bins = do.call(rbind, bins_list)
  }

  return(list(bins = bins, raw = raw, problem_type = "Classification"))
}

#' Compute calibration data for a caretList of models
#'
#' @param models a `caretList` object
#' @param df test/validation data.frame containing predictors and the outcome
#' @param outcome_var name of the outcome column in `df`
#' @param n_bins number of calibration bins (default 10)
#' @param strategy `"quantile"` (equal-count bins, default) or `"uniform"` (equal-width bins)
#' @param type passed to `predict.train(..., type=)` for classification models
#' @param problem_type `"Classification"` or `"Regression"`; if `NULL`
#'   (default), auto-detected per model from `model$modelType`
#'
#' @export
#'

compute_calibration.caretList = function(models, df, outcome_var, n_bins = 10, strategy = c("quantile", "uniform"), type = "prob", problem_type = NULL) {
  strategy = match.arg(strategy)
  if (!(outcome_var %in% colnames(df))) {
    stop("compute_calibration(): outcome_var '", outcome_var, "' not found in df. Available columns: ", paste(colnames(df), collapse = ", "), ".")
  }
  out = lapply(names(models), function(nm) {
    model = models[[nm]]
    model_name = names(model$method)
    if (is.null(model_name)) {
      model_name = model$method
    }
    cal = tryCatch({
      calibration_measures(model = model, df = df, outcome_var = outcome_var, n_bins = n_bins, strategy = strategy, type = type, problem_type = problem_type)
    }, error = function(e) {
      warning("Skipping calibration for model '", model_name, "': ", conditionMessage(e), call. = FALSE)
      return(NULL)
    })
    if (!is.null(cal)) {
      cal$bins$model = model_name
      cal$raw$model = model_name
    }
    return(cal)
  })
  out = out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) {
    warning("compute_calibration(): no models produced calibration data.", call. = FALSE)
    res = list(bins = NULL, raw = NULL, problem_type = NA_character_)
    class(res) = c("Rautomlcalibration", class(res))
    return(res)
  }
  bins = do.call(rbind, lapply(out, `[[`, "bins"))
  raw = do.call(rbind, lapply(out, `[[`, "raw"))
  res = list(bins = bins, raw = raw, problem_type = out[[1]]$problem_type)
  class(res) = c("Rautomlcalibration", class(res))
  return(res)
}

#' Compute calibration data for a caretEnsemble stack
#'
#' @details Adds the stacked ensemble model itself (labelled `"ensemble"`,
#'   following the same convention used in `compute_shap.caretEnsemble()`) to
#'   the base learners before computing calibration data for each.
#'
#' @param models a `caretEnsemble` object
#' @param df test/validation data.frame containing predictors and the outcome
#' @param outcome_var name of the outcome column in `df`
#' @param n_bins number of calibration bins (default 10)
#' @param strategy `"quantile"` (equal-count bins, default) or `"uniform"` (equal-width bins)
#' @param type passed to `predict.train(..., type=)` for the base learners
#' @param problem_type `"Classification"` or `"Regression"`; if `NULL`
#'   (default), auto-detected per model from `model$modelType`
#'
#' @export
#'

compute_calibration.caretEnsemble = function(models, df, outcome_var, n_bins = 10, strategy = c("quantile", "uniform"), type = "prob", problem_type = NULL) {
  strategy = match.arg(strategy)
  if (!(outcome_var %in% colnames(df))) {
    stop("compute_calibration(): outcome_var '", outcome_var, "' not found in df. Available columns: ", paste(colnames(df), collapse = ", "), ".")
  }
  ens_model = models
  base_models = as.list(models$models)
  base_models$ensemble = ens_model
  base_models$ensemble$method = "ensemble"

  out = lapply(names(base_models), function(nm) {
    model = base_models[[nm]]
    model_name = names(model$method)
    if (is.null(model_name)) {
      model_name = model$method
    }
    cal = tryCatch({
      calibration_measures(model = model, df = df, outcome_var = outcome_var, n_bins = n_bins, strategy = strategy, type = type, problem_type = problem_type)
    }, error = function(e) {
      warning("Skipping calibration for model '", model_name, "': ", conditionMessage(e), call. = FALSE)
      return(NULL)
    })
    if (!is.null(cal)) {
      cal$bins$model = model_name
      cal$raw$model = model_name
    }
    return(cal)
  })
  out = out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) {
    warning("compute_calibration(): no models produced calibration data.", call. = FALSE)
    res = list(bins = NULL, raw = NULL, problem_type = NA_character_)
    class(res) = c("Rautomlcalibration", class(res))
    return(res)
  }
  bins = do.call(rbind, lapply(out, `[[`, "bins"))
  raw = do.call(rbind, lapply(out, `[[`, "raw"))
  res = list(bins = bins, raw = raw, problem_type = out[[1]]$problem_type)
  class(res) = c("Rautomlcalibration", class(res))
  return(res)
}

#' Compute Brier score and its calibration/discrimination decomposition
#'
#' @details Computes the Brier score for each model in an `Rautomlcalibration`
#'   object, along with its Murphy (1973) decomposition into reliability
#'   (calibration error), resolution (discrimination ability), and
#'   uncertainty (a property of the outcome's base rate, not the model).
#'
#'   The Brier score itself is the exact mean squared error between
#'   predicted probability and observed outcome (`mean((pred - obs)^2)`),
#'   computed from the raw, unbinned predictions -- not an approximation.
#'   Lower is better, and it's a loss, the opposite direction from AUC,
#'   Accuracy, and most other metrics elsewhere in Rautoml.
#'
#'   The decomposition is computed from the same bins `compute_calibration()`
#'   already built, so it approximates the exact score (`reliability -
#'   resolution + uncertainty ~= brier`); the gap shrinks as `n_bins`
#'   increases and is typically small at the default of 10.
#'
#'   For multiclass outcomes, the Brier score is the standard multi-category
#'   version: the sum, not the average, of the one-vs-rest binary Brier
#'   score for every class. This is mathematically exact (verified against
#'   the direct multi-category formula), not an approximation, and the same
#'   summing applies to each decomposition component.
#'
#'   Uncertainty depends only on the observed outcome's base rate, not on
#'   any model's predictions, so it comes out essentially identical across
#'   every model in `cal` (same test set, same true outcomes) -- it isn't
#'   something a model can improve on.
#'
#' @param cal an object of class `Rautomlcalibration`, from
#'   `compute_calibration()`, for a classification model (regression isn't
#'   supported -- Brier score requires predicted probabilities)
#'
#' @return an object of class `Rautomlbrier`: a list with a `scores`
#'   data.frame, one row per model, with columns `model`, `brier`,
#'   `reliability`, `resolution`, `uncertainty`
#'
#' @export
#'

compute_brier = function(cal) {
  if (!inherits(cal, "Rautomlcalibration")) {
    stop("compute_brier(): `cal` must be an Rautomlcalibration object, from compute_calibration().")
  }
  if (identical(cal$problem_type, "Regression")) {
    stop("compute_brier(): Brier score is a classification metric (it compares predicted probabilities to a binary/categorical outcome). `cal` is from a Regression model; use RMSE/MAE (e.g. via boot_estimates_multiple()) instead.")
  }
  if (is.null(cal$bins) || nrow(cal$bins) == 0) {
    stop("compute_brier(): `cal` has no calibration data (see the warning compute_calibration() raised).")
  }

  raw = cal$raw
  bins = cal$bins
  group_cols = intersect(c("model", "Class"), colnames(raw))

  ## Exact Brier score component per (model[, Class]), from the raw,
  ## unbinned predictions
  raw_bs = (raw
    |> dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
    |> dplyr::summarise(brier_component = mean((x - y)^2, na.rm = TRUE), .groups = "drop")
  )

  ## Murphy decomposition per (model[, Class]), from the existing bins
  decomp = (bins
    |> dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
    |> dplyr::summarise(
      bin_n = sum(n)
      , obar = sum(n * y) / bin_n
      , reliability = sum(n * (x - y)^2) / bin_n
      , resolution = sum(n * (y - obar)^2) / bin_n
      , .groups = "drop"
    )
    |> dplyr::mutate(uncertainty = obar * (1 - obar))
    |> dplyr::select(-obar, -bin_n)
  )

  combined = merge(raw_bs, decomp, by = group_cols)

  if ("Class" %in% group_cols) {
    scores = (combined
      |> dplyr::group_by(model)
      |> dplyr::summarise(
        brier = sum(brier_component)
        , reliability = sum(reliability)
        , resolution = sum(resolution)
        , uncertainty = sum(uncertainty)
        , .groups = "drop"
      )
    )
  } else {
    scores = (combined
      |> dplyr::rename(brier = brier_component)
      |> dplyr::select(model, brier, reliability, resolution, uncertainty)
    )
  }

  res = list(scores = as.data.frame(scores))
  class(res) = c("Rautomlbrier", class(res))
  return(res)
}

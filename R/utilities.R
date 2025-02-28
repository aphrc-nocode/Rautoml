#' Create directory for various artifacts
#'
#' @details Checks if the directory exists otherwise creates it.
#'
#' @param name a directory name or relative path.
#'
#' @return NULL
#'
#' @export

create_dir = function(name="new_folder") {
	dir.create(name, showWarnings = FALSE)
}


#'  Get file extension 
#'  
#' @details Given a file name/path, extract the file extension.
#'
#' @param file file name or file path with a .ext.
#'
#' @return a character specifying file extension.
#'
#' @export
#'

get_file_ext = function(file) {
	(strsplit(basename(file), split="\\.(?=[^\\.]+$)", perl = TRUE)[[1]])[2]
}

#' Get the file name without the extension 
#'
#' @details Given a file name/path, extract the file extension.
#'
#' @param file file name or file path with a .ext.
#'
#' @return a character specifying the file name.
#'
#' @export
#'

get_file_name = function(file) {
	x = (strsplit(basename(file), split="\\.(?=[^\\.]+$)", perl = TRUE)[[1]])[1]
	gsub("\\.+$", "", x)
}

#' Create dataset metadata
#'
#' Create a file with the information about the data.
#'
#' @param data uploaded dataset.
#' @param filename a character string. Can be file name of the data or a file path, with the correct file extension.
#' @param study_name a short description about the study or analysis.
#' @param study_country Country where the study was conducted or where the data was collected from.
#' @param upload_time timestamp indicating when the data was uploaded.
#' @param user user accessing the platform
#' @param group collaborating groups which have access to the data.
#'
#' @details The function collects user provided information, together with the system generate to create a metadata file.
#'
#' @return a dataframe.
#'
#' @export
#'

create_df_metadata = function(data, filename, study_name, study_country, additional_info, upload_time = Sys.time(), last_modified = upload_time, user="Admin", groups="admin") {
	file_metadata = list()
	file_metadata$user = user
	file_metadata$study_name = study_name
	file_metadata$study_country = paste0(study_country, collapse=", ")
	file_metadata$additional_info = additional_info
	file_metadata$file_name = filename
	file_metadata$size = c(object.size(data))
	file_metadata$observations = NROW(data)
	file_metadata$features = NCOL(data)
	file_metadata$upload_time = upload_time
	file_metadata$last_modified = last_modified
	file_metadata$groups = groups
	file_metadata = cbind.data.frame(file_metadata)
	return(file_metadata)
}

#' Collect log files 
#'
#' Create a data frame of log files based on a particular pattern.
#'
#' @details Collect all log files and create a dataframe.
#'
#' @param path path to the files.
#' @param pattern search pattern.
#'
#' @return dataframe object.
#'
#' @export

collect_logs = function(path, pattern="*.log$") {
	logs = list.files(path = path, pattern=pattern, full.names=TRUE)
	logs = sapply(logs, read.csv, simplify=FALSE)
	logs = do.call("rbind", logs)
	return(logs)
}

#' Get data class
#'
#' Assigns a class to the upload path or url based on the file extension.
#'
#' @param path data path or url.
#'
#' @return A character string specifying the class of the data.
#'
#' @export

get_data_class = function(path) {
	class(path) = get_file_ext(path)
	if (class(path) %in% c("sav", "por")) {
		class(path) = "spss"
	}
	return(path)
}

#' Upload csv data
#'
#' Uploads csv data
#'
#' @param path
#'
#' @return a dataframe.
#'
#' @importFrom readr read_csv
#'
#' @rdname upload_data
#'
#' @export

upload_data.csv = function(path) {
	df = readr::read_csv(path, show_col_types = FALSE)
	return(df)
}

#' Upload Excel data
#'
#' Uploads excel data
#'
#' @param path
#'
#' @return a dataframe.
#'
#' @importFrom openxlsx read.xlsx
#'
#' @rdname upload_data
#'
#' @export

upload_data.xlsx = function(path) {
	df = openxlsx::read.xlsx(path)
	return(df)
}


#' Upload Excel data
#'
#' Uploads excel data
#'
#' @param path
#'
#' @return a dataframe.
#'
#' @importFrom readxl read_xls
#'
#' @rdname upload_data
#'
#' @export

upload_data.xls = function(path) {
	df = readxl::read_xls(path)
	return(df)
}


#' Upload Stata data
#'
#' Uploads Stata data
#'
#' @param path
#'
#' @return a dataframe.
#'
#' @importFrom haven read_dta
#'
#' @rdname upload_data
#'
#' @export

upload_data.dta = function(path) {
	df = haven::read_dta(path)
	return(df)
}

#' Upload rda data
#'
#' Uploads rda data. The rda file should be a dataframe.
#'
#' @param path
#'
#' @return a dataframe.
#'
#' @rdname upload_data
#'
#' @export

upload_data.rda = function(path) {
	df = get(load(path))
	return(df)
}

#' Upload rds data
#'
#' Uploads rds data. The rda file should be a dataframe.
#'
#' @param path
#'
#' @return a dataframe.
#'
#' @rdname upload_data
#'
#' @export

upload_data.rds = function(path) {
	df = readRDS(path)
	return(df)
}


#' Upload Spss data
#'
#' Uploads .sav or .por data.
#'
#' @param path
#'
#' @return a dataframe.
#'
#' @importFrom haven read_spss
#'
#' @rdname upload_data
#'
#' @export

upload_data.spss = function(path) {
	df = haven::read_spss(path)
	return(df)
}


#' Write csv data
#'
#' Writes csv data
#'
#' @param df data frame
#'
#' @param path
#'
#' @return NULL.
#'
#' @importFrom readr write_csv
#'
#' @rdname write_data
#'
#' @export

write_data.csv = function(path, df) {
	readr::write_csv(df, file=path)
}


#' Write xlsx data
#'
#' Writes xlsx data
#'
#' @param df data frame
#'
#' @param path
#'
#' @return NULL.
#'
#' @importFrom openxlsx write.xlsx
#'
#' @rdname write_data
#'
#' @export

write_data.xlsx = function(path, df) {
	write.xlsx(df, file=path)
}


#' Write Stata data
#'
#' Writes Stata data
#'
#' @param df dataframe
#'
#' @param path
#'
#' @return NULL.
#'
#' @importFrom haven write_dta
#'
#' @rdname write_data
#'
#' @export

write_data.dta = function(path, df) {
	haven::write_dta(df, path=path)
}

#' Write rda data
#'
#' Writes rda data. The rda file should be a dataframe.
#'
#' @param df data frame.
#' @param path
#'
#' @return NULL.
#'
#' @rdname write_data
#'
#' @export

write_data.rda = function(path, df) {
	save(df, file=path)
}

#' Write rds data
#'
#' Writes rds dataframe.
#'
#' @param df data frame.
#' @param path
#'
#' @return NULL.
#'
#' @rdname write_data
#'
#' @export

write_data.rds = function(path, df) {
	saveRDS(df, file=path)
}


#' Write Spss data
#'
#' Writes .sav or .por data.
#'
#' @param df data frame.
#' @param path
#'
#' @return NULL.
#'
#' @importFrom haven write_spss
#'
#' @rdname write_data
#'
#' @export

write_data.spss = function(path, df) {
	write_sav(data=df, path=path)
}

#' Filter data
#'
#' Applies filter patterns to the data.
#'
#' @param df data frame.
#' @param pattern filter pattern.
#'
#' @importFrom dplyr filter
#' @importFrom rlang parse_expr
#'
#' @return data.frame object.
#'
#' @export
#'

filter_data = function(df, pattern) {
	df = (df
		|> dplyr::filter(eval(rlang::parse_expr(pattern)))
	)
	return(df)
}

#' Proportion of missing values
#'
#' @param df input data frame
#'
#' @return data.frame
#'
#' @importFrom dplyr group_by summarise_all arrange rename mutate
#'
#' @export
#'

missing_prop = function(df) {
	df = (df
		|> dplyr::ungroup()
		|> dplyr::summarise_all(~sum(is.na(.)|. %in% c("", " "))/n())
		|> tidyr::pivot_longer(everything())
		|> dplyr::arrange(desc(value))
		|> dplyr::rename("variable"="name", "missing"="value")
		|> dplyr::mutate(missing = scales::percent(missing))
		|> as.data.frame()
	)
	return(df)
}

#' Get variable type
#'
#' @param x vector.
#'
#' @return a character string specifying data type.
#'
#' @export
#'

get_type = function(x) {
	type = class(x)
	if (inherits(x, "factor")|is.factor(x) | type=="factor") {
		type = "factor"
	} else if (type=="character") {
		type = "character"
	} else {
		type = "numeric"
	}
	return(type)
}

#' Convert levels to NA
#'
#' @param x input vector.
#' @param ... levels to be converted.
#'
#' @return a vector.
#'
#' @export

na_codes <- function(x, ...) {
    x[x %in% c(...)] <- NA
    x
}

#' Convert factors to numeric
#'
#' @param x input vector.
#'
#' @return a vector of class numeric.
#'
#' @export

factor_numeric <- function(x) {
	sjlabelled::as_numeric(x)
}

#' Convert numeric to factors
#'
#' @param x input vector.
#'
#' @return a vector of class factor.
#'
#' @export

numeric_factor <- function(x) {
	if (isTRUE(!is.null(attr(x, "labels")))) {
		x = sjlabelled::to_label(x)
	} else {
		x = sjlabelled::as_factor(x)
	}
	return(x)
}

#' Convert numeric to character
#'
#' @param x input vector.
#'
#' @return a vector of class character.
#'
#' @export

numeric_character <- function(x) {
	if (isTRUE(!is.null(attr(x, "labels")))) {
		x = sjlabelled::to_label(x)
	} else {
		x = sjlabelled::as_character(x)
	}
	return(x)
}

#' Generate quick data summary
#'
#' Generate data summary statistics for all the variables.
#'
#' @param df data.frame object.
#'
#' @return summary statistics.
#'
#' @export
#'

generate_data_summary = function(df) {
	out = sapply(df, function(x) {
		if (is.numeric(x) | is.integer(x)) {
		  summary(x)
		} else {
		  out = as.data.frame(table(x, useNA = "always"))
		  row.names(out) = NULL
		  colnames(out) = c("Category", "Frequency")
		  out = dplyr::arrange(out, desc(Frequency))
		  out
		}
	}, simplify = FALSE)
	return(out)
}

#' Format data-time 
#'
#' Customize data-time format for file naming
#'
#' @param x date vector.
#'
#' @param format date-time formate to output.
#'
#' @export
#'

format_date_time = function(x, format="%d-%m-%Y %H:%M:%S") {
	return(format(x, format))
}

#' Get value labels
#'
#' Extract unique value labels 
#'
#' @param df data.frame object.
#'
#' @param var variable name.
#'
#' @return a vector.
#'
#' @export
#'

extract_value_labels = function(df, var) {
	x = (df
		|> select(one_of(var))
		|> droplevels()
		|> pull()
	)
	if (is.factor(x)) {
		l = levels(x)
	} else {
		l = unique(x)
	}
	return(l)
}

#' Recode value labels
#'
#' Recode value labels of a character or a factor variable.
#'
#' @param x a vector of the input values/variable.
#' @param old old labels/values to be recoded.
#' @param new new labels.
#'
#' @return a vector.
#'
#' @export
#'

recode_value_labels = function(x, old, new) {
	
	if (length(old) > 1) {
		new = unlist(strsplit(new, "\\,|\\;"))
		if (length(new) != length(old)) {
			new = rep(new, length(old))
		}
	}
	labs = as.character(old)
	names(labs) = as.character(new)
	x = forcats::fct_recode(as.factor(as.character(x)), !!!labs)
	x = forcats::fct_drop(x)
	return(x)

}

#' Create new labels for missing values
#'
#' Create new labels for missing values. For categorical values, assigns a factor or a character label, otherwise assigns a number.
#'
#' @param x vector input.
#' @param new_value new category or value.
#' @return a vector.
#'
#' @export
#'

relabel_missing_values = function(x, new_value) {
	if (get_type(x) == "numeric")	 {
		x = dplyr::recode(x, .default = NULL, .missing = new_value)
	} else {
		x = dplyr::recode_factor(x, .default = NULL, .missing = new_value)
	}
	return(x)
}

#' Get range
#'
#' Compute the range for numeric values.
#'
#' @param df a data.frame.
#' @param var variable of interest 
#'
#' @return vector of min and max.
#'
#' @export

get_range = function(df, var) {
	x = (df
		|> select(all_of(var))
		|> range(na.rm=TRUE)
	)
	return(c(x[[1]], x[[2]]))
}

#' Convert patterns to NA 
#'
#' Create NAs based on particular patterns.
#'
#' @param df input dataframe.
#' @param var input variable.
#'
#' @details if regex not specified, every text is replaced with pattern. Same as `stringr::str_replace_all`.
#'
#' @return a dataframe.
#'
#' @export
#'

na_if_pattern = function(df, var, pattern, replacement = NA_character_, apply_all=FALSE) {
	if (pattern=="") {
		pattern = "^$"
	}

	.ff = function(x, pattern, replacement) {
		if (any(grep(pattern, x))) {
			vartype = get_type(x)
			x = stringr::str_replace_all(x, pattern, replacement)
			if (vartype=="numeric") {
				x = factor_numeric(x)
			}
		}
		return(x)
	}
	
	if (apply_all) {
		df = (df
			|> dplyr::mutate_all(.ff, pattern, replacement)
		)
	} else {
		df = (df
			|> dplyr::mutate_at(var, .ff, pattern, replacement)
		)
	}
	return(df)
}

#' Convert patterns to NA for single variable
#'
#' Create NAs based on particular patterns.
#'
#' @param df input dataframe.
#' @param var input variable.
#'
#' @return a dataframe.
#'
#' @export
#'

na_if_category_single = function(df, var, labels, range=FALSE) {
	.ff = function(x, labels, range) {
		if (get_type(x) == "numeric") {
			labels = unlist(strsplit(labels, "\\,|\\;"))
			labels = factor_numeric(labels)
			if (range) {
				if (length(labels)==2) {
					x[x>=labels[[1]] & x<=labels[[2]]] = NA
				} else {
					x[x %in% labels] = NA
				}
			} else {
				x[x %in% labels] = NA
			}
		} else {
			x[x %in% labels] = NA_character_
		}
		return(x)
	}

	df = (df
		|> dplyr::mutate_at(var, .ff, labels, range)
	)
	return(df)
}

#' Convert patterns to NA for multiple variables
#'
#' Create NAs based on particular patterns.
#'
#' @param df input dataframe.
#'
#' @return a dataframe.
#'
#' @export
#'

na_if_category_multiple = function(df, numeric_labels, categorical_labels, range=FALSE) {
	.ff = function(x, labels, range) {
		if (get_type(x) == "numeric") {
			labels = unlist(strsplit(numeric_labels, "\\,|\\;"))
			labels = factor_numeric(labels)
			if (range) {
				if (length(labels)==2) {
					x[x>=labels[[1]] & x<=labels[[2]]] = NA
				} else {
					x[x %in% labels] = NA
				}
			} else {
				x[x %in% labels] = NA
			}
		} else {
			labels = unlist(strsplit(categorical_labels, "\\, |\\; |\\,|\\;"))
			x[x %in% labels] = NA_character_
		}
		return(x)
	}

	df = (df
		|> dplyr::mutate_all(.ff, labels, range)
	)

	return(df)
}

#' Convert factors/characters values to NA
#'
#' Create NAs bassed on particular value labels.
#'
#' @param df input dataframe.
#' @param var input variable.
#'
#'
#' @return a dataframe.
#'
#' @export

na_if_label = function(df, var, labels, apply_all=FALSE) {
	if (apply_all) {
		df = (df
			|> dplyr::mutate_if(is.factor, na_codes, labels)
			|> dplyr::mutate_if(is.character, na_codes, labels)
		)
	} else {
		df = (df
			|> dplyr::mutate_at(var, na_codes, labels)
		)
	}
	return(df)
}

#' Convert numeric values to NA
#'
#' Create NAs bassed on particular numeric values.
#'
#' @param df input dataframe.
#' @param var input variable.
#'
#'
#' @return a dataframe.
#'
#' @export

na_if_numeric = function(df, var, range, apply_all=FALSE) {
	.ff = function(x, range) {
		x[x>=range[[1]] & x<=range[[2]]] = NA
		return(x)
	}

	if (apply_all) {
		df = (df
			|> dplyr::mutate_if(is.integer, .ff, range)
			|> dplyr::mutate_if(is.integer, .ff, range)
		)
	} else {
		df = (df
			|> dplyr::mutate_at(var, .ff, range)
		)
	}
	return(df)
}

#' Assign missing values new values/categories
#'
#' @param df input dataframe.
#'
#' @return a vector.
#'
#' @export

na_to_values = function(df, var, na_pattern, new_category, new_numeric=NULL, apply_all=FALSE) {
	
	.ff = function(x, na_pattern, new_category, new_numeric){
		if (get_type(x) != "numeric") {
			x = sjlabelled::as_character(x)
		} else {
			if (!is.null(new_numeric)) {
				new_category = as.numeric(new_numeric)
			} else {
				new_category = as.numeric(new_category)
			}
		}
		if (any(na_pattern %in% c("", "Blank")) & any(grep("^$", x))) {
			x[x==""] = new_category
		} 

		if (any(na_pattern %in% "NA") & any(is.na(x))) {
			x[is.na(x)] = new_category
		} 

		if (any(na_pattern %in% "NaN") & any(is.nan(x))) {
			x[is.nan(x)] = new_category
		}
		return(x)
	}

	if (apply_all) {
		df = (df
			|> dplyr::mutate_all(.ff, na_pattern, new_category, new_numeric)
		)
	} else {
		df = (df
			|> dplyr::mutate_at(var, .ff, na_pattern, new_category, new_numeric)
		)
	}
	return(df)
}


#' Identify outliers
#'
#' Identify outlier values of a numeric variable.
#'
#' @details 
#' Uses `boxplot` function to identify outliers.
#'
#' @param x numeric variable 
#'
#' @return a vector of outlier values.
#'
#' @export

get_outliers = function(df, var) {
	x = (df
		|> dplyr::select(all_of(var))
		|> pull()
	)
	if (get_type(x)=="numeric") {
		x = graphics::boxplot(x, plot = FALSE)$out
		x = unique(x)
	} else {
		x = NULL
	}
	return(x)
}

#' Handle outliers
#'
#' @export

handle_outliers = function(df, var, outliers, action, new_values=NULL, fun=NULL) {

	.ff = function(x, outliers, new_values, fun) {
		if (!is.null(fun)) {
			fun = get(fun)
		}
		if (is.function(fun) & is.null(new_values)) {
			new_values = do.call(fun, list(x, na.rm=TRUE))
		}
		x[x %in% outliers] = new_values
		return(x)
	}
	if (isTRUE(action=="drop")) {
		df = (df
			|> filter_at(dplyr::vars(var), dplyr::any_vars(!. %in% outliers))
		)
	} else if (isTRUE(action=="correct")) {
		df = (df
			|> dplyr::mutate_at(var, .ff, outliers, new_values, fun)
		)
	}
	return(df)
}

#' Filter variables with missing values
#'
#' @export

filter_missing_values_df = function(df, column, var_column, var) {
	df = (df
		|> dplyr::mutate_at(column, function(x){
			x = readr::parse_number(x)
			return(x)
		})
		|> dplyr::rename_at(var_column, ~c("variable"))
		|> dplyr::filter_at(dplyr::vars(column), dplyr::any_vars(.>0 & variable %in% var))
	)
	return(df)
}

#' Drop NAs
#'
#' @details Drops NAs for a specific variable or entire dataset
#'
#' @param df data frame
#' @param variable. Default is NULL. A vector of variable name(s) in the df. If specified, drop NAs for a specific variable otherwise for all the variables in the df
#'
#' @importFrom tidyr drop_na
#'
#' @export

drop_missing_values = function(df, variable = NULL) {
	if (is.null(variable)) {
		df =(df
			|> tidyr::drop_na()
		) 
	} else {
		df = (df
			|> tidyr::drop_na(all_of(variable))
		)
	}
	return(df)
}

#' Add rows or columns
#'
#' @details Combine two datasets by row, i.e., append  or by columns
#'
#' @param df1 base dataset
#' @param df2 new dataset
#' @param id indicator variable
#' @param type either "row" or "column"
#'
#' @importFrom dplyr bind_rows 
#'
#' @export
#'

combine_data = function(df1, df2, type, id=NULL) {
	if (id=="") {
		id = NULL
	}
	common = intersect(colnames(df1), colnames(df2))
	if (length(common)>0) {
		types1 = sapply(df1[, common, drop=FALSE], get_type)	
		types2 = sapply(df2[, common, drop=FALSE], get_type)	
		if (!isTRUE(all.equal(types1, types2))) {
			as.types1 = paste0("as.", types1)
			names(as.types1) = names(types1)
			as.types2 = paste0("as.", types2)
			names(as.types2) = names(types2)
			df1[, names(as.types1)] = sapply(names(as.types1), function(x) {
				x = do.call(as.types1[x], list(df1[[x]]))
				x
			})
			df2[, names(as.types1)] = sapply(names(as.types1), function(x) {
				x = do.call(as.types1[x], list(df2[[x]]))
				x
			})
		}
	}
	if (type=="row") {
		df = dplyr::bind_rows(df1, df2, .id=id)
	} else {
		df = dplyr::left_join(df1, df2, by = common, suffix = c(".data1", ".data2"))	
	}
	add_rows = NROW(df2)
	add_cols = NCOL(df) - NCOL(df1)
	return(list(df=df, dim=c(add_rows, add_cols)))
}


combine_columns = function(df1, df2, by = NULL) {
	df = dplyr::left_join(df1, df2, by = by, suffix = c(".data1", ".data2"))	
	add_rows = NROW(df2)
	add_cols = NCOL(df) - NCOL(df1)
	return(list(df=df, dim=c(add_rows, add_cols)))
}


#' Rename variables
#'
#' Rename variables in a data frame
#'
#' @param df dataframe
#' @param old old variable name
#' @param new new variable name
#'
#' @importFrom dplyr rename_at
#'
#' @export

rename_vars = function(df, old, new) {
	df = (df
		|> dplyr::rename_at(old, ~c(new))
	)
	return(df)
}

#' Masking
#'
#' This fuction uses asterics to partially or wholly hide a section of variable
#' @param df is data frame
#' @param cols the selected column name
#'
#' @importFrom sdcMicro createSdcObj kAnon extractManipData
#' 
#' @export
#'
# --- MASKING ---
# Replace every character in character columns with "*"
apply_masking <- function(df, cols){
  df |>
    dplyr::mutate(across(
      dplyr::all_of(cols),
      ~ if (is.character(.x)) stringr::str_replace_all(.x, ".", "*") else .x
    ))
}
attr(apply_masking, "description") <- "Replace each character in specified columns with '*'"

#' Suppression
#'
#' @export

apply_suppression <- function(df, cols) {
  df |> dplyr::select(-dplyr::any_of(cols))
}
attr(apply_suppression, "description") <- "Drop the specified columns"

#' Bucketing
#'
#' @export
#'

apply_bucketing <- function(df, cols, bin_size) {
  df |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      ~ {
        max_val <- max(.x, na.rm = TRUE)
        breaks  <- seq(0, max_val + bin_size, by = bin_size)
        labels  <- paste0(head(breaks, -1), "-", breaks[-1] - 1)
        cut(.x, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
      }
    ))
}
attr(apply_bucketing, "description") <- "Bucket numeric columns into fixed-width intervals starting at 0"

#' Pseudomization 
#'
#' @export
#'

apply_pseudonymization <- function(df, cols) {
  df |>
    dplyr::mutate(across(
      dplyr::all_of(cols),
      ~ digest::digest(as.character(.x), algo = "sha256")
    ))
}
attr(apply_pseudonymization, "description") <- "Replace values with SHA-256 hash digests"

#' Tokenization
#'
#' @export
#'

apply_tokenization <- function(df, cols, seed = 123) {
  set.seed(seed)
  df |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      ~ replicate(dplyr::n(), paste0(sample(c(0:9, letters, LETTERS), 10, TRUE), collapse = ""))
    ))
}
attr(apply_tokenization, "description") <- "Replace values with random 10-character tokens"


#' Basic K-anonymity
#'
#' @export
#'

apply_k_anonymity <- function(df, cols, k) {
  df |>
	dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
	dplyr::mutate(.group_size = dplyr::n()) |>
	dplyr::ungroup() |>
	dplyr::filter(.group_size >= k) |>
	dplyr::select(-.group_size)
}
attr(apply_k_anonymity, "description") <- "Filter to only those groups whose size >= k"

#' Extended K-anonymity
#'
#' @export
#'

apply_k_extended <- function(df, qids, k, bucket_cols = list(), direct_ids = character()) {
  # 1) Suppress direct identifiers
  df_proc <- df |> dplyr::select(-dplyr::any_of(direct_ids))
  df_proc$row_id_temp <- seq_len(nrow(df_proc))
  
  # 2) Generalize numeric QIDs
  for (col in names(bucket_cols)) {
    df_proc[[col]] <- bucket_cols[[col]](df_proc[[col]])
  }
  
  # 3) sdcMicro k-anonymity
  qids2 <- intersect(qids, names(df_proc))
  sdcObj <- sdcMicro::createSdcObj(dat = df_proc, keyVars = qids2)
  sdcObj <- sdcMicro::kAnon(sdcObj, k = k)
  df_k   <- sdcMicro::extractManipData(sdcObj)
  
  # 4) Combine matched & unmatched
  matched   <- df_k |> dplyr::select(-row_id_temp)
  unmatched <- df_proc |> dplyr::filter(!row_id_temp %in% df_k$row_id_temp) |> dplyr::select(-row_id_temp)
  dplyr::bind_rows(matched, unmatched)
}
attr(apply_k_extended, "description") <- "Extended k-anonymity with bucketing & suppression"


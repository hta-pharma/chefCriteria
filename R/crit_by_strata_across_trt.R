#' Evaluate inclusion criterion for by-strata-across-treatment statistics based on the number of subjects with events.
#'
#' @description
#' Check if the number of subjects with events is at least 10 in one of the strata levels.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in `dat`.
#' @param subjectid_var character. Name of the subject identifier variable in `dat` (default is "USUBJID").
#' @param ... Additional arguments passed to the function.
#'
#' @return A Boolean value indicating whether the criterion is met.
#' @export
#'
# crit_ba_nsubev_01 <- function(dat, event_index, subjectid_var, ...) {
#
#   # Extract unique subjects with events
#   dt <- unique(dat[list(event_index)], by = c(subjectid_var)) |>
#     .[, .(bool = .N >= 10), by = c(strata_var)]
#
#   return(any(dt[["bool"]]))
# }

#' Evaluate inclusion criterion for by-strata-by-treatment statistics based on the number of subjects with events in the total population.
#'
#' @description
#' This function checks if the number of subjects with events in the total population is greater than or equal to a specified minimum.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param subjectid_var character. Name of the subject identifier variable in `dat` (default is "USUBJID").
#' @param n_subj_event_min The minimum number of subjects with events required in the total population.
#' @param ... Additional arguments passed to the function.
#'
#' @return A Boolean value indicating whether the criterion is met.
#' @export
#' @import data.table
crit_bb_nsubev_01 <- function(dat, event_index, subjectid_var, n_subj_event_min, ...){

  # Evaluate criterion
  crit_accept <- dat[J(event_index)] |>
    unique(by = c(subjectid_var)) |>
    nrow() >= n_subj_event_min

  return(crit_accept)
}


#' Evaluate inclusion criterion for by-strata-by-treatment statistics based on the p-value in the total population.
#'
#' @description This function checks if the p-value in the total population is below a specified maximum.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param cell_index A vector of integers referencing the rows of `dat` (as
#'   specified by the `INDEX_` column in `dat`) that match the population to be
#'   analyzed. See the "Endpoint Events" vignette in {ramnog}
#'   for more information.
#' @param treatment_var character. The name of the treatment variable in `dat`.
#' @param treatment_refval character. The reference value of the treatment variable in `dat`.
#' @param subjectid_var character. Name of the subject identifier variable in `dat` (default is "USUBJID").
#' @param pval_max The maximum p-value.
#' @param ... Additional arguments passed to the function.
#'
#' @return A Boolean value indicating whether the criterion is met.
#' @export
crit_bb_pval_01 <-
  function(dat,
           event_index,
           cell_index,
           treatment_var,
           treatment_refval,
           subjectid_var,
           pval_max,
           ...) {

    # Calculate p-value
    p_value <- chefStats::p_val(
      dat = dat,
      event_index = event_index,
      cell_index = dat[["INDEX_"]],
      treatment_var = treatment_var,
      treatment_refval = treatment_refval,
      subjectid_var = subjectid_var
    )[["value"]]

    return(p_value < pval_max)
  }


#' Evaluate inclusion criterion for by-strata-by-treatment statistics based on the number of subjects.
#'
#' @description This function checks if the number of subjects in each strata level is at least 10.
#'
#' @param dat data.table. The analysis data set.
#' @param strata_var character. Variable in `dat` to stratify by specific for this call.
#' @param subjectid_var character. Name of the subject identifier variable in `dat` (default is "USUBJID").
#' @param ... Additional arguments passed to the function.
#'
#' @return A Boolean value indicating whether the criterion is met.
#' @export
#'
# crit_bb_nsub_01 <- function(dat, strata_var, subjectid_var, ...) {
#
#
#   # Extract unique subjects per treatment arm
#   dt <- dat |> unique(by = c(subjectid_var))
#
#   # Evaluate criterion
#   dt2 <- dat |>
#     unique(by = c(subjectid_var)) |>
#     .[, .(bool = .N >= 10), by = c(strata_var)]
#
#   return(all(dt2[["bool"]]))
# }

#' Evaluate endpoint criterion based on number of subjects with events.
#'
#' @description
#' This function checks if the number of subjects with events in at least one study arm is greater than or equal to a specified minimum.
#'
#' @param dat data.table. The analysis data set.
#' @param event_index vector of integers that index the rows in `dat` that match
#'   the definition of an 'event'. Matching is done via the `INDEX_` column in
#'   `dat`.
#' @param subjectid_var character. Name of the subject identifier variable in `dat` (default is "USUBJID").
#' @param treatment_var The name of the variable in dat containing the treatment group assignments.
#' @param n_subj_event_min The minimum number of subjects with events required in at least one study arm.
#' @param ... Additional arguments passed to the function.
#'
#' @return A Boolean value indicating whether the criterion is met.
#' @import data.table
#' @export
crit_ep_nsubev_01 <- function(dat,
                              event_index,
                              subjectid_var,
                              treatment_var,
                              n_subj_event_min = 10,
                              ...) {
  stat <- dat[J(event_index)] |>
    unique(by = c(subjectid_var, treatment_var))

  return(any(table(stat[[treatment_var]]) >= n_subj_event_min))
}

#' Evaluate inclusion criteria for adverse events (AE) by system organ class (SOC) and preferred term (PT).
#'
#' @description
#' This function evaluates whether the specified inclusion criteria for AE by SOC and PT are met. The incidence thresholds for analysis of endpoints regarding AE by SOC and PT are as follows:
#'
#' 1. AE (regardless of severity): at least 10% in one of the study arms.
#' 2. Severe AE and serious AE: at least 5% in one of the study arms.
#' 3. Additional criteria for AE regardless of severity (all AE, non-severe AE, severe AE and serious AE): At least 10 subjects and at least 1% in one of the study arms.
#'
#' If one of the criteria is fulfilled, the respective SOC or PT must be presented.
#' @param dat data.table. The analysis data set.
#' @param treatment_var The name of the variable in dat containing the treatment group assignments.
#' @param subjectid_var character. Name of the subject identifier variable in `dat` (default is "USUBJID").
#' @param ... Additional arguments passed to the function.
#'
#' @return A Boolean value indicating whether the criterion are met.
#' @export
# crit_ep_complex01 <-
#   function(dat, treatment_var, subjectid_var, ...) {
#
#     # Identify if the group is PT or SOC
#     socpt_flg <-
#       intersect(c("AEBODSYS_flg", "AEDECOD_flg", "AESOC_flg"), names(dat))
#
#     # Number of subjects in each treatment arm
#     stats_subj <- dat |>
#       unique(by = c(subjectid_var, treatment_var)) |>
#       .[, .(N_sub = .N), by = c(treatment_var)]
#
#     # Number of subjects with events in each treatment arm
#     stats_subjev <- dat |>
#       unique(by = c(subjectid_var, treatment_var, event_index)) |>
#       .[, .(N_subev = sum(get(event_index))), by = c(treatment_var)]
#
#     # Number of subjects with events in each treatment arm disregarding severity
#     stats_subjev_disregard_sev <- dat |>
#       unique(by = c(subjectid_var, treatment_var, socpt_flg)) |>
#       .[, .(N_subev_disregard_sev = sum(get(socpt_flg))), by = c(treatment_var)]
#
#     # Percentage of subjects with events in each treatment arm
#     stats_all <-
#       merge(stats_subj,
#             stats_subjev,
#             by = c(treatment_var),
#             all = T) |>
#       merge(.,
#             stats_subjev_disregard_sev,
#             by = c(treatment_var),
#             all = T) |>
#       dplyr::mutate(P_subev = N_subev / N_sub,
#                     P_subev_disregard_sev = N_subev_disregard_sev / N_sub)
#
#     # Evaluate partial criteria:
#
#     # 1. AE (regardless of severity): at least 10% in one of the study arms
#     crit1 <- any(stats_all[["P_subev_disregard_sev"]] >= 0.1)
#
#     # 2. Severe AE and SAE: at least 5% in one of the study arms.
#     data_filtered <- dat[event_flg == 1,]
#     any_events <- nrow(data_filtered) > 0
#     severe_or_serious_ae <-
#       any_events &
#       (all(unique(data_filtered[["AESEV"]]) == "SEVERE") ||
#          all(unique(data_filtered[["AESER"]]) == "Y"))
#     crit2 <-
#       ifelse(severe_or_serious_ae, any(stats_all[["P_subev"]] >= 0.05), F)
#
#     # 3. Additional criteria for AE regardless of severity (All AE, Non-severe AE,
#     # Severe AE and SAE): at least 10 subjects AND at least 1% in one of the study arms.
#     crit3 <-
#       any(stats_all[["N_subev_disregard_sev"]] >= 10 &
#             stats_all[["P_subev_disregard_sev"]] >= 0.01)
#
#     criterion_evaluated <- (crit1 | crit2 | crit3)
#
#     return(criterion_evaluated)
#   }

# NEEDS TO BE REVISED

ep_base <-
  purrr::partial(
    chef::mk_endpoint_str,
    study_metadata = list(),
    pop_var = "SAFFL",
    pop_value = "Y",
    treatment_var = "TRT01A",
    treatment_refval = "Xanomeline High Dose",
    period_var = "ANL01FL",
    period_value = "Y"
  )

ep_waiting_grps <- purrr::partial(
  ep_base,
  stratify_by = list(c("SEX", "AGEGR2")),
  stat_by_strata_by_trt = list("n_subj" = n_sub,
                               "n_subev" = n_subev)
)

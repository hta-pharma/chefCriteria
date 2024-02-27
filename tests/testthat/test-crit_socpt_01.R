# NEEDS TO BE REVISED

test_that("crit_ep_01 with nsub_min=10 (default)",
{
  # SETUP -------------------------------------------------------------------

  skip_on_devops()
  ep <- rbind(
    ep_base(
      data_prepare = mk_adae,
      group_by = list(list(AESOC = c())),
      endpoint_label = "<AESOC>",
      crit_endpoint = list(c(crit_socpt_01))
    )
  )
  study_metadata <- list()
  ep <- add_id(ep)
  ep_fn_map <- suppressWarnings(unnest_endpoint_functions(ep))
  user_def_fn <- mk_userdef_fn_dt(ep_fn_map, env = environment())
  fn_map <- merge(ep_fn_map[, .(endpoint_spec_id, fn_hash)], user_def_fn,
                  by = "fn_hash")
  adam_db <-
    fetch_db_data(study_metadata = ep$study_metadata[[1]],
                  fn_dt = user_def_fn)
  ep_and_data <- filter_db_data(ep, ep_fn_map, adam_db)
  ep_data_key <- ep_and_data$ep
  analysis_data_container <- ep_and_data$analysis_data_container
  ep_expanded <- expand_over_endpoints(ep_data_key, analysis_data_container, fn_map)
  ep_ev_index <- add_event_index(ep_expanded, analysis_data_container)
  ep_with_data <- ep_ev_index[analysis_data_container]

  # ACT ---------------------------------------------------------------------

  ep_with_data[, crit_ep_accept := crit_socpt_01(
    dat = dat[[1]],
    endpoint_filter = endpoint_filter[[1]],
    endpoint_group_metadata = endpoint_group_metadata[[1]],
    event_index = event_index[[1]],
    period_value = period_value[[1]],
    period_var = period_var[[1]],
    stratify_by = stratify_by[[1]],
    treatment_refval = treatment_refval[[1]],
    treatment_var = treatment_var,
    subjectid_var = "USUBJID"
  ), by = 1:nrow(ep_with_data)]

  # EXPECT ------------------------------------------------------------------

  # Distribution of accepted vs. not accepted endpoints
  expect_equal(nrow(ep_with_data), 22)
  expect_equal(sum(ep_with_data$crit_ep_accept), 6)
  expect_equal(sum(!ep_with_data$crit_ep_accept), 16)

  # SOCs that are accepted
  expect_true(all(ep_with_data[ep_with_data$crit_ep_accept,][["endpoint_label"]]
               %in% c(
                 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                 "GASTROINTESTINAL DISORDERS",
                 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                 "INFECTIONS AND INFESTATIONS",
                 "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
                 "NERVOUS SYSTEM DISORDERS"
               )))
})

test_that("crit_ep_01 with nsub_min=5",
{
  # SETUP -------------------------------------------------------------------

  skip_on_devops()
  ep <- rbind(
    ep_base(
      data_prepare = mk_adae,
      group_by = list(list(AESOC = c())),
      endpoint_label = "<AESOC>",
      crit_endpoint = list(c(crit_socpt_01, nsub_min = 5))
    )
  )
  study_metadata <- list()
  ep <- add_id(ep)
  ep_fn_map <- suppressWarnings(unnest_endpoint_functions(ep))
  user_def_fn <- mk_userdef_fn_dt(ep_fn_map, env = environment())
  fn_map <- merge(ep_fn_map[, .(endpoint_spec_id, fn_hash)], user_def_fn,
                  by = "fn_hash")
  adam_db <-
    fetch_db_data(study_metadata = ep$study_metadata[[1]],
                  fn_dt = user_def_fn)
  ep_and_data <- filter_db_data(ep, ep_fn_map, adam_db)
  ep_data_key <- ep_and_data$ep
  analysis_data_container <- ep_and_data$analysis_data_container
  ep_expanded <- expand_over_endpoints(ep_data_key, analysis_data_container, fn_map)
  ep_ev_index <- add_event_index(ep_expanded, analysis_data_container)
  ep_with_data <- ep_ev_index[analysis_data_container]

  # ACT ---------------------------------------------------------------------

  ep_with_data[, crit_ep_accept := crit_socpt_01(
    dat = dat[[1]],
    endpoint_filter = endpoint_filter[[1]],
    endpoint_group_metadata = endpoint_group_metadata[[1]],
    event_index = event_index[[1]],
    period_value = period_value[[1]],
    period_var = period_var[[1]],
    stratify_by = stratify_by[[1]],
    treatment_refval = treatment_refval[[1]],
    treatment_var = treatment_var,
    subjectid_var = "USUBJID",
    nsub_min = 5
  ), by = 1:nrow(ep_with_data)]


  # EXPECT ------------------------------------------------------------------

  # Distribution of accepted vs. not accepted endpoints
  expect_equal(nrow(ep_with_data), 22)
  expect_equal(sum(ep_with_data$crit_ep_accept), 10)
  expect_equal(sum(!ep_with_data$crit_ep_accept), 12)

  # SOCs that are accepted
  expect_true(all(
    ep_with_data[ep_with_data$crit_ep_accept,][["endpoint_label"]]
    %in% c(
      "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
      "GASTROINTESTINAL DISORDERS",
      "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
      "INFECTIONS AND INFESTATIONS",
      "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
      "NERVOUS SYSTEM DISORDERS",
      "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
      "GASTROINTESTINAL DISORDERS",
      "INFECTIONS AND INFESTATIONS",
      "CARDIAC DISORDERS",
      "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
      "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
      "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",
      "PSYCHIATRIC DISORDERS",
      "NERVOUS SYSTEM DISORDERS" ,
      "INVESTIGATIONS"
    )))
})

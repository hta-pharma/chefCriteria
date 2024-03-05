test_that("Check crit_ep_nsubev_01", {
    # SETUP -------------------------------------------------------------------

    dat <- mk_adae()
    dat[["INDEX_"]] <- 1:nrow(dat)
    setkey(dat, INDEX_)
    event_index <- c(1:10, 50:100)
    treatment_var <- "TRT01A"
    treatment_refval <- "Xanomeline High Dose"
    subjectid_var <- "USUBJID"

    # ACT ---------------------------------------------------------------------

    crit_eval_1 <- crit_ep_nsubev_01(
        dat = dat,
        event_index = event_index,
        subjectid_var = subjectid_var,
        treatment_var = treatment_var,
        n_subj_event_min = 5
    )
    crit_eval_2 <- crit_ep_nsubev_01(
        dat = dat,
        event_index = event_index,
        subjectid_var = subjectid_var,
        treatment_var = treatment_var,
        n_subj_event_min = 10
    )

     # EXPECT ------------------------------------------------------------------

    expect_true(crit_eval_1)
    expect_false(crit_eval_2)

})

test_that("Check crit_bb_nsubev_01", {
    # SETUP -------------------------------------------------------------------

    dat <- data.table(
        INDEX_ = 1:10,
        USUBJID = 1:10
    )

    setkey(dat, INDEX_)
    event_index <- c(1, 3, 5, 7)
    n_subj_event_min <- 5
    subjectid_var <- "USUBJID"

    # ACT ---------------------------------------------------------------------

    crit_eval_1 <- crit_bb_nsubev_01(dat, event_index, subjectid_var, n_subj_event_min = 3)
    crit_eval_2 <- crit_bb_nsubev_01(dat, event_index, subjectid_var, n_subj_event_min = 5)

    # EXPECT ------------------------------------------------------------------

    expect_true(crit_eval_1)
    expect_false(crit_eval_2)
    expect_error(crit_bb_nsubev_01(dat, event_index, subjectid_var))
})

test_that("Check crit_bb_pval_01", {
    # SETUP -------------------------------------------------------------------

    dat <- mk_adae()
    dat[["INDEX_"]] <- 1:nrow(dat)
    event_index <- c(1:10, 50:100, 600:700)
    cell_index <- 500:700
    treatment_var <- "TRT01A"
    treatment_refval <- "Xanomeline High Dose"
    subjectid_var <- "USUBJID"

    pval_max_1 <- 0.87
    pval_max_2 <- 0.88

    # ACT ---------------------------------------------------------------------

   crit_eval_1 <- crit_bb_pval_01(
        dat = dat,
        event_index = event_index,
        cell_index = cell_index,
        treatment_var = treatment_var,
        treatment_refval = treatment_refval,
        subjectid_var = subjectid_var,
        pval_max = pval_max_1
    )

    crit_eval_2 <- crit_bb_pval_01(
        dat = dat,
        event_index = event_index,
        cell_index = cell_index,
        treatment_var = treatment_var,
        treatment_refval = treatment_refval,
        subjectid_var = subjectid_var,
        pval_max = pval_max_2
    )

    # EXPECT ------------------------------------------------------------------

    p_value <- chefStats::p_val(
      dat = dat,
      event_index = event_index,
      cell_index = dat[["INDEX_"]],
      treatment_var = treatment_var,
      treatment_refval = treatment_refval,
      subjectid_var = subjectid_var
    )[["value"]]

    expect_equal(crit_eval_1, p_value < pval_max_1)
    expect_equal(crit_eval_2, p_value < pval_max_2)

})

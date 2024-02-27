mk_adcm <- function(study_metadata){

  # Read ADSL and ADCM
  adcm <- data.table::as.data.table(pharmaverseadam::adcm)
  adsl <- data.table::as.data.table(pharmaverseadam::adsl)

  # Merge ADaM tables
  adcm_out <-
    merge(adsl, adcm[, c(setdiff(names(adcm), names(adsl)), "USUBJID"), with =
                       F], by = "USUBJID", all = TRUE)

  # Return ADaM data from two treatment arms
  return(adcm_out[adcm_out$TRT01A %in% c("Placebo", "Xanomeline High Dose"),])
}

mk_adae <- function(study_metadata){

  # Read ADSL and ADAE
  adae <- data.table::as.data.table(pharmaverseadam::adae)
  adsl <- data.table::as.data.table(pharmaverseadam::adsl)

  # Extract analysis flag from ADCM
  adcm <- data.table::as.data.table(pharmaverseadam::adcm)
  sub_ANL01FL <- unique(adcm[adcm$ANL01FL=="Y", c("USUBJID", "ANL01FL")])

  # Merge ADaM tables
  adae_out <-
    merge(adsl, sub_ANL01FL, by = "USUBJID", all.x = TRUE) %>%
    merge(., adae[, c(setdiff(names(adae), names(adsl)), "USUBJID"), with =
            F], by = "USUBJID", all = TRUE)

  # Return ADaM data from two treatment arms
  return(adae_out[adae_out$TRT01A %in% c("Placebo", "Xanomeline High Dose"),])
}

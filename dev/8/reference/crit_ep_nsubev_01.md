# Evaluate endpoint criterion based on number of subjects with events.

This function checks if the number of subjects with events in at least
one study arm is greater than or equal to a specified minimum.

## Usage

``` r
crit_ep_nsubev_01(
  dat,
  event_index,
  subjectid_var,
  treatment_var,
  n_subj_event_min,
  ...
)
```

## Arguments

- dat:

  data.table. The analysis data set.

- event_index:

  vector of integers that index the rows in `dat` that match the
  definition of an 'event'. Matching is done via the `INDEX_` column in
  `dat`.

- subjectid_var:

  character. Name of the subject identifier variable in `dat` (default
  is "USUBJID").

- treatment_var:

  The name of the variable in dat containing the treatment group
  assignments.

- n_subj_event_min:

  The minimum number of subjects with events required in at least one
  study arm.

- ...:

  Additional arguments passed to the function.

## Value

A Boolean value indicating whether the criterion is met.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3", "S4"),
  TRT     = c("Active", "Active", "Placebo", "Placebo")
)
dat[, INDEX_ := .I]
#>    USUBJID     TRT INDEX_
#>     <char>  <char>  <int>
#> 1:      S1  Active      1
#> 2:      S2  Active      2
#> 3:      S3 Placebo      3
#> 4:      S4 Placebo      4
data.table::setkey(dat, INDEX_)
# Returns TRUE: Active arm has 2 subjects with events >= minimum of 2
crit_ep_nsubev_01(dat, event_index = c(1L, 2L),
                  subjectid_var = "USUBJID",
                  treatment_var = "TRT", n_subj_event_min = 2L)
#> [1] TRUE
```

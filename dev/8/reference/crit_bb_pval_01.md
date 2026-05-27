# Evaluate inclusion criterion for by-strata-by-treatment statistics based on the p-value in the total population.

This function checks if the p-value in the total population is below a
specified maximum.

## Usage

``` r
crit_bb_pval_01(
  dat,
  event_index,
  cell_index,
  treatment_var,
  treatment_refval,
  subjectid_var,
  pval_max,
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

- cell_index:

  A vector of integers referencing the rows of `dat` (as specified by
  the `INDEX_` column in `dat`) that match the population to be
  analyzed. See the "Endpoint Events" vignette in ramnog for more
  information.

- treatment_var:

  character. The name of the treatment variable in `dat`.

- treatment_refval:

  character. The reference value of the treatment variable in `dat`.

- subjectid_var:

  character. Name of the subject identifier variable in `dat` (default
  is "USUBJID").

- pval_max:

  The maximum p-value.

- ...:

  Additional arguments passed to the function.

## Value

A Boolean value indicating whether the criterion is met.

## Examples

``` r
dat <- data.table::data.table(
  USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6"),
  TRT     = c("Active", "Active", "Active", "Placebo", "Placebo", "Placebo")
)
dat[, INDEX_ := .I]
#>    USUBJID     TRT INDEX_
#>     <char>  <char>  <int>
#> 1:      S1  Active      1
#> 2:      S2  Active      2
#> 3:      S3  Active      3
#> 4:      S4 Placebo      4
#> 5:      S5 Placebo      5
#> 6:      S6 Placebo      6
data.table::setkey(dat, INDEX_)
crit_bb_pval_01(dat, event_index = c(1L, 2L, 4L),
                cell_index = dat[["INDEX_"]],
                treatment_var = "TRT", treatment_refval = "Placebo",
                subjectid_var = "USUBJID", pval_max = 0.05)
#> ----- Calculating Barnards test - this may take a minute 
#> [1] FALSE
```

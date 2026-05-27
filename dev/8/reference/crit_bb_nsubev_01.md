# Evaluate inclusion criterion for by-strata-by-treatment statistics based on the number of subjects with events in the total population.

This function checks if the number of subjects with events in the total
population is greater than or equal to a specified minimum.

## Usage

``` r
crit_bb_nsubev_01(dat, event_index, subjectid_var, n_subj_event_min, ...)
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

- n_subj_event_min:

  The minimum number of subjects with events required in the total
  population.

- ...:

  Additional arguments passed to the function.

## Value

A Boolean value indicating whether the criterion is met.

## Examples

``` r
dat <- data.table::data.table(USUBJID = c("S1", "S2", "S3", "S4"))
dat[, INDEX_ := .I]
#>    USUBJID INDEX_
#>     <char>  <int>
#> 1:      S1      1
#> 2:      S2      2
#> 3:      S3      3
#> 4:      S4      4
data.table::setkey(dat, INDEX_)
# Returns TRUE: 2 subjects with events >= minimum of 2
crit_bb_nsubev_01(dat, event_index = c(1L, 2L),
                  subjectid_var = "USUBJID", n_subj_event_min = 2L)
#> [1] TRUE
# Returns FALSE: 1 subject with events < minimum of 2
crit_bb_nsubev_01(dat, event_index = c(1L),
                  subjectid_var = "USUBJID", n_subj_event_min = 2L)
#> [1] FALSE
```

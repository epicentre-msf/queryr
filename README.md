
<!-- README.md is generated from README.Rmd. Please edit that file -->

# queryr: Data validation queries with tidy, stackable output

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/epicentre-msf/queryr.svg?branch=master)](https://travis-ci.com/epicentre-msf/queryr)
[![Codecov test
coverage](https://codecov.io/gh/epicentre-msf/queryr/branch/master/graph/badge.svg)](https://codecov.io/gh/epicentre-msf/queryr?branch=master)
<!-- badges: end -->

### Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/queryr")
```

### Preliminaries

Load package and examine example dataset `ll`, an epidemiological
“linelist”.

``` r
library(queryr)
data(ll)
head(ll)
#>      id site    age     status date_onset date_admit   date_lab lab_result  date_exit   outcome
#> 1 TC143    A 14.000 Not a case 2020-03-30 2020-04-01 2020-04-01   Negative 2024-04-02 Sent home
#> 2 TC345    B  0.667  Suspected 2024-04-03 2020-04-05       <NA>   Positive 2020-04-19      <NA>
#> 3  T104    A 29.000  Suspected 2020-03-16 2020-04-03 2020-03-12       <NA> 2020-03-11      <NA>
#> 4 TC623    C 91.000  Confirmed 2020-04-10 2020-04-12 2020-03-12    Unclear 2020-04-30     Cured
#> 5 TC685    C  0.192 Not a case       <NA>       <NA> 2020-03-25   Negative 2010-03-26 Sent home
#> 6 TC361    B 68.000  Confirmed 2020-03-20 2020-03-25 2020-03-26   Positive       <NA>   Unknown
```

### Example queries

**E.g. 1.** Find observations where `date_exit` is earlier than
`date_admit`.

``` r
query(ll, date_exit < date_admit, cols_base = id:site)
#>      id site variable1     value1  variable2     value2
#> 1  T104    A date_exit 2020-03-11 date_admit 2020-04-03
#> 2 TC550    B date_exit 2010-05-13 date_admit 2020-04-30
#> 3 TC457    A date_exit 2020-03-19 date_admit 2020-04-10
```

Note that by default the columns referenced in the query expression are
pivoted to long-format (`variable1`, `value1`, `variable2`, `value2`,
…), to enable stacking multiple queries on different variables. The
optional `cols_base` argument can be used to specify additional columns
to retain in the output (via
[tidy-selection](https://tidyselect.r-lib.org/reference/select_helpers.html)).

**E.g. 2.** Find any date value in the future using a `.x` selector
within the query expression to represent a set of multiple columns. The
columns represented by `.x` are specified separately with argument
`cols_dotx`.

``` r
query(ll, .x > Sys.Date(), cols_dotx = starts_with("date"), cols_base = id:site)
#>      id site  variable1     value1
#> 1 TC345    B date_onset 2024-04-03
#> 2 TC457    A   date_lab 2040-04-12
#> 3 TC143    A  date_exit 2024-04-02
#> 4 TC443    C  date_exit 2030-04-05
```

**E.g. 3.** Find non-valid values of `lab_result`.

``` r
lab_result_valid <- c("Positive", "Negative", "Inc.", NA)
query(ll, !lab_result %in% lab_result_valid, cols_base = id:site)
#>      id site  variable1  value1
#> 1 TC623    C lab_result Unclear
```

**E.g. 4.** Stack multiple queries on different variables.

Because we’ll usually want to retain the same set of ‘base columns’
across multiple queries, the argument `cols_base` can be set for an
entire session using option “queryr\_cols\_base”.

``` r
# set argument `cols_base` for all subsequent queries (wrap in quote() if using
# non-standard evaluation rather than a character vector)
options(queryr_cols_base = quote(id:site))

# stack multiple queries
dplyr::bind_rows(
  .id = "query",
  # non-valid patient id
  "IDENT_01" = query(ll, !grepl("^TC\\d{3}", id)),
  # date in future
  "DATES_01" = query(ll, .x > Sys.Date(), cols_dotx = starts_with("date")),
  # exit before admission
  "DATES_02" = query(ll, date_exit < date_admit),
  # non-valid value of age_unit
  "FCTRS_01" = query(ll, !lab_result %in% lab_result_valid),
  # status 'Confirmed' but lab result not 'Positive'
  "LOGIC_01" = query(ll, status == "Confirmed" & !lab_result %in% "Positive")
)
#>       query    id site  variable1     value1  variable2     value2
#> 1  IDENT_01  T104    A         id       T104       <NA>       <NA>
#> 2  IDENT_01   190    A         id        190       <NA>       <NA>
#> 3  DATES_01 TC345    B date_onset 2024-04-03       <NA>       <NA>
#> 4  DATES_01 TC457    A   date_lab 2040-04-12       <NA>       <NA>
#> 5  DATES_01 TC143    A  date_exit 2024-04-02       <NA>       <NA>
#> 6  DATES_01 TC443    C  date_exit 2030-04-05       <NA>       <NA>
#> 7  DATES_02  T104    A  date_exit 2020-03-11 date_admit 2020-04-03
#> 8  DATES_02 TC550    B  date_exit 2010-05-13 date_admit 2020-04-30
#> 9  DATES_02 TC457    A  date_exit 2020-03-19 date_admit 2020-04-10
#> 10 FCTRS_01 TC623    C lab_result    Unclear       <NA>       <NA>
#> 11 LOGIC_01 TC623    C     status  Confirmed lab_result    Unclear
```

### Queries relating to two data frames

For these we’ll introduce a second example dataset `sll`, a ‘summary’
linelist. Whereas the original linelist `ll` contains all patients
presenting at treatment centres, the summary linelist `sll` contains
only patients with status “Confirmed” or “Probable”.

**E.g. 1.** Find treatment centre IDs that appear in `ll` but not `sll`,
and the corresponding patient status.

``` r
query2(
  ll,
  sll,
  cols_base1 = c(id, site, status),
  join_type = "anti",
  join_by = c("id" = "tc_id")
)
#>      id site     status
#> 1 TC143    A Not a case
#> 2  T104    A  Suspected
#> 3 TC685    C Not a case
#> 4   190    A Not a case
#> 5 TC443    C       <NA>
#> 6 TC206    A Not a case
```

**E.g. 2.** Find confirmed/probable patients in `ll` with different
outcomes listed in `ll` vs `sll`.

``` r
query2(
  ll,
  sll,
  cond1 = status %in% c("Confirmed", "Probable"),
  cols_base1 = id:site,
  join_type = "inner",
  join_by = c("id" = "tc_id"),
  cond3 = outcome != sll_outcome
)
#>      id site variable1    value1 variable2  value2   variable3 value3
#> 1 TC361    B    status Confirmed   outcome Unknown sll_outcome  Cured
#> 2 TC550    B    status  Probable   outcome   Other sll_outcome   Died
```

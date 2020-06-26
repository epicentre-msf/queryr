
<!-- README.md is generated from README.Rmd. Please edit that file -->

# queryr: Data validation queries with tidy, stackable output

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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
#>     id site age age_unit     status date_onset date_admit   date_lab lab_result  date_exit   outcome
#> 1 M143    A  14    Years Not a case 2020-03-30 2020-04-01 2020-04-01   Negative 2024-04-02 Sent home
#> 2 M932    B  61    Years  Confirmed 2020-04-01 2020-04-02 2020-04-01   Positive 2020-04-24      Died
#> 3 M345    B   8   Months  Suspected 2024-04-03 2020-04-05       <NA>   Positive 2020-04-19      <NA>
#> 4 N104    A  29    Years  Suspected 2020-03-16 2020-04-03 2020-03-12       <NA> 2020-03-11      <NA>
#> 5 M623    C  91      Yrs  Confirmed 2020-04-10 2020-04-12 2020-03-12       Inc. 2020-04-30     Cured
#> 6 M685    C  10    Weeks Not a case       <NA>       <NA> 2020-03-25   Negative 2010-03-26 Sent home
```

### Example queries

**E.g. 1.** Find observations where `date_exit` is earlier than
`date_admit`.

``` r
query(ll, date_exit < date_admit, cols_base = id:site)
#>     id site variable1     value1  variable2     value2
#> 1 N104    A date_exit 2020-03-11 date_admit 2020-04-03
#> 2 M550    B date_exit 2010-05-13 date_admit 2020-04-30
#> 3 M457    A date_exit 2020-03-19 date_admit 2020-04-10
```

Note that by default the columns referenced in the query expression are
pivoted to long-format (`variable1`, `value1`, `variable2`, `value2`,
…), to enable stacking multiple queries on different variables. The
optional `cols_base` argument can be used to specify additional columns
to retain in the output (via
[tidy-selection](https://tidyselect.r-lib.org/reference/select_helpers.html)).

**E.g. 2.** Find any date value in the future using a `.x` selector
within the query expression to represent a set of multiple columns. The
columns represented by `.x` are specified separately via tidy-selection
with argument
`cols_dotx`.

``` r
query(ll, .x > Sys.Date(), cols_dotx = starts_with("date"), cols_base = id:site)
#>     id site  variable1     value1
#> 1 M345    B date_onset 2024-04-03
#> 2 M457    A   date_lab 2040-04-12
#> 3 M143    A  date_exit 2024-04-02
#> 4 M443    C  date_exit 2030-04-05
```

**E.g. 3.** Find non-valid values of `age_unit`.

``` r
age_unit_valid <- c("Years", "Months", "Weeks", "Days")
query(ll, !age_unit %in% age_unit_valid, cols_base = id:site)
#>     id site variable1 value1
#> 1 M623    C  age_unit    Yrs
#> 2 M550    B  age_unit   <NA>
```

**E.g. 4.** Stack multiple queries on different variables.

Because we’ll usually want to retain the same set of ‘base columns’
across multiple queries, the argument `cols_base` can be set for an
entire session using option “queryr\_cols\_base”.

``` r
# set argument `cols_base` for all subsequent queries
options(queryr_cols_base = quote(id:site))

# stack multiple queries
dplyr::bind_rows(
  # ID_01: non-valid patient id
  query(ll, !grepl("^M[0-9]{3}", id), qval = "ID_01"),
  # DATE_01: date in future
  query(ll, .x > Sys.Date(), cols_dotx = starts_with("date"), qval = "DATE_01"),
  # DATE_02: exit before admission
  query(ll, date_exit < date_admit, qval = "DATE_02"),
  # CATEGOR_01: non-valid value of age_unit
  query(ll, !age_unit %in% age_unit_valid, qval = "CATEGOR_01"),
  # LOGIC_01: status 'Confirmed' but lab result not 'Positive'
  query(ll, status == "Confirmed" & !lab_result %in% "Positive", qval = "LOGIC_01")
)
#>         query   id site  variable1     value1  variable2     value2
#> 1       ID_01 N104    A         id       N104       <NA>       <NA>
#> 2       ID_01  190    A         id        190       <NA>       <NA>
#> 3     DATE_01 M345    B date_onset 2024-04-03       <NA>       <NA>
#> 4     DATE_01 M457    A   date_lab 2040-04-12       <NA>       <NA>
#> 5     DATE_01 M143    A  date_exit 2024-04-02       <NA>       <NA>
#> 6     DATE_01 M443    C  date_exit 2030-04-05       <NA>       <NA>
#> 7     DATE_02 N104    A  date_exit 2020-03-11 date_admit 2020-04-03
#> 8     DATE_02 M550    B  date_exit 2010-05-13 date_admit 2020-04-30
#> 9     DATE_02 M457    A  date_exit 2020-03-19 date_admit 2020-04-10
#> 10 CATEGOR_01 M623    C   age_unit        Yrs       <NA>       <NA>
#> 11 CATEGOR_01 M550    B   age_unit       <NA>       <NA>       <NA>
#> 12   LOGIC_01 M623    C     status  Confirmed lab_result       Inc.
```

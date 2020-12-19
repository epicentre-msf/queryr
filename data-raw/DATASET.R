## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(readxl)

is_posix <- function(x) {
  # test whether object 'x' is of class POSIXt/POSIXct/POSIXlt
  any(class(x) %in% c("POSIXt", "POSIXct", "POSIXlt"))
}

ll <- readxl::read_xlsx("data-raw/ll_example.xlsx") %>%
  mutate_if(is_posix, as.Date) %>%
  as.data.frame()

sll <- readxl::read_xlsx("data-raw/sll_example.xlsx") %>%
  mutate_if(is_posix, as.Date) %>%
  as.data.frame()

ll_queries <- data.frame(
  query_id = c(
    "ID_01",
    "DATES_01",
    "DATES_02",
    "FACTORS_01",
    "LOGIC_01"
    ),
  query = c(
    "!grepl(\"^TC[[:digit:]]{3}\", id)",
    "date_onset > Sys.Date() | date_admit > Sys.Date() | date_exit > Sys.Date()",
    "date_exit < date_admit",
    "!lab_result %in% c(\"Positive\", \"Negative\", \"Inc.\", NA)",
    "status == \"Confirmed\" & !lab_result %in% \"Positive\""
  ),
  stringsAsFactors = FALSE
)


usethis::use_data(ll, sll, ll_queries, overwrite = TRUE)


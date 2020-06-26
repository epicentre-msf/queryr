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


usethis::use_data(ll, overwrite = TRUE)

rm(list = ls())
source("_functions.R")
f_install_and_load(c("lubridate", "testthat", "zoo", "forcats", "tidyverse"))

# ---- Load ----

d_train_raw <- read_csv("../data_raw/train.csv", col_types = cols(StateHoliday = col_factor(levels = c("0", "a", "b", "c"))))
d_validate_raw <- read_csv("../data_raw/test.csv", col_types = cols(StateHoliday = col_factor(levels = c("0", "a", "b", "c"))))
d_store <- read_csv("../data_raw/store.csv")

# ---- Clean ----

summary(d_validate_raw$Date)

d_train <- d_train_raw %>% filter(Open != 0) %>%
  arrange(Store, Date) %>%
  group_by(Store) %>%
  mutate(Sales.l1 = dplyr::lag(Sales, n = 1),
         Sales.7days = rollapply(Sales.l1, FUN = mean, width = 7,
                                 align = "right", fill = NA, na.rm = T),
         Sales.30days = rollapply(Sales.l1, FUN = mean, width = 30,
                                  align = "right", fill = NA, na.rm = T)) %>%
  inner_join(select(d_store, Store, StoreType, CompetitionDistance), by = "Store") %>%
  mutate(StoreType = factor(StoreType)) %>%
  ungroup()

d_train_final <- d_train %>%
  filter(Date < "2014-08-01") %>%
  select(-Date) %>%
  write_rds("../data_clean/train_clean.RData")


d_validate_final <- d_train %>%
  filter(Date >= "2014-08-01" & Date <= "2014-09-30") %>%
  select(-Date) %>%
  write_rds("../data_clean/validate_clean.RData")

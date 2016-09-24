rm(list = ls())
source("_functions.R")
f_install_and_load(c("tidyverse", "lubridate"))

# ---- Load ----

train <- read_csv("../data_raw/train.csv", col_types = cols(StateHoliday = col_factor(levels = c("0", "a", "b", "c"))))
store <- read_csv("../data_raw/store.csv")
test <- read_csv("../data_raw/test.csv", col_types = cols(StateHoliday = col_factor(levels = c("0", "a", "b", "c"))))

pred <- read_csv("../data_raw/sample_submission.csv")

# ---- Visualize ----

table(year(train$Date))

train %>% summarise(n_distinct(Store))

yearmonth

train <- train %>%
  mutate(YearMonth = floor_date(Date, unit = "month"),
         Month = month(Date))


ggplot(train %>% group_by(YearMonth) %>% summarise(mean = mean(Sales), median = median(Sales))) +
  geom_line(aes(YearMonth, mean), col = "red") +
  geom_line(aes(YearMonth, median), col = "blue") +
  scale_x_date(labels = scales::date_format("%y-%m"), breaks = scales::date_breaks(width = "1 month"))

mean(train$Sales)
median(train$Sales)
hist(train$Sales)

train %>% group_by(Store) %>% summarise(n = n()) %>% group_by(n) %>% summarise(n2 = n())

# ---- Baseline ----

train %>%
  group_by(YearMonth) %>% summarise(mean = mean(Sales), median = median(Sales))

d_storemonth_avg <- train %>%
  group_by(Store, Month) %>% summarise(mean = mean(Sales), median = median(Sales))

d_storeweek_avg <- train %>%
  group_by


pred_storemonth_avg <- test %>%
  mutate(Month = month(Date)) %>%
  inner_join(d_storemonth_avg, by = c("Store", "Month")) %>%
  select(Id, Sales = median) %>%
  write_csv("../prediction/avg_storemonth.csv")



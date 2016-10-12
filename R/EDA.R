rm(list = ls())
source("_functions.R")
f_install_and_load(c("tidyverse", "lubridate", "testthat"))

# ---- Load ----

train <- read_csv("../data_raw/train.csv", col_types = cols(StateHoliday = col_factor(levels = c("0", "a", "b", "c"))))
store <- read_csv("../data_raw/store.csv")
test <- read_csv("../data_raw/test.csv", col_types = cols(StateHoliday = col_factor(levels = c("0", "a", "b", "c"))))

pred <- read_csv("../data_raw/sample_submission.csv")

train <- train %>%
  mutate(YearMonth = floor_date(Date, unit = "month"),
         Month = month(Date))

test <- test %>%
  mutate(YearMonth = floor_date(Date, unit = "month"),
         Month = month(Date))
# ---- Summarize ----

summary(train$YearMonth)
summary(test$YearMonth)

table(year(train$Date))
train %>% summarise(n_distinct(Store))

# All closed stores have 0 sales
expect_equal(all(train[train$Open == 0, "Sales"] == 0), TRUE)

# ---- Visualize ----

# Median and Average sales by month
ggplot(train %>% group_by(YearMonth) %>% summarise(mean = mean(Sales), median = median(Sales))) +
  geom_line(aes(YearMonth, mean), col = "red") +
  geom_line(aes(YearMonth, median), col = "blue") +
  scale_x_date(labels = scales::date_format("%y-%m"), breaks = scales::date_breaks(width = "1 month")) +
  theme(axis.text.x=element_text(angle=90, hjust=0))

mean(train$Sales)
median(train$Sales)
hist(train$Sales)

train %>% group_by(Store) %>% summarise(n = n()) %>% group_by(n) %>% summarise(n2 = n())

# ---- Baseline ----

d_storemonth_avg <- train %>%
  group_by(Store, Month) %>% summarise(mean = mean(Sales), median = median(Sales))
d_storemonth_avg

# Prediction = the average sales of that store in that day of the week
d_storeday_avg <- train %>% filter(Open == 1) %>%
  group_by(DayOfWeek, Store) %>%
  summarise(mean = mean(Sales))

pred_storeday_avg <- test %>%
  left_join(d_storeday_avg, by = c("Store", "DayOfWeek")) %>%
  mutate(Open = ifelse(Store == 622 & DayOfWeek != 7, 1, Open)) %>% # Store 622 has Open = NA for some reasons
  mutate(mean = ifelse(Open == 0, 0, mean)) %>%
  select(Id, Sales = mean) %>%
  write_csv("../prediction/avg_storeday.csv")


test %>% anti_join(d_storeday_avg, by = c("Store", "DayOfWeek")) %>% filter(DayOfWeek != 7)

# Prediction = the average sales of that store in that month of the year
pred_storemonth_avg <- test %>%
  mutate(Month = month(Date)) %>%
  inner_join(d_storemonth_avg, by = c("Store", "Month")) %>%
  select(Id, Sales = median) %>%
  write_csv("../prediction/avg_storemonth.csv")



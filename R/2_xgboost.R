rm(list = ls())
source("_functions.R")
f_install_and_load(c("xgboost", "tidyverse"))

# ---- Load data ----

d_train <- read_rds("../data_clean/train_clean.RData") %>%
  filter(Sales != 0)
d_validate <- read_rds("../data_clean/validate_clean.RData") %>%
  filter(Sales != 0)

mat_train_label <- na.omit(d_train)$Sales
mat_train <- xgb.DMatrix(data = data.matrix(na.omit(d_train) %>% select(-Sales)),
                         label = mat_train_label)

m1 <- xgboost(data = mat_train,
              nround = 4,
              params = list(objective = "reg:linear",
                            eta = 0.05, max.depth = 10,
                            nthread = parallel::detectCores()))

mat_validate_data <- data.matrix(na.omit(d_validate) %>% select(-Sales))
mat_validate_label <- na.omit(d_validate)$Sales

pred1_in <- predict(m1, mat_train)
pred1 <- predict(m1, mat_validate_data)

RMSPE(mat_train_label, pred1_in)
RMSPE(mat_validate_label, pred1)

# ---- Create prediction ----

rm(list = ls())
source("_functions.R")
f_install_and_load(c("xgboost", "tidyverse"))

# ---- Load data ----

d_train <- read_rds("../data_clean/train_clean.RData")

class(d_train)

d_train %>%
  map(function(x) sum(is.na(x)))



which(is.na(tmp))

data <- xgb.DMatrix(data.matrix(na.omit(d_train) %>% select(-Sales)))
label <- data.matrix(na.omit(d_train) %>% select(Sales))

m1 <- xgboost(data = data, label = label,
              nround = 4,
              params = list(objective = "binary:logistic",
                            max.depth = 10,
                            nthread = 4))

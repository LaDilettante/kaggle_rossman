# Install and load packages
f_install_and_load <- function(packs) {
  new.packs <- packs[!(packs %in% installed.packages()[ ,"Package"])]
  lapply(new.packs, install.packages, repos="http://cran.rstudio.com/", dependencies=TRUE)
  lapply(packs, library, character.only=TRUE)
}

# Evaluation criterion Root Mean Square Percentage Error
RMSPE <- function(true, pred) {
  sqrt(mean( ( (true - pred) / true) ** 2 ))
}
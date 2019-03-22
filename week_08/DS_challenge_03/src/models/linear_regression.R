#  ####################################################################### 
#       File-Name:      linear_regression.R
#       Version:        R 3.5.2
#       Date:           March 3, 2019
#       Author:         MM <marco.morales@columbia.edu>
#       Purpose:        linear regressions to compare imputation model
#                       estimates
#       Input Files:    see below (from gr5069-ds-challenge S3 bucket)
#       Output Files:   NONE
#       Data Output:    NONE
#       Dependencies:   NONE
#       Required by:    NONE
#       Status:         COMPLETED
#       Machine:        Mac laptop
#  #######################################################################

library(dotwhisker)
library(ggplot2)


# ::::::::::::::::: INITIAL DEFINITIONS :::::::::::::::::::::::::::::::::::::::

data_1 <- "https://s3.amazonaws.com/gr5069-ds-challenge-03/datset_one.csv"
data_2 <- "https://s3.amazonaws.com/gr5069-ds-challenge-03/datset_two.csv"
data_3 <- "https://s3.amazonaws.com/gr5069-ds-challenge-03/datset_three.csv"

# ::::::::::::::::: LOAD DATA :::::::::::::::::::::::::::::::::::::::::::::::::

dataset_1 <- read_csv(data_1)
dataset_2 <- read_csv(data_2)
dataset_3 <- read_csv(data_3)

# ::::::::::::::::: FIT MODELS ::::::::::::::::::::::::::::::::::::::::::::::::

# fit model on dataset 1
summary(
  fit_ols_1 <- lm(
    Income ~ Age + Education + Gender +
      Question_1 + Question_2 + Question_3 + Question_4 + Question_5 +     
      Item_1 + Item_2 + Item_3 + Item_4 + Item_5 + Item_6 + Item_7,
    data = dataset_1
    )
)

# fit model on dataset 2
summary(
fit_ols_2 <- lm(
  Income ~ Age + Education + Gender +
    Question_1 + Question_2 + Question_3 + Question_4 + Question_5 +     
    Item_1 + Item_2 + Item_3 + Item_4 + Item_5 + Item_6 + Item_7,
  data = dataset_2
  )
)

# fit model on dataset 3
summary(
  fit_ols_3 <- lm(
    Income ~ Age + Education + Gender +
      Question_1 + Question_2 + Question_3 + Question_4 + Question_5 +     
      Item_1 + Item_2 + Item_3 + Item_4 + Item_5 + Item_6 + Item_7,
    data = dataset_3
    )
)

# ::::::::::::::::: PLOT MODEL COEFFICIENTS  ::::::::::::::::::::::::::::::::::

# plot all coefficients from all models in a single graph

dwplot(
  list(
    fit_ols_1,
    fit_ols_2,
    fit_ols_3
    )
  ) +
  theme_minimal()

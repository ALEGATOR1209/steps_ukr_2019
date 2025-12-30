library(dplyr)
library(survey)
source("utils/survey.R")

data <- read.csv("data/ukr2019.csv")
data$smoker <- ifelse(data$t1 == 1, 1, 0)

survey <- step2019(data)
results <- svyby(~smoker, ~agerange, survey, svymean, na.rm = TRUE)
print(results)
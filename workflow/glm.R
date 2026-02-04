library(logistf)
library(pROC)

source("utils/survey.R")

data <- read.csv("data/ukr2019.csv")

results <- data %>%
  add_weight_type() %>%
  filter(h17 %in% c(1, 2)) %>%
  filter(h6 == 1) %>%
  filter(!is.na(m16a) & m16a != 888) %>%
  filter(!is.na(m16b) & m16b != 888) %>%
  filter(!is.na(m16c) & m16c != 888) %>%
  filter(a5 %in% c(1, 2)) %>%
  filter(t1 %in% c(1, 2)) %>%
  mutate(
    had_stroke = h17 == 1,
    high_sugar = h7a == 1,
    bps = (m16a + m16b + m16c) / 3,
    active_alcohol_use = ifelse(a5 == 2, F, T),
    quitted_smoking = t1 == 2 & t8 == 1,
    active_smoker = t1 == 1,
  )

survey <- step2019(results)

model <- logistf(
  had_stroke ~ age + mbmi + high_sugar + p16a + d1 + d6 + da1,
  data = results,
  family = binomial
)

summary(model)

model_data <- model$y
prob <- model$predict

roc_obj <- roc(model_data, prob)

auc(roc_obj)
plot(roc_obj)

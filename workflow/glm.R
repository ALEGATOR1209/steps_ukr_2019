source("utils/survey.R")

data <- read.csv("data/ukr2019.csv")
survey <- step2019(data)

results <- data %>%
  add_weight_type() %>%
  filter(h17 %in% c(1, 2)) %>%
  filter(h6 == 1) %>%
  filter(!is.na(m16a) & m16a != 888) %>%
  filter(!is.na(m16b) & m16b != 888) %>%
  filter(!is.na(m16c) & m16c != 888) %>%
  filter(!is.na(m4b) & m4b != 888) %>%
  filter(!is.na(m5a) & m5a != 888) %>%
  filter(!is.na(m5b) & m5b != 888) %>%
  filter(!is.na(m6a) & m6a != 888) %>%
  filter(!is.na(m6b) & m6b != 888) %>%
  filter(a5 %in% c(1, 2)) %>%
  mutate(
    had_stroke = h17 == 1,
    high_sugar = h7a == 1,
    bps = (m16a + m16b + m16c) / 3,
    active_alcohol_use = ifelse(a5 == 2, F, T),
    quitted_smoking = t1 == 2 & t8 == 1,
    systolic_pressure = (m4a + m5a + m5a) / 3,
    diastolic_pressure = (m4b + m5b + m5b) / 3
  )

model <- glm(
  had_stroke ~ age + mbmi + high_sugar + bps + active_alcohol_use + quitted_smoking + systolic_pressure + diastolic_pressure,
  data = results,
  family = binomial
)

summary(model)

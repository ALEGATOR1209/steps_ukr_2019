library(dplyr)
library(forcats)
library(ggplot2)

data <- read.csv("data/ukr2019.csv")
agg_by_lang <- data %>% count(i6)
agg_by_land <- factor(
    agg_by_lang$i6,
    levels = c(1, 2, 3),
    labels = c("Ukrainian", "Russian", "English")
)

ggplot(agg_by_lang, aes(x = fct_reorder(agg_by_land, n), y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  labs(x = "Interview language", y = "Count")

agg_by_region <- data %>% count(region)

ggplot(agg_by_region, aes(x = fct_reorder(region, n), y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  labs(x = "Region", y = "Count")

agg_by_ethnicity <- data %>% count(c6)
agg_by_ethnicity$c6 <- factor(
  agg_by_ethnicity$c6,
  levels = c(1,2,3,4,5,6,7,8,9,10,77,88),
  labels = c(
    "Ukrainians",
    "Bulgarians",
    "Byelorussians",
    "Crimean Tatars",
    "Hungarians",
    "Jews",
    "Poles",
    "Romani people",
    "Romanians",
    "Russians",
    "Other",
    "Refused"
  )
)

ggplot(agg_by_ethnicity, aes(x = fct_reorder(c6, n), y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  scale_y_log10() +
  labs(x = "Ethnicity", y = "Count, log")

agg_by_age_range <- data %>% count(agerange)
ggplot(agg_by_age_range, aes(x = agerange, y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  labs(x = "Age range", y = "Count")

agg_by_education <- data %>% count(c5)
agg_by_education$c5 <- factor(
  agg_by_education$c5,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 88),
  labels = c(
    "No formal schooling",
    "Less than primary school",
    "Primary school completed",
    "Secondary school completed",
    "High school completed",
    "Special secondary",
    "College/University completed",
    "Post graduate degree",
    "Refused"
  )
)

ggplot(agg_by_education, aes(x = c5, y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  scale_y_log10() +
  labs(x = "Education level", y = "Count, log")

agg_by_marital <- data %>% count(c7)
agg_by_marital$c7 <- factor(
  agg_by_marital$c7,
  levels = c(1, 2, 3, 4, 5, 6, 88),
  labels = c(
    "Never married",
    "Currently married",
    "Separated",
    "Divorced",
    "Widowed",
    "Cohabitating",
    "Refused"
  )
)

ggplot(agg_by_marital, aes(x = fct_reorder(c7, n), y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  scale_y_log10() +
  labs(x = "Marital status", y = "Count, log")

agg_by_employment <- data %>% count(c8)
agg_by_employment$c8 <- factor(
    agg_by_employment$c8,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 88),
  labels = c(
    "Employee of a state-owned organisation",
    "Employee of a non-government organisation (NGO)",
    "Employee of a private company",
    "Self-employed",
    "Non-paid",
    "Student",
    "Homemaker",
    "Retired",
    "Unemployed (able to work)",
    "Unemployed (unable to work)",
    "Parental leave",
    "Refused"
  )
)
ggplot(agg_by_employment, aes(x = fct_reorder(c8, n), y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Employment status", y = "Count, log")

agg_by_sex <- data %>% count(sex)
ggplot(agg_by_sex, aes(x = fct_reorder(sex, n), y = n)) + geom_col() + geom_text(
  aes(label = n),
  vjust = -0.3
)

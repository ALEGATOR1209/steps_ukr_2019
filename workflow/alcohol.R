library(survey)
library(ggplot2)
library(forcats)
library(scales)

source("utils/survey.R")

data <- read.csv("data/ukr2019.csv")
survey <- step2019(data)

########################
# SECTION 1. General alcohol use
########################
plot_sober <- function() {
  survey <- update(
    survey,
    sober = ifelse(a1 == 2, 1, 0)
  )

  print("Mean sober people:")
  print(svymean(~sober, survey, na.rm = TRUE))

  print("Soberity over sex and age:")
  results <- bind_rows(
    svyby(~sober, ~sex + agerange, survey, svymean, na.rm = TRUE),
    svyby(~sober, ~sex, survey, svymean, na.rm = TRUE) %>% mutate(agerange = "18-69")
  )

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print(summary(svyglm(sober ~ sex + agerange, design = survey)))
  print(results)

  ggplot(results, aes(x = agerange, y = sober, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(
        ymin = sober - 1.96 * se,
        ymax = sober + 1.96 * se
      ),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    geom_text(
      aes(label = scales::percent(sober, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.2)
    ) +
    labs(
      x = "Age group",
      y = "Soberity",
      fill = "Sex",
      title = "Soberity by sex and age",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_active_alcohol_use <- function() {
  survey <- update(
    subset(survey, a5 == 1 | a5 == 2),
    active_use = ifelse(a5 == 1, 1, 0)
  )

  print("Mean active alcohol use:")
  print(svymean(~active_use, survey, na.rm = TRUE))
  print(summary(svyglm(active_use ~ sex + agerange, design = survey)))

  print("Active use over sex and age:")
  results <- bind_rows(
    svyby(~active_use, ~sex + agerange, survey, svymean, na.rm = TRUE),
    svyby(~active_use, ~sex, survey, svymean, na.rm = TRUE) %>% mutate(agerange = "18-69")
  )

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print(results)

  ggplot(results, aes(x = agerange, y = active_use, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(
        ymin = active_use - 1.96 * se,
        ymax = active_use + 1.96 * se
      ),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    geom_text(
      aes(label = scales::percent(active_use, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1)
    ) +
    labs(
      x = "Age group",
      y = "Used alcohol in last 30 days, %",
      fill = "Sex",
      title = "Alcohol use in last 30 days",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_sober()
plot_active_alcohol_use()

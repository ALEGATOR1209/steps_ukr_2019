library(survey)
library(ggplot2)
library(forcats)
library(scales)

source("utils/survey.R")

data <- read.csv("data/ukr2019.csv")

##################################
# SECTION 1. General tobacco use
##################################

plot_general_tobacco_use <- function() {
  data$smoker <- ifelse(data$t1 == 1, 1, 0)

  survey <- step2019(data)
  print("Mean smoking prevalence:")
  svymean(~smoker, survey, na.rm = TRUE)

  print("Smoking prevalence over sex and age:")
  results <- bind_rows(
    svyby(~smoker, ~sex + agerange, survey, svymean, na.rm = TRUE),
    svyby(~smoker, ~sex, survey, svymean, na.rm = TRUE) %>% mutate(agerange = "18-69")
  )

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print(results)

  ggplot(results, aes(x = agerange, y = smoker, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(
        ymin = smoker - 1.96 * se,
        ymax = smoker + 1.96 * se
      ),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    geom_text(
      aes(label = scales::percent(smoker, accuracy = 0.01)),
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
      y = "Smoking prevalence",
      fill = "Sex",
      title = "Smoking prevalence by sex and age",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_general_tobacco_use()

##################################
# SECTION 2. Vape + THC use
##################################
get_vape_and_thc_users <- function(survey) {
  survey <- update(
    survey,
    vape_ths = ifelse(t1b == 1 | t1c == 1, 1, 0)
  )

  return(survey)
}

plot_general_vape_use <- function() {
  survey <- get_vape_and_thc_users(survey)

  results <- bind_rows(
    svyby(~vape_ths, ~sex + agerange, survey, svymean, na.rm = TRUE),
    svyby(~vape_ths, ~sex, survey, svymean, na.rm = TRUE) %>% mutate(agerange = "18-69")
  )

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print("THS/vaping prevalence:")
  print(results)

  ggplot(results, aes(x = agerange, y = vape_ths, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = scales::percent(vape_ths, accuracy = 0.01)),
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
      y = "Vape/THC prevalence",
      fill = "Sex",
      title = "Vape/THC smoking prevalence by sex and age",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_urban_rural_vape_use <- function() {
  survey <- get_vape_and_thc_users(survey)

  results <- svyby(~vape_ths, ~ur, survey, svymean, na.rm = TRUE)

  ggplot(results, aes(x = ur, y = vape_ths)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(
        ymin = vape_ths - 1.96 * se,
        ymax = vape_ths + 1.96 * se
      ),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    geom_text(
      aes(label = scales::percent(vape_ths, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.2)
    ) +
    labs(
      y = "Vape/THC prevalence",
      title = "Vape/THC smoking prevalence by urban/rural",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_general_vape_use()
plot_urban_rural_vape_use()

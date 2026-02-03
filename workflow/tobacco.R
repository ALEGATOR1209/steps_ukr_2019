library(survey)
library(ggplot2)
library(forcats)
library(scales)

source("utils/survey.R")

data <- read.csv("data/ukr2019.csv")
survey <- step2019(data)

##################################
# SECTION 1. General tobacco use
##################################

plot_general_tobacco_use <- function() {
  survey <- update(survey, smoker = ifelse(data$t1 == 1, 1, 0))
  print("Mean smoking prevalence:")
  print(svymean(~smoker, survey, na.rm = TRUE))

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

  overall <- svymean(~vape_ths, survey, na.rm = TRUE)
  overall <- data.frame(
    ur = "Total",
    vape_ths = coef(overall),
    se = SE(overall)
  )

  results <- svyby(~vape_ths, ~ur, survey, svymean, na.rm = TRUE)
  results <- bind_rows(results, overall)
  results$ur <- factor(
    results$ur,
    levels = c("Rural", "Urban", "Total")
  )

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

##################################
# SECTION 3. Cigarettes use
##################################
get_cigarettes_users <- function(survey) {
  survey <- update(
    survey,
    cigarettes = ifelse(t1a == 1, 1, 0)
  )

  return(survey)
}

plot_general_cigarettes_use <- function() {
  survey <- get_cigarettes_users(survey)

  results <- bind_rows(
    svyby(~cigarettes, ~sex + agerange, survey, svymean, na.rm = TRUE),
    svyby(~cigarettes, ~sex, survey, svymean, na.rm = TRUE) %>% mutate(agerange = "18-69")
  )

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print("Cigarettes prevalence:")
  print(results)

  ggplot(results, aes(x = agerange, y = cigarettes, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = scales::percent(cigarettes, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1.0)
    ) +
    labs(
      x = "Age group",
      y = "Cigarettes prevalence",
      fill = "Sex",
      title = "Cigarette smoking prevalence by sex and age",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_urban_rural_cigarettes_use <- function() {
  survey <- get_cigarettes_users(survey)

  results <- svyby(~cigarettes, ~ur, survey, svymean, na.rm = TRUE)

  ggplot(results, aes(x = ur, y = cigarettes)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(
        ymin = cigarettes - 1.96 * se,
        ymax = cigarettes + 1.96 * se
      ),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    geom_text(
      aes(label = scales::percent(cigarettes, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
        limits = c(0, 1.0)
    ) +
    labs(
      y = "Cigarettes prevalence",
      title = "Cigarettes smoking prevalence by urban/rural",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

##################################
# SECTION 4. Hookah use
##################################
get_hookah_users <- function(survey) {
  survey <- update(
    survey,
    hookah = ifelse(t1d == 1, 1, 0)
  )

  return(survey)
}

plot_general_hookah_use <- function() {
  survey <- get_hookah_users(survey)

  results <- bind_rows(
    svyby(~hookah, ~sex + agerange, survey, svymean, na.rm = TRUE),
    svyby(~hookah, ~sex, survey, svymean, na.rm = TRUE) %>% mutate(agerange = "18-69")
  )

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print("Hookah prevalence:")
  print(results)

  ggplot(results, aes(x = agerange, y = hookah, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = scales::percent(hookah, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1.0)
    ) +
    labs(
      x = "Age group",
      y = "Hookah prevalence",
      fill = "Sex",
      title = "Hookah smoking prevalence by sex and age",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_urban_rural_hookah_use <- function() {
  survey <- get_hookah_users(survey)

  results <- svyby(~hookah, ~ur, survey, svymean, na.rm = TRUE)

  ggplot(results, aes(x = ur, y = hookah)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(
        ymin = hookah - 1.96 * se,
        ymax = hookah + 1.96 * se
      ),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    geom_text(
      aes(label = scales::percent(hookah, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
        limits = c(0, 0.25)
    ) +
    labs(
      y = "Hookah prevalence",
      title = "Hookah smoking prevalence by urban/rural",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

##################################
# SECTION 5. Non-smoked tobacco use
##################################
get_non_smoked_tobacco_users <- function(survey) {
  survey <- update(
    survey,
    non_smoked_tobacco = ifelse(t1e == 1, 1, 0)
  )

  return(survey)
}

plot_general_non_smoked_tobacco_use <- function() {
  survey <- get_non_smoked_tobacco_users(survey)

  results <- bind_rows(
    svyby(~non_smoked_tobacco, ~sex + agerange, survey, svymean, na.rm = TRUE),
    svyby(~non_smoked_tobacco, ~sex, survey, svymean, na.rm = TRUE) %>% mutate(agerange = "18-69")
  )

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print("Non-smoked tobacco prevalence:")
  print(results)

  ggplot(results, aes(x = agerange, y = non_smoked_tobacco, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = scales::percent(non_smoked_tobacco, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.05)
    ) +
    labs(
      x = "Age group",
      y = "Non-smoked tobacco prevalence",
      fill = "Sex",
      title = "Non-smoked tobacco smoking prevalence by sex and age",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_urban_rural_non_smoked_tobacco_use <- function() {
  survey <- get_non_smoked_tobacco_users(survey)

  results <- svyby(~non_smoked_tobacco, ~ur, survey, svymean, na.rm = TRUE)

  ggplot(results, aes(x = ur, y = non_smoked_tobacco)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(
        ymin = non_smoked_tobacco - 1.96 * se,
        ymax = non_smoked_tobacco + 1.96 * se
      ),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    geom_text(
      aes(label = scales::percent(non_smoked_tobacco, accuracy = 0.01)),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
        limits = c(0, 0.05)
    ) +
    labs(
      y = "Non-smoked tobacco prevalence",
      title = "Non-smoked tobacco smoking prevalence by urban/rural",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

##################################
# SECTION 3. Age differences
##################################

get_tobacco_preferences_by_age <- function(age_value = "") {
  subset <- update(
    survey,
    agerange = factor(
      agerange,
      levels = c("18-29", "30-44", "45-59", "60-69")
    )
  )

  if (age_value != "") {
    subset <- subset(subset, agerange == age_value)
  }

  subset <- update(
    subset,
    cigs = ifelse(!is.na(t1a) & t1a == 1, 1, 0),
    ths = ifelse(!is.na(t1b) & t1b == 1, 1, 0),
    vape = ifelse(!is.na(t1c) & t1c == 1, 1, 0),
    hookah = ifelse(!is.na(t1d) & t1d == 1, 1, 0),
    snuff = ifelse(!is.na(t1e) & t1e == 1, 1, 0)
  )

  results <- svymean(~cigs + ths + vape + hookah + snuff, subset, na.rm = TRUE)
  return(results)
}

plot_tobacco_by_age <- function() {
  age_groups <- c("18-29", "30-44", "45-59", "60-69", "")
  age_labels <- c("18-29", "30-44", "45-59", "60-69", "18-69")

  all_results <- data.frame()

  for (i in seq_along(age_groups)) {
    agerange <- age_groups[i]
    label <- age_labels[i]

    results <- get_tobacco_preferences_by_age(agerange)
    results_df <- data.frame(
      product = names(results),
      prevalence = coef(results),
      se = SE(results),
      agerange = label
    )

    all_results <- bind_rows(all_results, results_df)
  }

  all_results$product <- factor(
    all_results$product,
    levels = c("cigs", "ths", "vape", "hookah", "snuff"),
    labels = c("Cigarettes", "THS", "Vape", "Hookah", "Snuff")
  )

  all_results$agerange <- factor(
    all_results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  ggplot(all_results, aes(x = agerange, y = prevalence, fill = product)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = scales::percent(prevalence, accuracy = 0.01)),
      position = position_dodge(width = 0.8),
      vjust = -0.3,
      size = 3.5
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.5)
    ) +
    labs(
      x = "Tobacco product",
      y = "Prevalence",
      fill = "Age group",
      title = "Tobacco product preferences by age group",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_general_vape_use()
plot_urban_rural_vape_use()
plot_general_cigarettes_use()
plot_urban_rural_cigarettes_use()
plot_general_hookah_use()
plot_urban_rural_hookah_use()
plot_general_non_smoked_tobacco_use()
plot_urban_rural_non_smoked_tobacco_use()
plot_tobacco_by_age()

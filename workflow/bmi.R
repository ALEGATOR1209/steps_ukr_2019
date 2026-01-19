library(survey)
library(ggplot2)
library(forcats)
library(scales)

source("utils/survey.R")

data <- read.csv("data/ukr2019.csv")
survey <- step2019(data)

plot_bmi_by_age <- function () {
  print("Mean BMI people:")
  print(svymean(~mbmi, survey, na.rm = TRUE))

  print("BMI over sex and age:")
  results <- bind_rows(
    svyby(~mbmi, ~sex + agerange, survey, svymean, na.rm = TRUE),
    svyby(~mbmi, ~sex, survey, svymean, na.rm = TRUE) %>% mutate(agerange = "18-69")
  )

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print(results)

  ggplot(results, aes(x = agerange, y = mbmi, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(
      aes(
        ymin = mbmi - 1.96 * se,
        ymax = mbmi + 1.96 * se
      ),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    geom_text(
      aes(label = floor(mbmi * 100) / 100),
      position = position_dodge(width = 1.2),
      vjust = -0.3,
      size = 3.5
    ) +
    geom_hline(
      yintercept = 25,
      linetype = "dashed",
      color = "yellow",
      linewidth = 1
    ) +
    geom_hline(
      yintercept = 30,
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    labs(
      x = "Age group",
      y = "BMI",
      fill = "Sex",
      title = "BMI by sex and age",
      subtitle = "Survey-weighted estimates with 95% confidence intervals"
    )
}

plot_bmi_by_drinking <- function() {
  results <- data %>% filter(!is.na(a6) & a6 != 77 & a6 != 88)

  ggplot(results, aes(x = a6, y = mbmi)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    geom_hline(
      yintercept = 18.5,
      linetype = "dashed",
      color = "blue",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 16.5,
      label = "Underweight",
      hjust = 1.1,
      vjust = -0.3
    ) +
    geom_hline(
      yintercept = 25,
      linetype = "dashed",
      color = "yellow",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 26,
      label = "Overweight",
      hjust = 1.1,
      vjust = -0.3
    ) +
    geom_hline(
      yintercept = 30,
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 31,
      label = "Obesity",
      hjust = 1.1,
      vjust = -0.3
    ) +
    labs(
      x = "Drinking occasions, last 30 days, n",
      y = "BMI, kg/m^2",
      title = "BMI by drinking",
    )
}

plot_bmi_by_smoking <- function() {
  results <- data %>% mutate(
    smoking_cig = case_when(
      t2 == 2 ~ 0,
      t5a == 77 | t5a == 88 ~ NA,
      .default = t5a
    )
  ) %>% filter(!is.na(smoking_cig))

  ggplot(results, aes(x = smoking_cig, y = mbmi)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    geom_hline(
      yintercept = 18.5,
      linetype = "dashed",
      color = "blue",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 16.5,
      label = "Underweight",
      hjust = 1.1,
      vjust = -0.3
    ) +
    geom_hline(
      yintercept = 25,
      linetype = "dashed",
      color = "yellow",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 26,
      label = "Overweight",
      hjust = 1.1,
      vjust = -0.3
    ) +
    geom_hline(
      yintercept = 30,
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 31,
      label = "Obesity",
      hjust = 1.1,
      vjust = -0.3
    ) +
    labs(
      x = "Smoking, cigarettes per day, n",
      y = "BMI, kg/m^2",
      title = "BMI by smoking",
    )
}

plot_bmi_by_fruits <- function() {
  results <- data %>% filter(!is.na(d1) & d1 != 77 & d1 != 88) %>%
    filter(!is.na(d2) & d2 != 77 & d2 != 88) %>%
    mutate(fruits_per_week = d1 * d2)

  ggplot(results, aes(x = fruits_per_week, y = mbmi)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    geom_hline(
      yintercept = 18.5,
      linetype = "dashed",
      color = "blue",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 16.5,
      label = "Underweight",
      hjust = 1.1,
      vjust = -0.3
    ) +
    geom_hline(
      yintercept = 25,
      linetype = "dashed",
      color = "yellow",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 26,
      label = "Overweight",
      hjust = 1.1,
      vjust = -0.3
    ) +
    geom_hline(
      yintercept = 30,
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 31,
      label = "Obesity",
      hjust = 1.1,
      vjust = -0.3
    ) +
    labs(
      x = "Fruit servings per week, n",
      y = "BMI, kg/m^2",
      title = "BMI by fruit consumption",
    )
}

plot_bmi_by_vegetables <- function() {
  results <- data %>% filter(!is.na(d3) & d3 != 77 & d3 != 88) %>%
    filter(!is.na(d4) & d4 != 77 & d4 != 88) %>%
    mutate(veggies_per_week = d3 * d4)

  ggplot(results, aes(x = veggies_per_week, y = mbmi)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    geom_hline(
      yintercept = 18.5,
      linetype = "dashed",
      color = "blue",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 16.5,
      label = "Underweight",
      hjust = 1.1,
      vjust = -0.3
    ) +
    geom_hline(
      yintercept = 25,
      linetype = "dashed",
      color = "yellow",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 26,
      label = "Overweight",
      hjust = 1.1,
      vjust = -0.3
    ) +
    geom_hline(
      yintercept = 30,
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = Inf,
      y = 31,
      label = "Obesity",
      hjust = 1.1,
      vjust = -0.3
    ) +
    labs(
      x = "Vegetables servings per week, n",
      y = "BMI, kg/m^2",
      title = "BMI by vegetables consumption (potato excluded)",
    )
}

plot_bmi_by_heart_rate <- function() {
  results <- data %>% filter(!is.na(m16a) & m16a != 888) %>%
    filter(!is.na(m16b) & m16b != 888) %>%
    filter(!is.na(m16c) & m16c != 888) %>%
    mutate(bps = (m16a + m16b + m16c) / 3) %>%
    mutate(
      weight_type = case_when(
        mbmi < 18.5 ~ "underweight",
        mbmi < 25 ~ "normal",
        mbmi < 30 ~ "overweight",
        .default = "obesity"
      )
    )

  print(summary(results$bps))

  grouped <- results %>% group_by(sex, weight_type) %>%
    summarise(
      bps = mean(bps, na.rm = TRUE),
      .groups = "drop"
    )

  all <- results %>% group_by(weight_type) %>%
    summarise(
      bps = mean(bps, na.rm = TRUE),
      .groups = "drop"
    ) %>% mutate(sex = "All")

  results <- bind_rows(grouped, all)

  results$weight_type <- factor(
    results$weight_type,
    levels = c("underweight", "normal", "overweight", "obesity")
  )

  print("Heart rate over bmi clusters:")
  print(results)

  ggplot(results, aes(x = weight_type, y = bps, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_text(
      aes(label = as.integer(bps)),
      position = position_dodge(width = 1.0),
      vjust = -0.3,
      size = 3.5
    ) +
    labs(
      x = "Heart rite, beats/sec",
      y = "BMI, kg/m^2",
      title = "BMI by heart rate",
    )
}

plot_bmi_by_heart_rate <- function() {
  results <- data %>% filter(!is.na(m16a) & m16a != 888) %>%
    filter(!is.na(m16b) & m16b != 888) %>%
    filter(!is.na(m16c) & m16c != 888) %>%
    mutate(bps = (m16a + m16b + m16c) / 3) %>%
    mutate(
      weight_type = case_when(
        mbmi < 18.5 ~ "underweight",
        mbmi < 25 ~ "normal",
        mbmi < 30 ~ "overweight",
        .default = "obesity"
      )
    )

  print(summary(results$bps))

  grouped <- results %>% group_by(sex, weight_type) %>%
    summarise(
      bps = mean(bps, na.rm = TRUE),
      .groups = "drop"
    )

  all <- results %>% group_by(weight_type) %>%
    summarise(
      bps = mean(bps, na.rm = TRUE),
      .groups = "drop"
    ) %>% mutate(sex = "All")

  results <- bind_rows(grouped, all)

  results$weight_type <- factor(
    results$weight_type,
    levels = c("underweight", "normal", "overweight", "obesity")
  )

  print("Heart rate over bmi clusters:")
  print(results)

  ggplot(results, aes(x = weight_type, y = bps, fill = sex)) +
    geom_col(
      position = position_dodge(width = 0.8),
      color = "black",
      linewidth = 0.3
    ) +
    geom_text(
      aes(label = as.integer(bps)),
      position = position_dodge(width = 1.0),
      vjust = -0.3,
      size = 3.5
    ) +
    coord_cartesian(ylim = c(60, 90)) +
    labs(
      x = "Body mass index",
      y = "Heart rate, bps",
      title = "BMI by heart rate",
    )
}

plot_bmi_by_age()
plot_bmi_by_drinking()
plot_bmi_by_smoking()
plot_bmi_by_fruits()
plot_bmi_by_vegetables()
plot_bmi_by_heart_rate()

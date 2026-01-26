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

  results$weight_type <- factor(
    results$weight_type,
    levels = c("underweight", "normal", "overweight", "obesity")
  )

  print(dunnTest(bps ~ weight_type, data = results, method = "bonferroni"))

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

plot_bmi_by_blood_pressure <- function() {
  results <- data %>% filter(!is.na(m4a) & m4a != 888) %>%
    filter(!is.na(m4b) & m4b != 888) %>%
    filter(!is.na(m5a) & m5a != 888) %>%
    filter(!is.na(m5b) & m5b != 888) %>%
    filter(!is.na(m6a) & m6a != 888) %>%
    filter(!is.na(m6b) & m6b != 888) %>%
    mutate(
      systolic_pressure = (m4a + m5a + m5a) / 3,
      diastolic_pressure = (m4b + m5b + m5b) / 3
    ) %>% mutate(
      weight_type = case_when(
        mbmi < 18.5 ~ "underweight",
        mbmi < 25 ~ "normal",
        mbmi < 30 ~ "overweight",
        .default = "obesity"
      )
    )

  results$weight_type <- factor(
    results$weight_type,
    levels = c("underweight", "normal", "overweight", "obesity")
  )

  print(summary(results$systolic_pressure))
  print(summary(results$diastolic_pressure))

  grouped <- results %>% group_by(sex, weight_type) %>%
    summarise(
      systolic_pressure = mean(systolic_pressure, na.rm = TRUE),
      diastolic_pressure = mean(diastolic_pressure, na.rm = TRUE),
      .groups = "drop"
    )

  all <- results %>% group_by(weight_type) %>%
    summarise(
      systolic_pressure = mean(systolic_pressure, na.rm = TRUE),
      diastolic_pressure = mean(diastolic_pressure, na.rm = TRUE),
      .groups = "drop"
    ) %>% mutate(sex = "All")

  grouped <- bind_rows(grouped, all)

  print("Blood pressure over bmi clusters:")
  print(grouped)

  ggplot(grouped, aes(x = weight_type, group = sex)) +
    geom_linerange(
      aes(
        ymin = diastolic_pressure,
        ymax = systolic_pressure,
        color = sex,
      ),
      position = position_dodge(width = 0.6),
      linewidth = 1.2
    ) +
    geom_point(
      aes(
        y = (as.integer(systolic_pressure) + as.integer(diastolic_pressure)) / 2,
        color = sex
      ),
      position = position_dodge(width = 0.6),
      size = 3
    ) +
    geom_text(
      aes(
        y = systolic_pressure,
        label = paste0(as.integer(systolic_pressure), "/", as.integer(diastolic_pressure))
      ),
      position = position_dodge(width = 0.6),
      vjust = -0.4,
      size = 3.5
    ) +
    labs(
      x = "Body mass index",
      y = "Blood pressure, mmHg",
      title = "BMI by blood pressure",
    )

  high_pressure <- results %>%
    mutate(high_bp = systolic_pressure >= 140 | diastolic_pressure >= 90) %>%
    group_by(weight_type) %>%
    summarise(
      total = n(),
      high_bp_n = sum(high_bp, na.rm = TRUE),
      proportion = high_bp_n / total
    ) %>% ungroup()

  print(high_pressure)

  ggplot(high_pressure, aes(x = weight_type, y = proportion)) +
    geom_col() +
    labs(
      x = "Body mass index",
      y = "Proportion of respondents with high pressure",
      title = "BMI by blood pressure",
    )

  print("Systolic pressure across weight type:")
  print(kruskal.test(systolic_pressure ~ weight_type, data = results))
  print(dunnTest(systolic_pressure ~ weight_type, data = results, method = "bonferroni"))

  print("Diastolic pressure across weight type:")
  print(kruskal.test(diastolic_pressure ~ weight_type, data = results))
  print(dunnTest(diastolic_pressure ~ weight_type, data = results, method = "bonferroni"))
}

plot_bmi_by_strokes <- function() {
  results <- data %>%
    filter(!is.na(h17) & h17 != 77 & h17 != 88) %>%
    mutate(
      had_stroke = h17 == 1,
      weight_type = case_when(
        mbmi < 18.5 ~ "underweight",
        mbmi < 25 ~ "normal",
        mbmi < 30 ~ "overweight",
        .default = "obesity"
      )
    )

  results <- results %>% group_by(weight_type) %>%
    summarise(
      total = n(),
      had_stroke = sum(had_stroke, na.rm = TRUE),
      no_stroke = total - had_stroke,
      proportion = had_stroke / total,
    ) %>% ungroup()

  results$weight_type <- factor(
    results$weight_type,
    levels = c("underweight", "normal", "overweight", "obesity")
  )

  ggplot(results, aes(x = weight_type, y = proportion)) +
    geom_col() +
    labs(
      x = "Body mass index",
      y = "Proportion of respondents that had heart attacks",
      title = "Heart attacks by BMI",
    )
}

plot_bmi_by_age()
plot_bmi_by_heart_rate()
plot_bmi_by_blood_pressure()
plot_bmi_by_strokes()

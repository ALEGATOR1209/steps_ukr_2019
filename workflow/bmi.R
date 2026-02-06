library(survey)
library(ggplot2)
library(forcats)
library(scales)
library(FSA)

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

  print(summary(svyglm(mbmi ~ sex + agerange, design = survey)))

  results$agerange <- factor(
    results$agerange,
    levels = c("18-29", "30-44", "45-59", "60-69", "18-69")
  )

  print(results)

  p <- ggplot(results, aes(x = agerange, y = mbmi, fill = sex)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_hline(
      aes(yintercept = 25, color = "Overweight (BMI = 25)"),
      linetype = "dashed",
      linewidth = 1
    ) +
    geom_hline(
      aes(yintercept = 30, color = "Obesity (BMI = 30)"),
      linetype = "dashed",
      linewidth = 1
    ) +
    scale_color_manual(
      name = "BMI thresholds",
      values = c(
        "Overweight (BMI = 25)" = "yellow",
        "Obesity (BMI = 30)" = "red"
      )
    ) +
    labs(
      x = "Age group",
      y = "BMI",
      fill = "Sex",
    )

  ggsave(
    filename = "results/bmi.png",
    plot = p,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
  )

  p + labs(
    title = "BMI by sex and age",
    subtitle = "Survey-weighted estimates with 95% confidence intervals"
  )
}

plot_bmi_by_heart_rate <- function() {
  results <- data %>% filter(!is.na(m16a) & m16a != 888) %>%
    filter(!is.na(m16b) & m16b != 888) %>%
    filter(!is.na(m16c) & m16c != 888) %>%
    mutate(bps = (m16a + m16b + m16c) / 3) %>%
    add_weight_type()

  print(kruskal.test(bps ~ weight_type, data = results))
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
    ) %>% add_weight_type()

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

  p1 <- ggplot(grouped, aes(x = weight_type, group = sex)) +
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
    )

  high_pressure <- results %>%
    mutate(high_bp = systolic_pressure >= 140 | diastolic_pressure >= 90) %>%
    group_by(weight_type) %>%
    summarise(
      total = n(),
      high_bp_n = sum(high_bp, na.rm = TRUE),
      proportion = high_bp_n / total,
      odds_high_bp = high_bp_n / (total - high_bp_n),
      .groups = "drop"
    )

  odds_normal <- high_pressure %>%
    filter(weight_type == "normal") %>%
    pull(odds_high_bp)

  high_pressure <- high_pressure %>%
    mutate(
      odds_ratio_vs_normal = odds_high_bp / odds_normal
    )

  print(high_pressure)

  p2 <- ggplot(high_pressure, aes(x = weight_type, y = proportion)) +
    geom_col() +
    labs(
      x = "Body mass index",
      y = "Proportion of respondents with high pressure",
    )

  ggsave(
    filename = "results/bmi_blood_pressure.png",
    plot = p1,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
  )

  ggsave(
    filename = "results/bmi_high_pressure.png",
    plot = p2,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
  )

  print("Systolic pressure across weight type:")
  print(kruskal.test(systolic_pressure ~ weight_type, data = results))
  print(dunnTest(systolic_pressure ~ weight_type, data = results, method = "bonferroni"))

  print("Diastolic pressure across weight type:")
  print(kruskal.test(diastolic_pressure ~ weight_type, data = results))
  print(dunnTest(diastolic_pressure ~ weight_type, data = results, method = "bonferroni"))
  print(p1 + labs(title = "BMI by blood pressure"))
  print(p2 + labs(title = "BMI by blood pressure"))
}

plot_bmi_by_strokes <- function() {
  results <- data %>%
    filter(!is.na(h17) & h17 != 77 & h17 != 88) %>%
    mutate(had_stroke = h17 == 1) %>%
    add_weight_type()

  results <- results %>% group_by(weight_type) %>%
    summarise(
      total = n(),
      had_stroke = sum(had_stroke, na.rm = TRUE),
      proportion = had_stroke / total,
      odds_stroke = had_stroke / (total - had_stroke),
      .groups = "drop"
    )

  odds_normal <- results %>%
    filter(weight_type == "normal") %>%
    pull(odds_stroke)

  results <- results %>%
    mutate(
      odds_ratio_vs_normal = odds_stroke / odds_normal
    )

  print(results)

  p <- ggplot(results, aes(x = weight_type, y = proportion)) +
    geom_col() +
    labs(
      x = "Body mass index",
      y = "Proportion of respondents that had heart problems",
    )

  ggsave(
    filename = "results/bmi_strokes.png",
    plot = p,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
  )

  print("Strokes across weight type:")
  print(kruskal.test(had_stroke ~ weight_type, data = results))
  print(dunnTest(had_stroke ~ weight_type, data = results, method = "bonferroni"))

  p + labs(
    title = "Heart attacks by BMI",
  )
}

plot_bmi_by_physical_activity <- function() {
  results <- data %>%
    mutate(high_activity = p1 == 1 | p10 == 1) %>%
    mutate(
      high_activity_days = case_when(
        p4 == 77 | p11 == 77 ~ NA_integer_,
        is.na(p2) & is.na(p11) ~ NA_integer_,
        TRUE ~ pmin.int(coalesce(p2, 0) + coalesce(p11, 0), 7)
      )
    ) %>%
    filter(!is.na(high_activity_days)) %>%
    add_weight_type()

  mean_activity <- results %>%
    group_by(weight_type) %>%
    summarise(mean_days = mean(high_activity_days, na.rm = TRUE))

  ggplot(mean_activity, aes(x = weight_type, y = mean_days, fill = weight_type)) +
    geom_col() +
    labs(
      x = "Body mass index",
      y = "Number of high physical activity days per week",
      title = "Physical activity by BMI",
    )
}

plot_bmi_by_age()
plot_bmi_by_heart_rate()
plot_bmi_by_blood_pressure()
plot_bmi_by_strokes()
plot_bmi_by_physical_activity()

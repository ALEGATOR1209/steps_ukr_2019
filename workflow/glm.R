library(logistf)
library(pROC)
library(survey)

source("utils/survey.R")

compute_fscore <- function(y_true, prob, threshold) {
  y_pred <- prob >= threshold

  tp <- sum(y_pred & y_true, na.rm = TRUE)
  fp <- sum(y_pred & !y_true, na.rm = TRUE)
  fn <- sum(!y_pred & y_true, na.rm = TRUE)
  tn <- sum(!y_pred & !y_true, na.rm = TRUE)

  precision <- ifelse(tp + fp == 0, NA, tp / (tp + fp))
  recall    <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
  f1 <- ifelse(
    is.na(precision) | is.na(recall) | (precision + recall == 0),
    NA,
    2 * precision * recall / (precision + recall)
  )

  list(
    precision = precision,
    recall = recall,
    f1 = f1,
    confusion = c(TP = tp, FP = fp, FN = fn, TN = tn)
  )
}

fit_firth_model <- function(data, formula) {
  cat("\n========== FIRTH LOGISTIC REGRESSION ==========\n")

  model <- logistf(
    formula,
    data = data,
    family = binomial
  )

  print(summary(model))

  prob <- model$predict
  model_data <- model$y

  roc_obj <- roc(model_data, prob, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))
  ci_auc <- as.numeric(ci.auc(roc_obj))

  coords_obj <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

  best_threshold <- as.numeric(coords_obj$threshold[1])
  best_sensitivity <- as.numeric(coords_obj$sensitivity[1])
  best_specificity <- as.numeric(coords_obj$specificity[1])

  fscore <- compute_fscore(model_data, prob, best_threshold)


  cat("\n--- Performance Metrics ---\n")
  cat(sprintf("AUC: %.4f (95%% CI: %.4f - %.4f)\n",
              auc_value, ci_auc[1], ci_auc[3]))
  cat(sprintf("Optimal threshold: %.4f\n", best_threshold))
  cat(sprintf("Sensitivity: %.4f\n", best_sensitivity))
  cat(sprintf("Specificity: %.4f\n", best_specificity))
  cat(sprintf("Precision: %.4f\n", fscore$precision))
  cat(sprintf("F1-score: %.4f\n", fscore$f1))
  cat(sprintf("Number of observations: %d\n", length(model_data)))
  cat(sprintf("Number of events: %d (%.2f%%)\n",
              sum(model_data), 100 * mean(model_data)))

  return(list(
    model = model,
    auc = auc_value,
    ci_lower = ci_auc[1],
    ci_upper = ci_auc[3],
    roc = roc_obj,
    predictions = prob,
    sensitivity = best_sensitivity,
    specificity = best_specificity,
    threshold = best_threshold,
    precision = fscore$precision,
    f1 = fscore$f1
  ))
}

fit_glm_model <- function(data, formula) {
  cat("\n========== STANDARD GLM ==========\n")

  model <- glm(
    formula,
    data = data,
    family = binomial(link = "logit")
  )

  print(summary(model))

  prob <- predict(model, type = "response")
  model_data <- model$y

  roc_obj <- roc(model_data, prob, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))
  ci_auc <- as.numeric(ci.auc(roc_obj))

  coords_obj <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

  best_threshold <- as.numeric(coords_obj$threshold[1])
  best_sensitivity <- as.numeric(coords_obj$sensitivity[1])
  best_specificity <- as.numeric(coords_obj$specificity[1])

  fscore <- compute_fscore(model_data, prob, best_threshold)

  loglik <- logLik(model)
  aic_value <- AIC(model)
  bic_value <- BIC(model)

  cat("\n--- Performance Metrics ---\n")
  cat(sprintf("AUC: %.4f (95%% CI: %.4f - %.4f)\n",
              auc_value, ci_auc[1], ci_auc[3]))
  cat(sprintf("Optimal threshold: %.4f\n", best_threshold))
  cat(sprintf("Sensitivity: %.4f\n", best_sensitivity))
  cat(sprintf("Specificity: %.4f\n", best_specificity))
  cat(sprintf("Precision: %.4f\n", fscore$precision))
  cat(sprintf("F1-score: %.4f\n", fscore$f1))
  cat(sprintf("Log-Likelihood: %.4f\n", as.numeric(loglik)))
  cat(sprintf("AIC: %.4f\n", aic_value))
  cat(sprintf("BIC: %.4f\n", bic_value))
  cat(sprintf("Number of observations: %d\n", length(model_data)))
  cat(sprintf("Number of events: %d (%.2f%%)\n",
              sum(model_data), 100 * mean(model_data)))

  return(list(
    model = model,
    auc = auc_value,
    ci_lower = ci_auc[1],
    ci_upper = ci_auc[3],
    roc = roc_obj,
    predictions = prob,
    sensitivity = best_sensitivity,
    specificity = best_specificity,
    threshold = best_threshold,
    precision = fscore$precision,
    f1 = fscore$f1,
    aic = aic_value,
    bic = bic_value
  ))
}

fit_svyglm_model <- function(data, formula, survey_design) {
  cat("\n========== SURVEY-WEIGHTED GLM ==========\n")

  model <- svyglm(
    formula,
    design = survey_design,
    family = quasibinomial(link = "logit")
  )

  print(summary(model))

  prob <- predict(model, type = "response")
  model_data <- model$y

  roc_obj <- roc(model_data, prob, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))
  ci_auc <- as.numeric(ci.auc(roc_obj))

  coords_obj <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

  best_threshold <- as.numeric(coords_obj$threshold[1])
  best_sensitivity <- as.numeric(coords_obj$sensitivity[1])
  best_specificity <- as.numeric(coords_obj$specificity[1])
  fscore <- compute_fscore(model_data, prob, best_threshold)

  aic_value <- AIC(model)

  cat("\n--- Performance Metrics ---\n")
  cat(sprintf("AUC: %.4f (95%% CI: %.4f - %.4f)\n",
              auc_value, ci_auc[1], ci_auc[3]))
  cat(sprintf("Optimal threshold: %.4f\n", best_threshold))
  cat(sprintf("Sensitivity: %.4f\n", best_sensitivity))
  cat(sprintf("Specificity: %.4f\n", best_specificity))
  cat(sprintf("Precision: %.4f\n", fscore$precision))
  cat(sprintf("F1-score: %.4f\n", fscore$f1))
  cat(sprintf("AIC: %.4f\n", aic_value))
  cat(sprintf("Number of observations: %d\n", length(model_data)))
  cat(sprintf("Number of events: %d (%.2f%%)\n",
              sum(model_data), 100 * mean(model_data)))
  cat(sprintf("Effective sample size: %.1f\n",
              sum(weights(survey_design, "sampling"))))

  return(list(
    model = model,
    auc = auc_value,
    ci_lower = ci_auc[1],
    ci_upper = ci_auc[3],
    roc = roc_obj,
    predictions = prob,
    sensitivity = best_sensitivity,
    specificity = best_specificity,
    threshold = best_threshold,
    precision = fscore$precision,
    f1 = fscore$f1,
    aic = aic_value
  ))
}

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
  filter(d6 != 77 & d6 != 88) %>%
  filter(p16a != 77 & p16a != 88) %>%
  mutate(
    had_stroke = h17 == 1,
    high_sugar = h7a == 1,
    bps = (m16a + m16b + m16c) / 3,
    active_alcohol_use = ifelse(a5 == 2, FALSE, TRUE),
    quitted_smoking = t1 == 2 & t8 == 1,
    active_smoker = t1 == 1,
    salt_frequency = d6,
    time_sitting_hours = p16a,
  )

survey <- step2019(results)

formula <- had_stroke ~ age + mbmi + high_sugar + salt_frequency

cat("\n" , rep("=", 60), "\n", sep = "")
cat("FITTING THREE MODELS\n")
cat(rep("=", 60), "\n", sep = "")

firth_results <- fit_firth_model(results, formula)
glm_results <- fit_glm_model(results, formula)
svyglm_results <- fit_svyglm_model(results, formula, survey)

plot(glm_results$roc, col = "blue", main = "ROC Curve Comparison")
lines(firth_results$roc, col = "red")
lines(svyglm_results$roc, col = "green")
legend("bottomright",
       legend = c(
         sprintf("GLM (AUC=%.3f)", glm_results$auc),
         sprintf("Firth (AUC=%.3f)", firth_results$auc),
         sprintf("Survey GLM (AUC=%.3f)", svyglm_results$auc)
       ),
       col = c("blue", "red", "green"),
       lty = 1,
       cex = 1.8)

par(mfrow = c(1, 1))

cat("\n", rep("=", 60), "\n", sep = "")
cat("SUMMARY COMPARISON\n")
cat(rep("=", 60), "\n", sep = "")

comparison <- data.frame(
  Model = c("Firth Logistic", "Standard GLM", "Survey-weighted GLM"),
  AUC = c(firth_results$auc, glm_results$auc, svyglm_results$auc),
  AUC_Lower = c(firth_results$ci_lower, glm_results$ci_lower, svyglm_results$ci_lower),
  AUC_Upper = c(firth_results$ci_upper, glm_results$ci_upper, svyglm_results$ci_upper),
  Sensitivity = c(firth_results$sensitivity, glm_results$sensitivity, svyglm_results$sensitivity),
  Specificity = c(firth_results$specificity, glm_results$specificity, svyglm_results$specificity),
  Threshold = c(firth_results$threshold, glm_results$threshold, svyglm_results$threshold),
  F1 = c(firth_results$f1, glm_results$f1, svyglm_results$f1)
)

print(comparison, row.names = FALSE)

results_list <- list(
  firth = firth_results,
  glm = glm_results,
  svyglm = svyglm_results,
  comparison = comparison
)

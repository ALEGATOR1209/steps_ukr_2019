library(dplyr)
library(survey)

load_derzhstat_file <- function(path) {
  derzhstat_demographics <- read.csv(path, sep = "\t")
  names(derzhstat_demographics) <- c("age", "population")

  derzhstat_demographics <- derzhstat_demographics %>% mutate(
    age = as.integer(as.character(age)),
    population = as.integer(as.character(population))
  ) %>% filter(!is.na(age) & age >= 18 & age <= 69)

  derzhstat_demographics <- derzhstat_demographics %>%
    mutate(agerange = cut(age,
                          breaks = c(18, 30, 45, 60, 70),
                          right = FALSE,
                          labels = c("18-29", "30-44", "45-59", "60-69"))) %>%
    group_by(agerange) %>%
    summarise(total_pop = sum(population, na.rm = TRUE))

  return(derzhstat_demographics)
}

derzhstat_age_brackets <- function () {
  male <- load_derzhstat_file("data/derzhstat_2019_male.csv")
  female <- load_derzhstat_file("data/derzhstat_2019_female.csv")

  male$sex <- "Men"
  female$sex <- "Women"
  result <- bind_rows(male, female)
  result$bracket <- paste(result$sex, result$agerange, sep = "_")
  result <- result %>% mutate(prop = total_pop / sum(total_pop))

  return(result)
}

step2019 <- function (data) {
  survey <- svydesign(
    id = ~psu,
    strata = ~stratum,
    weights = ~wstep1,
    data = data,
    nest = TRUE
  )

  demographics <- derzhstat_age_brackets()
  pop <- demographics$prop
  names(pop) <- demographics$bracket

  survey <- update(
    survey,
    bracket = paste(sex, agerange, sep = "_")
  )

  survey <- svystandardize(
    design = survey,
    by = ~bracket,
    over = ~1,
    population = pop
  )
}

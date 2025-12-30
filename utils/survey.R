derzhstat_age_brackets <- function () {
  derzhstat_demographics <- read.csv("data/000_0204.csv", sep = "\t")
  derzhstat_demographics <- derzhstat_demographics[, -c(1, 2)]
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
    summarise(total_pop = sum(population, na.rm = TRUE)) %>%
    mutate(prop = total_pop / sum(total_pop))

  return(derzhstat_demographics)
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

  survey <- svystandardize(
    design = survey,
    by = ~agerange,
    over = ~1,
    population = demographics$prop
  )
}

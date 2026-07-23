clean_location <- function(location_vec) {
  stopifnot(is.character(location_vec))

  location_vec[location_vec == "versailles"] <- "Versailles"
  location_vec[location_vec == "santa monica"] <- "Los Angeles"
  location_vec[location_vec == "Los Angeles Center Studios"] <- "Los Angeles"
  location_vec[location_vec == "Laid off 3/16"] <- NA

  location_vec
}


annualize_salary <- function(salary, salary_type) {
  stopifnot(length(salary) == length(salary_type))

  annual_salary <- rep(NA, length(salary))
  annual_salary[salary_type == "year"] <- salary[salary_type == "year"]
  annual_salary[salary_type == "hour"] <- salary[salary_type == "hour"] * 2080
  annual_salary[salary_type == "week"] <- salary[salary_type == "week"] * 52

  annual_salary
}


load_clean_salary_data <- function(path, min_location_n = 10) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  raw <- read.csv(path, stringsAsFactors = FALSE)

  raw$location <- clean_location(raw$location)
  raw$annual_salary <- annualize_salary(raw$current_salary, raw$salary_type)


  location_counts <- table(raw$location)
  valid_locations <- names(location_counts[location_counts >= min_location_n])

  keep <- raw$location %in% valid_locations &
    !is.na(raw$annual_salary) &
    raw$annual_salary > 1000

  cleaned <- raw[keep, ]
  cleaned$location <- factor(cleaned$location, levels = valid_locations)
  cleaned$log_annual_salary <- log(cleaned$annual_salary)

  stopifnot(nrow(cleaned) > 0)

  cleaned
}

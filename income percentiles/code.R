library(tidyverse)
library(haven)
library(forcats)
library(scales)

## Goal: explore wage‐income percentiles in ACS microdata.
## Steps: load data, clean wages, define sub-samples (all, employed, full-time, full-time+college),
## then visualise key percentiles and percentile rank of selected incomes.

# Set working directory to this project's folder
setwd("~/Documents/GitHub/acs_projects_personal/income percentiles")

# Load data
acs <- read_dta("acs.dta")

# Ensure output folder exists

## Clean wage variable and convert selected columns to factors

acs <- acs %>%
  mutate(
    incwage = as.numeric(incwage),
    incwage = ifelse(incwage %in% c(999999, 999998), NA, incwage)
  )


# Specify the variables to convert
vars <- c(
  "sex", "race", "hispan", "educ", "educd", "degfield", "marst",
  "citizen", "empstat", "labforce"
)

acs <- acs %>%
  mutate(across(all_of(vars), as_factor))

acs_employed <- acs %>%
  filter(age >= 18 & age <= 65) %>%
  filter(empstat == "employed") %>%
  filter(!is.na(incwage)) %>%
  filter(incwage > 0)



acs_employed_full_time <- acs %>%
  filter(age >= 18 & age <= 65) %>%
  filter(empstat == "employed") %>%
  filter(uhrswork >= 35) %>%
  filter(!is.na(incwage)) %>%
  filter(incwage > 0)

acs_employed_full_time_college <- acs %>%
  filter(age >= 18 & age <= 65) %>%
  filter(empstat == "employed") %>%
  filter(uhrswork >= 35) %>%
  filter(educ == "4 years of college" | educ == "5+ years of college") %>%
  filter(!is.na(incwage)) %>%
  filter(incwage > 0)



# Create a function for percentile plots
create_percentile_plot <- function(data, title) {
  data %>%
    summarise(
      p25 = quantile(incwage, 0.25, na.rm = TRUE),
      median = median(incwage, na.rm = TRUE),
      mean = mean(incwage, na.rm = TRUE),
      p75 = quantile(incwage, 0.75, na.rm = TRUE),
      p90 = quantile(incwage, 0.90, na.rm = TRUE),
      p95 = quantile(incwage, 0.95, na.rm = TRUE),
      p99 = quantile(incwage, 0.99, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
    ggplot(aes(x = fct_reorder(stat, value), y = value, fill = stat)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = scales::dollar(value)), vjust = -0.5, size = 3) +
    scale_fill_viridis_d(option = "D") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = title, x = NULL, y = "Income ($)") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Create plots using the function
overall_percentiles <- create_percentile_plot(acs, "Wage Income Distribution for All Americans")
print(overall_percentiles)
ggsave("overall_percentiles.png", width = 8, height = 6, units = "in")

employed_percentiles <- create_percentile_plot(acs_employed, "Wage Income Distribution for Employed Americans")
print(employed_percentiles)
ggsave("employed_percentiles.png", width = 8, height = 6, units = "in")

employed_full_time_percentiles <- create_percentile_plot(
  acs_employed_full_time,
  "Wage Income Distribution for Full-Time Employed Americans"
)
print(employed_full_time_percentiles)
ggsave("employed_full_time_percentiles.png", width = 8, height = 6, units = "in")

employed_full_time_college_percentiles <- create_percentile_plot(
  acs_employed_full_time_college,
  "Wage Income Distribution for Full-Time Employed College Graduates"
)
print(employed_full_time_college_percentiles)
ggsave("employed_full_time_college_percentiles.png", width = 8, height = 6, units = "in")



percentile_100k <- tibble(
  dataset = c("Overall", "Employed", "Full-Time Employed", "Full-Time Employed College Grads"),
  percentile = c(
    ecdf(acs$incwage)(100000),
    ecdf(acs_employed$incwage)(100000),
    ecdf(acs_employed_full_time$incwage)(100000),
    ecdf(acs_employed_full_time_college$incwage)(100000)
  )
)

# Percentile of $100k bar chart – use single viridis colour
percentile_100k %>%
  ggplot(aes(x = fct_reorder(dataset, desc(percentile)), y = percentile, fill = dataset)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(percentile, accuracy = 0.1)), vjust = -0.5, size = 3) +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentile Rank of $100,000 Income", x = NULL, y = "Percentile") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggsave("percentile_100k.png", width = 8, height = 6, units = "in")

# Create a function to show percentile of specific income
create_income_percentile_plot <- function(data, income_value, title) {
  # Calculate the percentile
  percentile <- ecdf(data$incwage)(income_value)

  # Create the plot
  data %>%
    ggplot(aes(x = incwage)) +
    geom_density(fill = "steelblue", alpha = 0.3) +
    geom_vline(xintercept = income_value, color = "red", linetype = "dashed") +
    annotate("text",
      x = income_value,
      y = max(density(data$incwage)$y),
      label = paste0(
        "$", scales::comma(income_value), "\n",
        scales::percent(percentile, accuracy = 0.1), " percentile"
      ),
      hjust = -0.1,
      vjust = 1,
      color = "red",
      size = 4
    ) +
    scale_x_continuous(labels = scales::dollar) +
    labs(
      title = title,
      x = "Income",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
}

# Create the plot for full-time employed workers where you can choose any income level
full_time_income_plot <- create_income_percentile_plot(
  acs_employed_full_time,
  160000,
  "Income Distribution for Full-Time Employed Workers\nwith $160,000 Income Level"
)

print(full_time_income_plot)
ggsave("full_time_income_distribution.png", width = 10, height = 6, units = "in")

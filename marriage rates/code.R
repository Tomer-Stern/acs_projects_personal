library(tidyverse)
library(scales)
library(ipumsr)

## Goal: compare age-specific marriage rates across birth cohorts (ACS 1-year files 2000-2023).
## Steps: (1) derive birth year and 10-year cohort; (2) flag ever-married; (3) aggregate weighted counts
## by age × cohort × year and compute rate; (4) average across years and plot.

## First, we need to create a birth-year and then birth cohort variable, by decade.
## Second, we will make a ever_married dummy variable.
## Third, we will sum up the total number of ever married and never married people by age-sample year
## Finally, calculate the marriage rate for each age-birth_cohort, averaged across sample years

# Set working directory
setwd("~/Documents/GitHub/acs_projects_personal/marriage rates")

## read in acs.dat and acs.xml
# Load ACS microdata
acs <- read_ipums_micro(
  ddi       = "acs.xml",
  data_file = "acs.dat"
) %>%
  rename_with(tolower) %>%
  select(perwt, year, age, marst) %>%
  mutate(marst = as_factor(marst))


marriage_rates <- acs %>%
  filter(age >= 18, age <= 55) %>%
  mutate(
    birth_year = year - age,
    birth_cohort = cut(
      birth_year,
      breaks = seq(1900, 2010, by = 10),
      right = FALSE,
      include.lowest = TRUE,
      labels = paste0(seq(1900, 2000, by = 10), "s")
    ),
    ever_married = case_when(
      marst %in% c("Married, spouse present", "Married, spouse absent", "Separated", "Divorced", "Widowed") ~ 1L,
      marst == "Never married/single" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(birth_cohort), !is.na(ever_married), !is.na(age)) %>%
  group_by(age, birth_cohort, year) %>%
  summarise(
    n = sum(perwt, na.rm = TRUE),
    n_ever_married = sum(perwt[ever_married == 1], na.rm = TRUE),
    marriage_rate = n_ever_married / n
  ) %>%
  ungroup() %>%
  group_by(age, birth_cohort) %>%
  summarise(
    marriage_rate = mean(marriage_rate, na.rm = TRUE),
    n = sum(n, na.rm = TRUE)
  ) %>%
  ungroup()

# Get cohorts that actually appear in the data, in chronological order
all_cohorts <- marriage_rates %>%
  distinct(birth_cohort) %>%
  arrange(birth_cohort) %>%
  pull(birth_cohort)

# Convert birth_cohort to factor with only the levels that exist in the data
marriage_rates <- marriage_rates %>%
  mutate(birth_cohort = factor(birth_cohort, levels = all_cohorts))


# Professional-quality plot
p <- ggplot(marriage_rates, aes(x = age, y = marriage_rate, color = birth_cohort, group = birth_cohort)) +
  geom_line(size = 1.2) +
  # Use a colour-blind-friendly discrete palette
  scale_color_viridis_d(name = "Birth Cohort", option = "D") +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1, big.mark = ",", decimal.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Marriage Rate by Age and Birth Cohort",
    x = "Age",
    y = "Marriage Rate",
    color = "Birth Cohort",
    caption = "Source: American Community Survey (ACS) 1-year samples (2000-2023)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Only horizontal grid lines
    panel.grid.major.y = element_line(linetype = "dotted"),
    legend.position = "right",
    plot.caption = element_text(size = 10, colour = "grey50", hjust = 1)
  )

# Print to RStudio Plots window
print(p)

# Export as PNG and SVG with white background
ggsave("marriage_rate_by_age_cohort.png", p, width = 9, height = 6, dpi = 300, bg = "white")
ggsave("marriage_rate_by_age_cohort.svg", p, width = 9, height = 6, bg = "white")
## As we can see, the marriage rate is shifting forwar each decade. However, the rate of these shifts has slowed down.
## The 2000s cohort is very close to the 1990s cohort. But the 1990s cohort had a sizeable gap with the 1980s cohort.

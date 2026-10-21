library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
 # c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#  get = "economic.data",
#  from = "1976-01-01"
# )
# head(unemp_raw)
#
# dir.create("data", showWarnings = FALSE)
#
# write_rds(unemp_raw, "data/unemp_raw.rds")


# 4.1 Clean up Data
unemp_raw <-  read_rds("data/unemp_raw.rds")
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"   ~ "Oregon",
      symbol == "CAUR"   ~ "California",
      symbol == "TXUR"   ~ "Texas",
      symbol == "NYUR"   ~ "New York"
    ))
head(unemp)
unemp |> filter(date == as.Date("2025-10-01"))

#4.2 Fill missing Oct 2025 value 
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp |> filter(date == as.Date("2025-10-01"))

#4.2 Wrangle Data
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate, na.rm = TRUE),
    max_rate  = max(unemployment_rate, na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate, na.rm = TRUE),
    max_rate  = max(unemployment_rate, na.rm = TRUE)
  )

unemp_summary
unemp_recent_summary

# 4.3: largest month over month increase and decrease
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5)

# 4.4: Extremes excluding 2020
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5)

unemp_top_increases
unemp_top_decreases
unemp_top_increases_no_covid
unemp_top_decreases_no_covid

# 5.1 Single line plot - one line per region
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Unemployment Rate: U.S. and Selected States",
    x = "Date",
    y = "Unemployment rate (%)",
    color = "Region"
  ) +
  theme_minimal()

# 5.2 Individual line plot
ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Unemployment Rate by Region",
    x = "Date",
    y = "Unemployment rate (%)"
  ) +
  theme_minimal()

# 5.3 Bar Chart
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col() +
  labs(
    title = "Average Unemployment Rate by Region, 1976 to Present",
    x = "Region",
    y = "Mean unemployment rate (%)"
  ) +
  theme_minimal()

# 5.4 Oregon and California Gap Relative to US
unemp |>
  select(date, region, unemployment_rate) |>
  pivot_wider(names_from = region, values_from = unemployment_rate) |>
  mutate(
    California = California - `United States`,
    Oregon = Oregon - `United States`
  ) |>
  select(date, California, Oregon) |>
  pivot_longer(-date, names_to = "region", values_to = "gap") |>
  ggplot(aes(x = date, y = gap, color = region)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Unemployment Rate Gap vs. U.S.: California and Oregon",
    x = "Date",
    y = "Percentage points above U.S. rate",
    color = "State"
  ) +
  theme_minimal()
# I chose to compare California and Oregon's gap relative to the US rate. I thought this would be interesting since most students at U of O are from either of these states, and it would provide an interesting view of how unemployment compares since they are so close in proximity. The chart displays that California is more often above the national unemployment rate than Oregon, though the peaks and dips for each state loosely mirror each other.

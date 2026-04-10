library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)

df <- read_csv("./data/hybrid_fashion_data.csv")

df <- df %>%
  mutate(transaction_date = dmy(transaction_date))

df_approved <- df %>%
  filter(order_status == "Approved")

customer_cohort <- df_approved %>%
  group_by(customer_id) %>%
  summarise(
    first_purchase_date = min(transaction_date)
  )

customer_cohort <- customer_cohort %>%
  mutate(
    cohort_month = floor_date(first_purchase_date, unit = "month")
  )

cohort_data <- df_approved %>%
  left_join(customer_cohort, by = "customer_id") %>%
  mutate(
    transaction_month = floor_date(transaction_date, unit = "month")
  )

# months function from lubridate package to calculate the cohort index.

cohort_data <- cohort_data %>%
  mutate(
    cohort_index = interval(cohort_month, transaction_month) %/% months(1)
  )

# Retention count table
# Calculate the number of unique customers in each cohort and cohort index combination.

cohort_counts <- cohort_data %>%
  group_by(cohort_month, cohort_index) %>%
  summarise(
    customers = n_distinct(customer_id),
    .groups = "drop"
  )

cohort_size <- cohort_counts %>%
  filter(cohort_index == 0) %>%
  select(cohort_month, cohort_size = customers)

cohort_retention <- cohort_counts %>%
  left_join(cohort_size, by = "cohort_month")

cohort_retention <- cohort_retention %>%
  mutate(
    retention_rate = customers / cohort_size
  )

cohort_retention <- cohort_retention %>%
  mutate(
    retention_rate = customers / cohort_size
  )

cohort_matrix <- cohort_retention %>%
  select(cohort_month, cohort_index, retention_rate) %>%
  pivot_wider(
    names_from = cohort_index,
    values_from = retention_rate
  )


cohort_matrix_pct <- cohort_retention %>%
  mutate(retention_pct = round(retention_rate * 100, 1)) %>%
  select(cohort_month, cohort_index, retention_pct) %>%
  pivot_wider(
    names_from = cohort_index,
    values_from = retention_pct
  )

cohort_retention <- cohort_retention %>%
  mutate(
    retention_label = paste0(round(retention_rate * 100, 1), "%")
  )

ggplot(cohort_retention, aes(x = cohort_index, y = cohort_month, fill = retention_rate)) +
  geom_tile() +
  geom_text(aes(label = retention_label), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_y_date(date_labels = "%Y-%m") +
  labs(
    title = "Cohort Retention Heatmap",
    x = "Months Since First Purchase",
    y = "Cohort Month",
    fill = "Retention Rate"
  ) +
  theme_minimal()
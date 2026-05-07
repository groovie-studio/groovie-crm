library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)

# ------------------------------------------------------------
# 06_churn_signal_detection.R
# Step 1: Load data
# ------------------------------------------------------------

orders <- read_csv("./data/hybrid_fashion_data_w_profit.csv")

lrfmp_base <- read_excel("./reports/lrfmp_base.xlsx")

orders_clean <- orders %>%
  mutate(
    transaction_date = as.Date(transaction_date),
    order_month = floor_date(transaction_date, "month")
  )

analysis_date <- max(orders_clean$transaction_date, na.rm = TRUE)

# ------------------------------------------------------------
# Step 2: Customer churn base
# ------------------------------------------------------------

customer_churn_base <- lrfmp_base %>%
  mutate(
    first_order = as.Date(first_order),
    last_order = as.Date(last_order),
    recency_days = as.numeric(analysis_date - last_order),
    churn_flag = ifelse(recency_days > 180, 1, 0)
  )

# ------------------------------------------------------------
# Step 3: Customer category dependency
# ------------------------------------------------------------

customer_category_dependency <- orders_clean %>%
  group_by(customer_id, category) %>%
  summarise(
    category_revenue = sum(net_revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(customer_id) %>%
  mutate(
    category_revenue_share = category_revenue / sum(category_revenue, na.rm = TRUE)
  ) %>%
  slice_max(category_revenue_share, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    dominant_category = category,
    dominant_category_share = category_revenue_share
  )

# ------------------------------------------------------------
# Step 4: Category decline
# recent 3 months vs previous 3 months
# ------------------------------------------------------------

category_decline <- orders_clean %>%
  mutate(
    period = case_when(
      transaction_date > analysis_date %m-% months(3) ~ "recent_3m",
      transaction_date > analysis_date %m-% months(6) &
        transaction_date <= analysis_date %m-% months(3) ~ "previous_3m",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(category, period) %>%
  summarise(
    category_revenue = sum(net_revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = category_revenue,
    values_fill = 0
  ) %>%
  mutate(
    category_decline_ratio = recent_3m / pmax(previous_3m, 1),
    category_decline_flag = ifelse(category_decline_ratio < 0.70, 1, 0)
  )

# ------------------------------------------------------------
# Step 5: Dependency failure signal
# ------------------------------------------------------------

dependency_failure_signal <- customer_churn_base %>%
  left_join(customer_category_dependency, by = "customer_id") %>%
  left_join(
    category_decline,
    by = c("dominant_category" = "category")
  ) %>%
  mutate(
    high_category_dependency = ifelse(dominant_category_share >= 0.60, 1, 0),

    possible_dependency_failure = ifelse(
      churn_flag == 1 &
        high_category_dependency == 1 &
        category_decline_flag == 1,
      1, 0
    )
  )

# ------------------------------------------------------------
# Step 6: Quick checks
# ------------------------------------------------------------

dependency_failure_signal %>%
  count(possible_dependency_failure)

dependency_failure_signal %>%
  filter(possible_dependency_failure == 1) %>%
  select(
    customer_id,
    dominant_category,
    dominant_category_share,
    recent_3m,
    previous_3m,
    category_decline_ratio,
    churn_flag,
    possible_dependency_failure
  )

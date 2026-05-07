# ------------------------------------------------------------
# Step 4: Operational Friction Exploration
# Recent return ratio within the churn window
# ------------------------------------------------------------

customer_recent_returns <- orders_clean %>%
  left_join(
    customer_churn_base %>%
      select(customer_id, last_order),
    by = "customer_id"
  ) %>%
  filter(
    transaction_date >= last_order - days(180),
    transaction_date <= last_order
  ) %>%
  group_by(customer_id) %>%
  summarise(
    recent_quantity = sum(quantity, na.rm = TRUE),
    recent_returned_qty = sum(returned_qty, na.rm = TRUE),
    recent_refund_amount = sum(refund_amount, na.rm = TRUE),
    recent_return_ratio = recent_returned_qty / pmax(recent_quantity, 1),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Step 5: Check return ratio distribution
# ------------------------------------------------------------

return_ratio_distribution <- customer_recent_returns %>%
  summarise(
    p50 = quantile(recent_return_ratio, 0.50, na.rm = TRUE),
    p75 = quantile(recent_return_ratio, 0.75, na.rm = TRUE),
    p90 = quantile(recent_return_ratio, 0.90, na.rm = TRUE),
    p95 = quantile(recent_return_ratio, 0.95, na.rm = TRUE),
    p99 = quantile(recent_return_ratio, 0.99, na.rm = TRUE),
    max_return_ratio = max(recent_return_ratio, na.rm = TRUE)
  )

print(return_ratio_distribution)

# ------------------------------------------------------------
# Step 6: Possible Operational Friction Signal
# ------------------------------------------------------------

operational_friction_signal <- customer_churn_base %>%
  left_join(
    customer_recent_returns,
    by = "customer_id"
  ) %>%
  mutate(
    high_return_flag = ifelse(
      recent_return_ratio >= 0.25,
      1, 0
    ),

    possible_operational_friction = ifelse(
      churn_flag == 1 &
        high_return_flag == 1,
      1, 0
    )
  )

# ------------------------------------------------------------
# Step 7: Signal summary
# ------------------------------------------------------------

operational_friction_signal %>%
  count(possible_operational_friction)

# ------------------------------------------------------------
# Step 8: Inspect customers
# ------------------------------------------------------------

operational_friction_signal %>%
  filter(possible_operational_friction == 1) %>%
  select(
    customer_id,
    recent_quantity,
    recent_returned_qty,
    recent_return_ratio,
    churn_flag,
    possible_operational_friction
  ) %>%
  arrange(desc(recent_return_ratio)) %>%
  head(25)

# ------------------------------------------------------------
# Step 9: Compare return behavior between
# churned vs active customers
# Goal:
# Check whether churned customers exhibit
# disproportionately higher return behavior
# before inactivity
# ------------------------------------------------------------

operational_friction_signal %>%
  group_by(churn_flag) %>%
  summarise(
    avg_return_ratio = mean(recent_return_ratio, na.rm = TRUE),
    median_return_ratio = median(recent_return_ratio, na.rm = TRUE),
    customer_count = n(),
    .groups = "drop"
  )

# Return behavior alone is probably not a dominant churn driver in this dataset.
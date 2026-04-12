library(dplyr)
library(BTYD)

# BG/NBD model translates past purchase behavior into
# expected future transactions and customer survival probability.

source("./scripts/02_lrfmvp_base.R")

# Map LRFM features to BG/NBD model inputs:
# x = number of repeat purchases (F - 1)
# t.x = time between first and last purchase (L)
# T.cal = total observation period (time from first purchase to analysis date)

bgnbd_cal <- lrfmp_base %>%
  mutate(
    x = F - 1,
    `t.x` = L,
    `T.cal` = as.numeric(analysis_date - first_order)
  ) %>%
  select(customer_id, x, `t.x`, `T.cal`)

# Convert calibration data to matrix format required by BTYD
bgnbd_matrix <- as.matrix(bgnbd_cal[, c("x", "t.x", "T.cal")])

# Fit BG/NBD model
bgnbd_model <- bgnbd.EstimateParameters(bgnbd_matrix)

# Predict expected number of transactions in the next 30 days
bgnbd_cal$pred_30d <- bgnbd.ConditionalExpectedTransactions(
  params = bgnbd_model,
  T.star = 30,
  x = bgnbd_cal$x,
  t.x = bgnbd_cal$`t.x`,
  T.cal = bgnbd_cal$`T.cal`
)

# Predict active probability in the next 30 days.
# Estimate the probability that each customer is still active
bgnbd_cal$p_alive <- bgnbd.PAlive(
  params = bgnbd_model,
  x = bgnbd_cal$x,
  t.x = bgnbd_cal$`t.x`,
  T.cal = bgnbd_cal$`T.cal`
)

summary(bgnbd_cal$p_alive)

bgnbd_output <- bgnbd_cal %>%
  select(customer_id, x, `t.x`, `T.cal`, pred_30d, p_alive)

dir.create("./reports", showWarnings = FALSE)

write.csv(
  bgnbd_output,
  "./reports/bgnbd_output.csv",
  row.names = FALSE
)

# pred_30d represents the expected number of transactions
# a customer is likely to make in the next 30 days.
# Higher values indicate stronger short-term purchase intent.

summary(bgnbd_cal$p_alive)

# Note: Customers with high past frequency can still have low p_alive,
# indicating that they were active in the past but may have churned recently.
# Combining pred_30d and p_alive allows segmentation of customers into:
# - Active high-value customers
# - Low engagement but still active
# - Likely churned customers
# - Potential comeback cases

bgnbd_cal %>%
  summarise(
    avg_pred = mean(pred_30d),
    avg_alive = mean(p_alive)
  )


# Prepare Gamma-Gamma / spend-model inputs:
# x = repeat purchases
# m.x = average monetary value per transaction

gg_data <- lrfmp_base %>%
  mutate(
    x = F - 1,
    m.x = M / F
  ) %>%
  filter(x > 0) %>%
  select(customer_id, x, m.x)

# Fit spend model (Gamma-Gamma equivalent)
gg_model <- spend.EstimateParameters(
  gg_data$x,
  gg_data$m.x
)

# Estimate expected average transaction value per customer
gg_data$exp_avg_value <- spend.expected.value(
  gg_model,
  gg_data$x,
  gg_data$m.x
)

# Combine BG/NBD and spend-model outputs to estimate 30-day customer value

clv_data <- bgnbd_cal %>%
  select(customer_id, pred_30d, p_alive) %>%
  left_join(
    gg_data %>% select(customer_id, exp_avg_value),
    by = "customer_id"
  )


# CLV = expected future transactions × expected average transaction value

clv_data <- clv_data %>%
  mutate(
    clv_30d = pred_30d * exp_avg_value
  )

# Combining CLV with p_alive enables identification of:
# - high-value active customers
# - high-value customers at risk of churn
# - low-value but active customers
# - churned low-value customers


# Top 10 customers by predicted 30-day CLV

clv_data %>%
  arrange(desc(clv_30d)) %>%
  head(10)

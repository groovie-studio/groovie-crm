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
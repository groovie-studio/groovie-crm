# ============================================================
# 08_churn_risk_engine.R
# ============================================================
#
# Purpose:
# This script creates an explainable churn risk layer by combining:
#
# 1) Behavioral customer rhythm from LRFMPV:
#    - R = days since last purchase
#    - P = expected purchase interval
#    - F = purchase frequency
#
# 2) Probabilistic survival signal from BG/NBD:
#    - p_alive = probability that the customer is still active
#
# The goal is not to define churn as a fixed rule such as
# "no purchase in 90 days".
#
# Instead, churn is defined as:
#
#   "A customer deviating from their expected purchase rhythm,
#    combined with a lower probability of still being active."
#
# This makes the churn model customer-specific, explainable,
# and suitable for CRM decisioning.
#
# ============================================================


library(dplyr)
library(writexl)

# ------------------------------------------------------------
# 1) Load upstream feature/model layers
# ------------------------------------------------------------
#
# 02_lrfmp_base.R creates:
# - lrfmp_base
# - L, R, F, M, P, V customer features
#
# 03_bgnbd_gamma_gamma_clv.R should create:
# - clv_data
# - p_alive
# - pred_30d
# - exp_avg_value
# - clv_30d

source("scripts/02_lrfmvp_base.R")
source("scripts/03_cohort_analysis.R")


# ------------------------------------------------------------
# 2) Prepare churn input
# ------------------------------------------------------------
#
# clv_data already contains customer-level model outputs.
# We join LRFMPV features to add behavioral rhythm variables.

churn_input <- clv_data %>%
  left_join(
    lrfmp_base %>%
      select(customer_id, L, R, P, V),
    by = "customer_id"
  )


# ------------------------------------------------------------
# 3) Calculate churn risk
# ------------------------------------------------------------
#
# recency_ratio:
#   R / P
#
# Interpretation:
# - < 1.0  : customer is still within expected purchase rhythm
# - 1.0+   : customer is starting to delay
# - 2.0+   : customer is significantly outside normal rhythm
#
# p_alive_risk:
#   1 - p_alive
#
# frequency_risk:
#   lower frequency customers receive higher risk contribution
#
# Default weights:
# - 60% p_alive risk
# - 25% recency rhythm risk
# - 15% frequency risk
#
# These are expert heuristic default weights.
# They can later be calibrated using backtesting or logistic regression.

calculate_churn_risk <- function(
  df,
  w_p_alive = 0.60,
  w_recency = 0.25,
  w_frequency = 0.15
) {
  df %>%
    mutate(
      recency_ratio = ifelse(
        is.na(P) | P == 0,
        NA_real_,
        R / P
      ),

      recency_risk = percent_rank(recency_ratio),
      p_alive_risk = 1 - p_alive,
      frequency_risk = 1 - percent_rank(F),

      churn_score =
        w_p_alive * p_alive_risk +
        w_recency * recency_risk +
        w_frequency * frequency_risk,

      churn_segment = case_when(
        churn_score >= 0.75 ~ "Critical",
        churn_score >= 0.55 ~ "High Risk",
        churn_score >= 0.35 ~ "Medium Risk",
        TRUE ~ "Healthy"
      ),

      churn_action = case_when(
        churn_segment == "Critical" ~ "Immediate winback campaign",
        churn_segment == "High Risk" ~ "Targeted retention campaign",
        churn_segment == "Medium Risk" ~ "Engagement / reminder campaign",
        churn_segment == "Healthy" ~ "Maintain / loyalty nurturing",
        TRUE ~ "Review"
      )
    )
}


# ------------------------------------------------------------
# 4) Run churn engine
# ------------------------------------------------------------

churn_data <- calculate_churn_risk(churn_input)


# ------------------------------------------------------------
# 5) Review output
# ------------------------------------------------------------

glimpse(churn_data)

churn_data %>%
  count(churn_segment) %>%
  arrange(desc(n))

summary(churn_data$churn_score)


# ------------------------------------------------------------
# 6) Export churn output
# ------------------------------------------------------------

dir.create("reports", showWarnings = FALSE)

write_xlsx(
  churn_data,
  "reports/churn_risk_output.xlsx"
)


# ------------------------------------------------------------
# Notes:
# ------------------------------------------------------------
#
# This model should be understood as an explainable decision layer,
# not a final machine learning churn classifier.
#
# Future improvements:
#
# 1) Calibrate weights with historical backtesting.
# 2) Add churn labels using future purchase windows.
# 3) Train logistic regression:
#      churn_label ~ p_alive_risk + recency_risk + frequency_risk
# 4) Add context correction:
#      - product availability
#      - lifecycle churn
#      - store/channel problems
#      - category dependency
# 5) Combine churn_score with CLV to create risk-adjusted value.
#
# ============================================================
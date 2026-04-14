library(dplyr)
library(writexl)

# 1) Use only "Approved" orders for LRFMPV analysis.
df_approved <- df %>%
  filter(order_status == "Approved")

# 2) Anlysis date: date after the last transaction date in the dataset.
analysis_date <- max(df_approved$transaction_date, na.rm = TRUE) + 1

# 3) L, R, F, M, P, V table for each customer.

lrfmp_base <- df_approved %>%
  group_by(customer_id) %>%
  summarise(
    first_order = min(transaction_date, na.rm = TRUE),
    last_order  = max(transaction_date, na.rm = TRUE),
    F = n_distinct(transaction_id),
    M = sum(net_revenue, na.rm = TRUE),
    V = n_distinct(category),
    .groups = "drop"
  ) %>%
  mutate(
    L = as.numeric(last_order - first_order),
    R = as.numeric(analysis_date - last_order),
    P = ifelse(F > 1, L / (F - 1), NA_real_)
  )

# 4) Feature set for LRFMPV analysis.
lrfmp_features <- lrfmp_base %>%
  select(customer_id, L, R, F, M, P, V)

# 5) Check the data.
glimpse(lrfmp_base)
head(lrfmp_features)

# 6) Export the LRFMPV base table to an Excel file for further analysis..
dir.create("reports", showWarnings = FALSE)
write_xlsx(lrfmp_base, "reports/lrfmp_base.xlsx")

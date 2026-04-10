df_approved <- df %>%
  filter(order_status == "Approved")

analysis_date <- max(df_approved$transaction_date) + 1

## Frequency, Monetary calculations

lrfmp_base <- df_approved %>%
  group_by(customer_id) %>%
  summarise(
    first_order = min(transaction_date),
    last_order = max(transaction_date),
    F = n(),
    M = sum(list_price),
    V = n_distinct(category_id)
  )

glimpse(lrfmp_base)

## Length, Recency calculations

lrfmp_base <- lrfmp_base %>%
  mutate(
    L = as.numeric(last_order - first_order),
    R = as.numeric(analysis_date - last_order)
  )

lrfmp_base <- lrfmp_base %>%
  mutate(
    P = ifelse(F > 1, L / (F - 1), NA)
  )

lrfmp_features <- lrfmp_base %>%
  select(customer_id, L, R, F, M, P, V)

lrfmp_features
library(writexl)
write_xlsx(lrfmp_base, "./reports/lrfmp_base.xlsx")

dir.create("./reports", showWarnings = FALSE)
output_path <- "./reports/lrfmp_base.xlsx"
write_xlsx(lrfmp_base, output_path)
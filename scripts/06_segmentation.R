library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(openxlsx)

# Load data
df <- read_csv("data/hybrid_fashion_data_w_profit.csv")

# Type conversions
df <- df %>%
  mutate(
    transaction_date = ymd(transaction_date),
    transaction_id   = as.numeric(transaction_id),
    customer_id      = as.numeric(customer_id),
    product_id       = as.numeric(product_id),
    quantity         = as.numeric(quantity),
    unit_cost        = as.numeric(unit_cost),
    total_cost       = as.numeric(total_cost),
    sold_price       = as.numeric(sold_price),
    gross_revenue    = as.numeric(gross_revenue),
    refund_amount    = as.numeric(refund_amount),
    net_revenue      = as.numeric(net_revenue),
    gross_profit     = as.numeric(gross_profit),
    line_revenue     = as.numeric(line_revenue),
    returned_qty     = as.numeric(returned_qty),
    sold_price_level = as.numeric(sold_price_level)
  )

# Optional: simplify categories into broader groups
df <- df %>%
  mutate(
    category_group = case_when(
      category %in% c("Dresses", "Tops", "Skirts", "Outerwear") ~ "fashion",
      category %in% c("Shoes", "Bags", "Accessories") ~ "accessory",
      TRUE ~ "other"
    )
  )

# Analysis date
analysis_date <- max(df$transaction_date, na.rm = TRUE)

# Customer data mart for segmentation
customer_dm <- df %>%
  group_by(customer_id) %>%
  summarise(
    first_order = min(transaction_date, na.rm = TRUE),
    last_order  = max(transaction_date, na.rm = TRUE),

    # Core
    F = n_distinct(transaction_id),
    M = sum(net_revenue, na.rm = TRUE),
    profit = sum(gross_profit, na.rm = TRUE),

    # LRFM features
    R = as.numeric(analysis_date - max(transaction_date, na.rm = TRUE)),
    L = as.numeric(max(transaction_date, na.rm = TRUE) - min(transaction_date, na.rm = TRUE)),
    V = n_distinct(category_group),

    # Behavioural features
    avg_basket = mean(net_revenue, na.rm = TRUE),
    avg_price_level = mean(sold_price_level, na.rm = TRUE),
    category_diversity = n_distinct(category_group),
    brand_diversity = n_distinct(brand),
    weekend_ratio = mean(wday(transaction_date, week_start = 1) >= 6, na.rm = TRUE),
    weekend_shopper = as.numeric(
      mean(wday(transaction_date, week_start = 1) >= 6, na.rm = TRUE) > 0.5
    ),

    return_ratio = ifelse(
      sum(quantity, na.rm = TRUE) > 0,
      sum(returned_qty, na.rm = TRUE) / sum(quantity, na.rm = TRUE),
      0
    ),

    refund_ratio = ifelse(
      sum(gross_revenue, na.rm = TRUE) > 0,
      sum(refund_amount, na.rm = TRUE) / sum(gross_revenue, na.rm = TRUE),
      0
    ),

    profit_margin = ifelse(
      sum(net_revenue, na.rm = TRUE) > 0,
      sum(gross_profit, na.rm = TRUE) / sum(net_revenue, na.rm = TRUE),
      0
    ),

    top_category_ratio = {
      cat_counts <- table(category_group)
      if (length(cat_counts) == 0) NA_real_ else max(cat_counts) / sum(cat_counts)
    },

    bargain_ratio = mean(sold_price_level <= 2, na.rm = TRUE),
    premium_ratio = mean(sold_price_level >= 4, na.rm = TRUE),

    .groups = "drop"
  )

# Top category group per customer
top_category <- df %>%
  count(customer_id, category_group, name = "n") %>%
  group_by(customer_id) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(flag = 1) %>%
  pivot_wider(
    names_from = category_group,
    values_from = flag,
    values_fill = 0,
    names_prefix = "topcat_"
  )

# Join top-category flags into customer data mart
customer_dm <- customer_dm %>%
  left_join(top_category, by = "customer_id")

# K-Means input
k_data <- customer_dm %>%
  select(
    F, M, profit, R, V,
    avg_basket, avg_price_level,
    category_diversity, brand_diversity,
    weekend_ratio, weekend_shopper,
    return_ratio, refund_ratio,
    profit_margin, top_category_ratio,
    bargain_ratio, premium_ratio,
    starts_with("topcat_")
  ) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), 0, .)))

# Remove constant columns
k_data <- k_data[, sapply(k_data, function(x) sd(x, na.rm = TRUE) > 0)]

# Scale
k_scaled <- scale(k_data)

# Remove any columns that still create non-finite values after scaling
k_scaled <- k_scaled[, colSums(!is.finite(k_scaled)) == 0, drop = FALSE]

# Elbow
wss <- sapply(2:8, function(k) {
  kmeans(k_scaled, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

plot(
  2:8, wss, type = "b", pch = 19,
  xlab = "n of clusters (k)",
  ylab = "Total Within-Cluster SS",
  main = "Elbow Method"
)

# 5 clusters chosen based on elbow plot
set.seed(42)
k_model <- kmeans(k_scaled, centers = 5, nstart = 25, iter.max = 100)

# Assign clusters
customer_dm <- customer_dm %>%
  mutate(cluster = k_model$cluster)

# Cluster summary
cluster_summary <- customer_dm %>%
  group_by(cluster) %>%
  summarise(
    customers = n(),
    avg_F = mean(F, na.rm = TRUE),
    avg_M = mean(M, na.rm = TRUE),
    avg_profit = mean(profit, na.rm = TRUE),
    avg_R = mean(R, na.rm = TRUE),
    avg_V = mean(V, na.rm = TRUE),
    avg_basket = mean(avg_basket, na.rm = TRUE),
    avg_price_level = mean(avg_price_level, na.rm = TRUE),
    avg_weekend_ratio = mean(weekend_ratio, na.rm = TRUE),
    avg_weekend_shopper = mean(weekend_shopper, na.rm = TRUE),
    avg_return_ratio = mean(return_ratio, na.rm = TRUE),
    avg_refund_ratio = mean(refund_ratio, na.rm = TRUE),
    avg_profit_margin = mean(profit_margin, na.rm = TRUE),
    avg_top_category_ratio = mean(top_category_ratio, na.rm = TRUE),
    avg_bargain_ratio = mean(bargain_ratio, na.rm = TRUE),
    avg_premium_ratio = mean(premium_ratio, na.rm = TRUE),
    avg_top_category_ratio = mean(top_category_ratio, na.rm = TRUE),
    across(starts_with("topcat_"), mean, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  arrange(desc(avg_M))

# Export
dir.create("reports", showWarnings = FALSE)
write.xlsx(cluster_summary, "reports/cluster_summary.xlsx")
write.xlsx(customer_dm, "reports/customer_segments.xlsx")

print(cluster_summary)
colnames(k_data)

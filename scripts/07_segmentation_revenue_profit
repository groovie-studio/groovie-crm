# =========================================
# 📦 LIBRARIES
# =========================================

library(dplyr)       # data manipulation
library(lubridate)   # date operations
library(tidyr)       # data completion (grid)
library(readr)       # csv reading
library(ggplot2)     # visualization

# =========================================
# 📥 DATA LOADING
# =========================================

# Read transactional dataset
df <- read_csv("./data/hybrid_fashion_data_w_profit.csv")

# Ensure date format is correct
orders <- df %>%
  mutate(transaction_date = as.Date(transaction_date))

# =========================================
# 🧱 CUSTOMER BASE TABLE (CORE CRM LAYER)
# =========================================

# Aggregate transaction-level data to customer-level
customer_base <- orders %>%
  group_by(customer_id) %>%
  summarise(
    # Total monetary metrics
    total_revenue = sum(net_revenue, na.rm = TRUE),
    total_profit = sum(gross_profit, na.rm = TRUE),
    
    # Purchase behavior
    total_orders = n_distinct(transaction_id),
    total_items = sum(quantity, na.rm = TRUE),
    
    # Lifecycle metrics
    first_order = min(transaction_date),
    last_order = max(transaction_date),
    
    # Recency (days since last purchase)
    recency = as.numeric(Sys.Date() - last_order),
    
    # Average order value
    avg_order_value = total_revenue / total_orders,
    
    # Return behavior (important for profitability analysis)
    return_rate = sum(returned_qty, na.rm = TRUE) / sum(quantity, na.rm = TRUE)
  )

# =========================================
# 📊 SEGMENTATION INPUT (QUANTILES)
# =========================================

# Create 5x5 segmentation buckets based on revenue & profit
customer_seg <- customer_base %>%
  mutate(
    revenue_segment = ntile(total_revenue, 5),  # 1 = low, 5 = high
    profit_segment  = ntile(total_profit, 5)    # 1 = low, 5 = high
  )

# =========================================
# 🧠 BUSINESS SEGMENTATION LOGIC
# =========================================

# Map each customer into strategic segments based on matrix position
customer_seg <- customer_seg %>%
  mutate(
    segment = case_when(
      
      # 🟦 PROFIT CORE
      # Low-mid revenue but high profitability → structural backbone
      profit_segment == 4 & revenue_segment == 1 ~ "Profit Core",
      profit_segment == 5 & revenue_segment == 1 ~ "Profit Core",
      profit_segment == 4 & revenue_segment == 2 ~ "Profit Core",
      profit_segment == 5 & revenue_segment == 2 ~ "Profit Core",

      # 🟨 RISING STARS
      # Medium revenue, high profit → growth potential
      profit_segment == 4 & revenue_segment == 3 ~ "Rising Stars",
      profit_segment == 5 & revenue_segment == 3 ~ "Rising Stars",

      # 🟥 MVC (MOST VALUABLE CUSTOMERS)
      # High revenue & high profit → top value segment
      profit_segment == 4 & revenue_segment == 4 ~ "MVC",
      profit_segment == 5 & revenue_segment == 4 ~ "MVC",
      profit_segment == 4 & revenue_segment == 5 ~ "MVC",
      profit_segment == 5 & revenue_segment == 5 ~ "MVC",

      # 🟫 MASS
      # Remaining customers → low or mid value
      TRUE ~ "Mass"
    )
  )

# =========================================
# 📈 SEGMENT SUMMARY (BUSINESS OUTPUT)
# =========================================

# Summarise segment performance
customer_seg %>%
  group_by(segment) %>%
  summarise(
    customers = n(),
    avg_revenue = mean(total_revenue),
    avg_profit = mean(total_profit)
  )

# =========================================
# 🔲 HEATMAP DATA PREPARATION
# =========================================

# Count customers per (revenue, profit) cell
# Ensure full 5x5 grid exists (fill missing with 0)
heatmap_data <- customer_seg %>%
  count(revenue_segment, profit_segment) %>%
  complete(
    revenue_segment = 1:5,
    profit_segment = 1:5,
    fill = list(n = 0)
  )

# =========================================
# 🎨 VISUALIZATION (5x5 MATRIX)
# =========================================

ggplot(heatmap_data, aes(x = revenue_segment, y = profit_segment, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), size = 5) +
  scale_fill_gradient(low = "white", high = "red") +
  
  # Reverse Y axis so top = high profit
  scale_y_continuous(trans = "reverse") +
  
  labs(
    title = "Revenue vs Profit 5x5 Segmentation",
    x = "Revenue Segment (Low → High)",
    y = "Profit Segment (High → Low)"
  ) +
  
  theme_minimal()
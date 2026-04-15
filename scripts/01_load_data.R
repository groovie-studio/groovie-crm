library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)

df <- read_csv("data/hybrid_fashion_data_w_profit.csv")

df <- df %>%
  mutate(
    transaction_date = ymd(transaction_date),
    transaction_id = as.numeric(transaction_id),
    customer_id = as.numeric(customer_id),
    product_id = as.numeric(product_id),
    quantity = as.numeric(quantity),
    unit_cost = as.numeric(unit_cost),
    total_cost = as.numeric(total_cost),
    sold_price = as.numeric(sold_price),
    gross_revenue = as.numeric(gross_revenue),
    refund_amount = as.numeric(refund_amount),
    net_revenue = as.numeric(net_revenue),
    gross_profit = as.numeric(gross_profit),
    line_revenue = as.numeric(line_revenue)
  )

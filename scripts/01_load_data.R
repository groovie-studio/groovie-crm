library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(writexl)

df <- read_csv("./data/hybrid_fashion_data.csv")
df <- df %>%
  mutate(
    transaction_date = dmy(transaction_date),
    product_first_sold_date = mdy(product_first_sold_date),
    standard_cost = as.numeric(str_remove(standard_cost, "\\$"))
  )

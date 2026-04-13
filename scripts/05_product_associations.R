library(arules)
library(dplyr)

basket_data <- df %>%
  filter(order_status == "Approved") %>%   # temiz data
  select(transaction_id, category)

transactions <- as(
  split(basket_data$category, basket_data$transaction_id),
  "transactions"
)

rules <- apriori(
  transactions,
  parameter = list(
    supp = 0.001,
    conf = 0.10,
    minlen = 2
  )
)
# Association rule metrics:
# support   = frequency of the rule in the dataset (how often lhs and rhs occur together)
# confidence = probability of rhs given lhs (strength of the rule)
# lift      = strength of association vs random chance (>1 indicates positive relationship)
# coverage  = frequency of lhs in the dataset (how often the left-hand side appears)

rules_lift <- sort(rules, by = "lift", decreasing = TRUE)
inspect(rules_lift[1:20])

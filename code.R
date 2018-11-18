diamonds_data = read.csv("diamonds.csv", header = TRUE)
diamonds_data <- diamonds_data[2:11]
normalized_price <- diamonds_data$price / max(diamonds_data$price)
fitted_model <- glm(normalized_price ~ diamonds_data$carat + diamonds_data$clarity, family = binomial())
summary(fitted_model)

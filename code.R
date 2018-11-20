diamonds_data = read.csv("diamonds.csv", header = TRUE)
#library(rcompanion)
library('corrplot')

#check outliers
diamonds_data <- diamonds_data[2:11] #data cleaning - removing unnecessary variables
#changing 0 to NAs
diamonds_data$z[diamonds_data$z==0] = NA
diamonds_data$x[diamonds_data$x==0] = NA
diamonds_data$y[diamonds_data$y==0] = NA
any(is.na(diamonds_data)) #check if there are any nulls in our dataset

#checking for typos
summary(diamonds_data$cut)
summary(diamonds_data$color)
summary(diamonds_data$clarity)

#datachecking - histograms
hist(diamonds_data$depth) # guassian
hist(diamonds_data$carat) # exponential
hist(diamonds_data$table) # almost gaussian
hist(diamonds_data$x) # not take into consideration distribution closer to uniform than to others
hist(diamonds_data$y) # 2 outliers hard to say anything about dostribution
hist(diamonds_data$z) # similarly to y
plot(summary(diamonds_data$cut))
plot(summary(diamonds_data$color))
plot(summary(diamonds_data$clarity))

# check correlation between variables
diamonds_data_omited <- na.omit(diamonds_data)
variables_data_frame <- data.frame(diamonds_data_omited$carat, diamonds_data_omited$depth, diamonds_data_omited$table, log(diamonds_data_omited$price), diamonds_data_omited$x, diamonds_data_omited$y, diamonds_data_omited$z)
corelations <- cor(variables_data_frame)
corrplot(corelations, method = 'circle')

standarized_price <- diamonds_data$price / max(diamonds_data$price) #it's obviously not normalization
fitted_model <- glm(standarized_price ~ diamonds_data$carat + diamonds_data$x, family = gaussian())
summary(fitted_model)

#step(model.nnull, scope = list(upper = model.full), direction = 'both', test = 'criteria', data)